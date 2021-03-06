/* $Id: scsi_target.c,v 1.91 2009/03/30 04:16:30 mjacob Exp $ */
/*
 *  Copyright (c) 1997-2009 by Matthew Jacob
 *  All rights reserved.
 * 
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 * 
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 * 
 * 
 *  Alternatively, this software may be distributed under the terms of the
 *  the GNU Public License ("GPL") with platforms where the prevalant license
 *  is the GNU Public License:
 * 
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of The Version 2 GNU General Public License as published
 *   by the Free Software Foundation.
 * 
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *  
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * 
 *  Matthew Jacob
 *  Feral Software
 *  421 Laurel Avenue
 *  Menlo Park, CA 94025
 *  USA
 * 
 *  gplbsd at feral com
 */
/*
 * SCSI Target Mode "toy disk" target device for Linux.
 */

#include <linux/version.h>
#include <linux/autoconf.h>
#include <linux/module.h>
#include <linux/autoconf.h>
#include <linux/types.h>
#include <linux/blkdev.h>
#include <linux/kthread.h>
#include <linux/proc_fs.h>
#include <linux/time.h>
#include <scsi/scsi.h>
#include <scsi/scsi_cmnd.h>

#ifdef  min
#undef  min
#endif
#define min(a,b) (((a)<(b))?(a):(b))
#ifndef roundup
#define roundup(x, y)   ((((x)+((y)-1))/(y))*(y))
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
#define sg_page(_sg) ((_sg)->page)
#define sg_assign_page(_sg, _pg) ((_sg)->page = (_pg))
#endif

#include "isp_tpublic.h"
#include "linux/smp_lock.h"

#define DEFAULT_DEVICE_TYPE 0       /* DISK */
#define MAX_BUS             8
#define MAX_LUN             64
#define N_SENSE_BUFS        256

#define cd_dp       cd_hreserved[0].ptrs[0]
#define cd_nsgelems cd_hreserved[1].longs[0]
#define cd_off      cd_hreserved[2].llongs[0]
#define cd_next     cd_hreserved[3].ptrs[0]

#define CDF_PRIVATE_0       0x8000  /* small (non page) data allocation */
#define CDF_PRIVATE_1       0x4000  /* page allocation attached */
#define CDF_PRIVATE_2       0x2000  /* sent status already */
#define CDF_PRIVATE_3       0x1000  /* sg list from sg element cache */
#ifndef SCSI_GOOD
#define SCSI_GOOD   0x0
#endif
#ifndef SCSI_BUSY
#define SCSI_BUSY   0x8
#endif
#ifndef SCSI_CHECK
#define SCSI_CHECK  0x2
#endif
#ifndef SCSI_QFULL
#define SCSI_QFULL  0x28
#endif

#ifndef SERVICE_ACTION_IN
#define SERVICE_ACTION_IN       0x9e
#endif
#ifndef SAI_READ_CAPACITY_16
#define SAI_READ_CAPACITY_16    0x10
#endif
#ifndef READ_12
#define READ_12                 0xa8
#endif
#ifndef READ_16
#define READ_16                 0x88
#endif
#ifndef WRITE_12
#define WRITE_12                0xaa
#endif
#ifndef WRITE_16
#define WRITE_16                0x8a
#endif
#ifndef REPORT_LUNS
#define REPORT_LUNS             0xa0
#endif

#define MODE_ALL_PAGES          0x3f
#define MODE_VU_PAGE            0x00
#define MODE_RWER               0x01
#define MODE_DISCO_RECO         0x02
#define MODE_FORMAT_DEVICE      0x03
#define MODE_GEOMETRY           0x04
#define MODE_CACHE              0x08
#define MODE_PERIPH             0x09
#define MODE_CONTROL            0x0A

#define MODE_DBD                0x08

#define MODE_PF                 0x08
#define MODE_SP                 0x01

#define MODE_PGCTL_MASK         0xC0
#define MODE_PGCTL_CURRENT      0x00
#define MODE_PGCTL_CHANGEABLE   0x40
#define MODE_PGCTL_DEFAULT      0x80
#define MODE_PGCTL_SAVED        0xC0

#define PSEUDO_SPT  64  /* sectors per track */
#define PSEUDO_HDS  64  /* number of heads */
#define PSEUDO_SPC  (PSEUDO_SPT * PSEUDO_HDS)

/*
 * Size to allocate both a scatterlist + payload for small allocations
 */ 
#define SGS_SIZE            1024
#define SGS0                (roundup(sizeof (struct scatterlist), sizeof (void *)))
#define SGS_PAYLOAD_SIZE    (SGS_SIZE - SGS0)
#define SGS_SGP(x)          ((struct scatterlist *)&((u8 *)(x))[SGS_PAYLOAD_SIZE])
#define COPYIN(uarg, karg, amt)     copy_from_user(karg, uarg, amt)
#define COPYOUT(karg, uarg, amt)    copy_to_user(uarg, karg, amt)

//#define DEBUG_ISP_XMIT

char sg_buf[PAGE_SIZE];
int count = 0;
#define BLK_SIZE  8
unsigned char read10CmdBlk[10] = {40, 0, 0, 0, 0, 8, 0, 0, BLK_SIZE , 0};
struct timeval start,end;
unsigned long timeused, mtime;

static __inline void *  scsi_target_kalloc(size_t, int);
static __inline void    scsi_target_kfree(void *, size_t);
static __inline void *  scsi_target_kzalloc(size_t, int);

static __inline void *
scsi_target_kalloc(size_t size, int flags)
{
    void *ptr;
    if (size > PAGE_SIZE) {
        ptr = vmalloc(size);
    } else {
        ptr = kmalloc(size, flags);
    }
    return (ptr);
}

static __inline void
scsi_target_kfree(void *ptr, size_t size)
{
    if (size > PAGE_SIZE) {
        vfree(ptr);
    } else {
        kfree(ptr);
    }
}

static __inline void *
scsi_target_kzalloc(size_t size, int flags)
{
    void *ptr = scsi_target_kalloc(size, flags);
    if (ptr != NULL){
        memset(ptr, 0, size);
    }
    return (ptr);
}

static __inline void init_sg_elem(struct scatterlist *, struct page *, int, void *, size_t);

static __inline void
init_sg_elem(struct scatterlist *sgp, struct page *p, int offset, void *addr, size_t length)
{
    sgp->length = length;
    if (p) {
        sg_assign_page(sgp, p);
        sgp->offset = offset;
    } else {
        sg_assign_page(sgp, virt_to_page(addr));
        sgp->offset = offset_in_page(addr);
    }
}

#include "scsi_target.h"


#ifndef SERNO
#define SERNO   "000000"
#endif

typedef struct bus bus_t;
typedef struct initiator ini_t;
typedef struct sdata sdata_t;

struct sdata {
    sdata_t *next;
    uint8_t sdata[TMD_SENSELEN];
};


struct initiator {
    ini_t *            ini_next;
    bus_t *            ini_bus;        /* backpointer to containing bus */
    sdata_t *          ini_sdata;      /* pending sense data list */
    sdata_t *          ini_sdata_tail; /* pending sense data list, tail */
    uint64_t           ini_iid;        /* initiator identifier */
};

#define    HASH_WIDTH    16
#define    INI_HASH_LISTP(busp, chan, ini_id)    busp->bchan[chan].list[ini_id & (HASH_WIDTH - 1)]

/*
 * We maintain a reasonable cache of large sized (8MB) scatterlists
 */
#define SGELEM_CACHE_SIZE   2048
#define SGELEM_CACHE_COUNT  128
//static struct scatterlist *sg_cache = NULL;


/*
 * A memory disk is constructed of a two dimensional array of pointers to pages. 
 *
 * Allocate a series of chunks of memory, each of which becomes a flat array
 * of pointers to pages that we allocate one at a time.
 */
#define PGLIST_SIZE         (32 << 10)                              /* how big each list is, in bytes */
#define PG_PER_LIST         (PGLIST_SIZE / sizeof (struct page *))  /* how many page pointers fist into that list */
#define PGLIST_MAPPING_SIZE (PG_PER_LIST << PAGE_SHIFT)             /* how many bytes each list covers */
#define START_LIST_IDX(x)   ((x) / PGLIST_MAPPING_SIZE)
#define START_PAGE_IDX(x)   (((x) % PGLIST_MAPPING_SIZE) >> PAGE_SHIFT)

typedef struct {
    struct page ***     pagelists;
    int                 npglists;
    int                             :   30,
                        wce         :   1,
                        enabled     :   1;
    uint64_t            nbytes;
} lun_t;
#define LUN_BLOCK_SHIFT 9

struct bus_chan {
    ini_t *         list[HASH_WIDTH];   /* hash list of known initiators */
    u_int8_t        luns[2][MAX_LUN];      /* per-channel lun arrays */
};

struct bus {
    hba_register_t  h;                      /* must be first */
    struct bus_chan *bchan;
};

// Added by netbear
typedef struct Isp_Device{
    struct Isp_Device *next;
    Scsi_Target_Device *dev;
    stml_device * stml_devlist;
    int	devnum;
	//tmd_list_t 	*isp_tmd;	
} ISP_Device_t;

#define    SDprintk     if (scsi_tdebug) printk
#define    SDprintk2    if (scsi_tdebug > 1) printk
#define    SDprintk3    if (scsi_tdebug > 2) printk


static int scsi_tdebug = 0;

static int
scsi_target_ioctl(struct inode *, struct file *, unsigned int, unsigned long);
static void scsi_target_handler(qact_e, void *);

static __inline bus_t *bus_from_tmd(tmd_cmd_t *);
static __inline bus_t *bus_from_name(char *);
static __inline ini_t *ini_from_tmd(bus_t *, tmd_cmd_t *);

static void add_sdata(ini_t *, void *);
static void rem_sdata(ini_t *);
static void free_sdata_chain(sdata_t *);
static void scsi_target_start_cmd(tmd_cmd_t *, int);
static int scsi_target_thread(void *);
//static int scsi_alloc_disk(bus_t *, int, int, uint64_t);
//static void scsi_free_disk(bus_t *, int, int);
static int scsi_target_endis(char *, uint64_t, int, int, int);

/*
 * Local Declarations
 */
#define INQ_SIZE    36
/*
static uint8_t inqdata[INQ_SIZE] = {
    DEFAULT_DEVICE_TYPE, 0x0, 0x2, 0x2, 32, 0, 0, 0x32,
    'L', 'I', 'N', 'U', 'X', ' ', ' ', ' ',
    'S', 'C', 'S', 'I', ' ', 'M', 'E', 'M',
    'O', 'R', 'Y', ' ', 'D', 'I', 'S', 'K',
    '0', '0', '0', '1'
};
static uint8_t vp0data[7] = {
    DEFAULT_DEVICE_TYPE, 0, 0, 0x3, 0, 0x80, 0x83 
};
static uint8_t vp80data[36] = {
    DEFAULT_DEVICE_TYPE, 0x80, 0, 0x20,
};*/
/* Binary, Associated with Target Port, FC-FS Identifier */
/*static uint8_t vp83data[18] = {
    DEFAULT_DEVICE_TYPE, 0x83, 0, 0xc, 0x01, 0x13, 0, 0x8
};
static uint8_t enomem[TMD_SENSELEN] = {
    0xf0, 0, 0x4, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0x55, 0x03
};
static uint8_t illfld[TMD_SENSELEN] = {
    0xf0, 0, 0x5, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0x24
};*/
/*
static uint8_t invfld[TMD_SENSELEN] = {
    0xf0, 0, 0x5, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0x26
};*/
#if 0
static uint8_t notrdy[TMD_SENSELEN] = {
    0xf0, 0, 0x2, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0x04
};
static uint8_t mediaerr[TMD_SENSELEN] = {
    0xf0, 0, 0x3
};
#endif
/*
static uint8_t ifailure[TMD_SENSELEN] = {
    0xf0, 0, 0x4, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0x44
};*/
static uint8_t ua[TMD_SENSELEN] = {
    0xf0, 0, 0x6, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0x29, 0x1
};
static uint8_t invchg[TMD_SENSELEN] = {
    0xf0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x3f, 0x0e
};

static bus_t busses[MAX_BUS];
static sdata_t *sdp = NULL;
// added by netbear : ISP dev_list
ISP_Device_t * dev_list = NULL;
Lun_Inf_t lun_list[MAX_LUN];
lun_user_t lun_user_list[MAX_USER_PERLUN*MAX_LUN];

static struct semaphore scsi_thread_sleep_semaphore;
static struct semaphore scsi_thread_entry_exit_semaphore;
static struct semaphore scsi_thread_tmd_semaphore;
static tmd_cmd_t *p_front = NULL, *p_last = NULL;
static spinlock_t scsi_target_lock = SPIN_LOCK_UNLOCKED;
static int scsi_target_thread_exit = 0;

static struct file_operations scsi_target_fops = {
    .ioctl  =   scsi_target_ioctl,
    .owner  =   THIS_MODULE,
};

static __inline int
validate_bus_pointer(bus_t *bp, void *identity)
{
    if (bp >= busses && bp < &busses[MAX_BUS]) {
        if (bp->h.r_action) {
            if (bp->h.r_identity == identity) {
                return (1);
            }
        }
    }
    return (0);
}

static __inline bus_t *
bus_from_tmd(tmd_cmd_t *tmd)
{
    bus_t *bp;
    for (bp = busses; bp < &busses[MAX_BUS]; bp++) {
        if (validate_bus_pointer(bp, tmd->cd_hba)) {
            return (bp);
        }
    }
    return (NULL);
}

static __inline bus_t *
bus_from_name(char *name)
{
    bus_t *bp;
    for (bp = busses; bp < &busses[MAX_BUS]; bp++) {
        char localbuf[32];
        if (bp->h.r_action == NULL) {
            continue;
        }
        snprintf(localbuf, sizeof (localbuf), "%s%d", bp->h.r_name, bp->h.r_inst);
        if (strncmp(name, localbuf, sizeof (localbuf) - 1) == 0) {
            return (bp);
        }
    }
    return (NULL);
}

static __inline ini_t *
ini_from_tmd(bus_t *bp, tmd_cmd_t *tmd)
{
   ini_t *ptr = INI_HASH_LISTP(bp, tmd->cd_channel, tmd->cd_iid);
   if (ptr) {
        do {
            if (ptr->ini_iid == tmd->cd_iid) {
                return (ptr);
            }
        } while ((ptr = ptr->ini_next) != NULL);
   }
   return (ptr);
}

static __inline bus_t *
bus_from_notify(isp_notify_t *np)
{
    bus_t *bp;
    for (bp = busses; bp < &busses[MAX_BUS]; bp++) {
        if (bp->h.r_action == NULL) {
            continue;
        }
        if (bp->h.r_identity == np->nt_hba) {
            return (bp);
        }
    }
    return (NULL);
}

static int
scsi_target_ioctl(struct inode *ip, struct file *fp, unsigned int cmd, unsigned long arg)
{
    int rv = 0;

    switch(cmd) {
    case SC_ENABLE_LUN:
    case SC_DISABLE_LUN:
    {
        sc_enable_t local, *sc = &local;
        if (COPYIN((void *)arg, (void *)sc, sizeof (*sc))) {
            rv = -EFAULT;
            break;
        }
        rv = scsi_target_endis(sc->hba_name_unit, sc->nbytes, sc->channel, sc->lun, (cmd == SC_ENABLE_LUN)? 1 : 0);
        break;
    }
    case SC_DEBUG:
    {
        int odebug = scsi_tdebug;
        if (COPYIN((void *)arg, (void *)&scsi_tdebug, sizeof (int))) {
            rv = EFAULT;
            break;
        }
        if (COPYOUT((void *)&odebug, (void *)arg, sizeof (int))) {
            rv = EFAULT;
            break;
        }
        break;
    }
    default:
        rv = -EINVAL;
        break;
    }
    return (rv);
}

/*
 * Make an initiator structure
 */
static void
add_ini(bus_t *bp, int chan, uint64_t iid, ini_t *nptr)
{
   ini_t **ptrlptr = &INI_HASH_LISTP(bp, chan, iid);

   nptr->ini_iid = iid;
   nptr->ini_bus = (struct bus *) bp;
   nptr->ini_next = *ptrlptr;

   *ptrlptr = nptr;
}

/*
 * Add this sense data from the list of
 * sense data structures for this initiator.
 * We always add to the tail of the list.
 */
static void
add_sdata(ini_t *ini, void *sd)
{
    unsigned long flags;
    sdata_t *t;

    spin_lock_irqsave(&scsi_target_lock, flags);
    t = sdp;
    if (t == NULL) {
        spin_unlock_irqrestore(&scsi_target_lock, flags);
        printk(KERN_WARNING "outta sense data structures\n");
        t = scsi_target_kalloc(sizeof (sdata_t), GFP_KERNEL|GFP_ATOMIC);
        if (t == NULL) {
            panic("REALLY outta sense data structures\n");
        }
        spin_lock_irqsave(&scsi_target_lock, flags);
    } else {
        sdp = t->next;
    }
    t->next = NULL;
    memcpy(t->sdata, sd, sizeof (t->sdata));
    if (ini->ini_sdata == NULL) {
        ini->ini_sdata = t;
    } else {
        ini->ini_sdata_tail->next = t;
    }
    ini->ini_sdata_tail = t;
    spin_unlock_irqrestore(&scsi_target_lock, flags);
}

/*
 * Remove one sense data item from the list of
 * sense data structures for this initiator.
 */
static void
rem_sdata(ini_t *ini)
{
    sdata_t *t = ini->ini_sdata;
    if (t) {
        unsigned long flags;
        spin_lock_irqsave(&scsi_target_lock, flags);
        if ((ini->ini_sdata = t->next) == NULL) {
            ini->ini_sdata_tail = NULL;
        }
        t->next = sdp;
        sdp = t;
        spin_unlock_irqrestore(&scsi_target_lock, flags);
    }
}

static void
free_sdata_chain(sdata_t *sdp)
{
    while (sdp) {
        sdata_t *nxt = sdp->next;
        scsi_target_kfree(sdp, sizeof (*sdp)); 
        sdp = nxt;
    }
}


static __inline void scsi_cmd_sched_restart_locked(tmd_cmd_t *, int, const char *);
static __inline void scsi_cmd_sched_restart(tmd_cmd_t *, const char *);

static __inline void
scsi_cmd_sched_restart_locked(tmd_cmd_t *tmd, int donotify, const char *msg)
{
    SDprintk("scsi_cmd_sched_restart[%llx]: %s\n", tmd->cd_tagval, msg);
    tmd->cd_next = NULL;
    if (p_front) {
        p_last->cd_next = tmd;
    } else {
        p_front = tmd;
    }
    p_last = tmd;
    if (donotify) {
        up(&scsi_thread_sleep_semaphore);
    }
}


static __inline void
scsi_cmd_sched_restart(tmd_cmd_t *tmd, const char *msg)
{
    unsigned long flags;
    spin_lock_irqsave(&scsi_target_lock, flags);
    scsi_cmd_sched_restart_locked(tmd, 1, msg);
    spin_unlock_irqrestore(&scsi_target_lock, flags);
}

static void
scsi_target_start_cmd(tmd_cmd_t *tmd, int from_intr)
{
    unsigned long flags;
    unsigned long flatlun;

    /*
     * First, find the bus.
     */

    /*
     * Next check if we're coming in at interrupt level. If so, insert
     * the command and return.
     */
    if (from_intr) {
        scsi_cmd_sched_restart_locked(tmd, 1, "from_intr && p_front");
        return;
    }

    /*
     * Send nolun sense data if tmd->cd_lun is invalid
     */
    flatlun = L0LUN_TO_FLATLUN(tmd->cd_lun);
    if (flatlun >= MAX_LUN || lun_list[flatlun].state == 0 || lun_list[flatlun].target == -1 || lun_list[flatlun].lun == -1) {
        printk("scsi_target_start_cmd : error lun %ld> MAX_LUN\n", flatlun);
    }


    down(&scsi_thread_tmd_semaphore);

    //SDprintk("Scsi start cmd : processing : tag %lld \n",tmd->cd_tagval);
    printk("scsi verify : id %x, lun %x \n", lun_list[flatlun].target, lun_list[flatlun].lun);
    tmd->cd_sc = rx_cmnd(dev_list->dev, lun_list[flatlun].target, lun_list[flatlun].lun, tmd->cd_cdb, 10);

    if (tmd->cd_sc == NULL) {
        printk("scsi_target_start_cmd : error rx_cmnd return NULL\n");
        tmd->cd_scsi_status = SCSI_BUSY;
        up(&scsi_thread_tmd_semaphore);
        return;
    }

    spin_lock_irqsave(&scsi_target_lock, flags);
    tmd->cd_sc->tmd_handle = (void *)tmd;
    spin_unlock_irqrestore(&scsi_target_lock, flags);
    up(&scsi_thread_tmd_semaphore);
    return;

    /*
     * Commands get lumped into 5 rough groups:
     *
     *   + Commands which don't ever really return CHECK CONDITIONS and
     *     always work. These are typically INQUIRY.
     *
     *   + Commands that we accept, but also report CHECK CONDITIONS against if
     *     we have pending contingent allegiance (e..g, TEST UNIT READY).
     *
     *   + Commands that retrieve Sense Data (REQUEST SENSE)
     *
     *   + Commmands that do something (like READ or WRITE)
     *
     *   + All others (which we bounce with either ILLEGAL COMMAND or BAD LUN).
     */


}

static int
scsi_target_ldfree(bus_t *bp, tmd_xact_t *xact, int from_intr)
{
    //unsigned long flags;
    tmd_cmd_t *tmd = xact->td_cmd;

    xact->td_data = NULL;
    tmd->cd_sc = NULL;
    tmd->cd_flags &= ~(CDF_PRIVATE_0 | CDF_PRIVATE_1 | CDF_PRIVATE_2 | CDF_PRIVATE_3);
    return (1);
/*resched:
    tmd->cd_next = NULL;
    spin_lock_irqsave(&scsi_target_lock, flags);
    if (q_front) {
        q_last->cd_next = tmd;
    } else {
        q_front = tmd;
    }
    q_last = tmd;
    up(&scsi_thread_sleep_semaphore);
    spin_unlock_irqrestore(&scsi_target_lock, flags);
    return (0);*/
}


static void
scsi_target_handler(qact_e action, void *arg)
{
}

static int
scsi_target_thread(void *arg)
{
    //unsigned long flags;
    int r,i, j, k, c;
    tmd_cmd_t command;

    siginitsetinv(&current->blocked, 0);
    lock_kernel();
    daemonize("scsi_target_thread");
    unlock_kernel();
    up(&scsi_thread_entry_exit_semaphore);
    printk("scsi_target_thread starting\n");

    memset(command.cd_cdb, 0, TMD_CDBLEN);
    memcpy(command.cd_cdb, read10CmdBlk, 10);
    FLATLUN_TO_L0LUN(command.cd_lun, 1);

    do_gettimeofday(&start);
    c = 0;
    while (scsi_target_thread_exit == 0) {

        
        for (i = 0; i < 8; i ++)
            for (j = 0; j < 8; j ++)
                for (k = 0; k <16; k += BLK_SIZE) {
                    command.cd_cdb[2] = i;
                    command.cd_cdb[3] = j;
                    command.cd_cdb[4] = k;
                    c ++;
                    scsi_target_start_cmd(&command,0);
                    if (c % 256 == 0) {
                        c = 0;
                        r = down_interruptible(&scsi_thread_sleep_semaphore);
                    }
                }
        break;
    }
    do_gettimeofday(&end);
    timeused = 1000000 * (end.tv_sec - start.tv_sec) + end.tv_usec - start.tv_usec;
    mtime = timeused % 1000000;
    timeused /= 1000000;
    printk("Used Time:%lu.%lu \n",timeused,mtime);
    printk("scsi_target_thread exiting\n");
    up(&scsi_thread_entry_exit_semaphore);
    return (0);
}



static int
scsi_target_endis(char *hba_name_unit, uint64_t nbytes, int chan, int lun, int en)
{
    struct semaphore rsem;
    unsigned long flags;
    enadis_t ec;
    info_t info;
    //lun_t *lp;
    bus_t *bp;
    int i;

    /*
     * XXX: yes, there is a race condition here where the bus can
     * XXX: go away. But in order to solve it, we have to make the
     * XXX: bus structure stay around while we call into the HBA
     * XXX: anyway, so fooey,.
     */
    bp = bus_from_name(hba_name_unit);
    if (bp == NULL) {
        SDprintk("%s: cannot find bus for %s\n", __func__, hba_name_unit);
        return (-ENXIO);
    }

    if (chan < 0 || chan >= bp->h.r_nchannels) {
        SDprintk("%s: bad chan (%d)\n", __func__, chan);
        return (-EINVAL);
    }
    if (lun < 0 || lun >= MAX_LUN) {
        SDprintk("%s: bad lun (%d)\n", __func__, lun);
        return (-EINVAL);
    }
    /*lp = &bp->bchan[chan].luns[lun];

    if (en) {
        if (lp->enabled) {
            printk("%s: lun %d already enabled\n", __func__, lun);
            return (-EBUSY);
        }
        rv = scsi_alloc_disk(bp, chan, lun, nbytes);
        if (rv) {
            return (rv);
        }
    } else {
        lp->enabled = 0;
    }*/

    memset(&info, 0, sizeof (info));
    info.i_identity = bp->h.r_identity;
    info.i_channel = chan;
    (*bp->h.r_action)(QIN_GETINFO, &info);
    if (info.i_error) {
        return (info.i_error);
    }
    memset(&ec, 0, sizeof (ec));
    ec.en_hba = bp->h.r_identity;
    if (bp->h.r_type == R_FC) {
        ec.en_lun = LUN_ANY;
    } else {
        ec.en_lun = lun;
    }
    ec.en_chan = chan;
    ec.en_private = &rsem;
    sema_init(&rsem, 0);
    (*bp->h.r_action)(en? QIN_ENABLE : QIN_DISABLE, &ec);
    down(&rsem);

    if (ec.en_error) {
        SDprintk("%s: HBA returned %d for %s action\n", __func__, ec.en_error, en? "enable" : "disable");
        // scsi_free_disk(bp, chan, lun);
        return (ec.en_error);
    }

    spin_lock_irqsave(&scsi_target_lock, flags);
    for (i = 0; i < HASH_WIDTH; i++) {
        ini_t *ini = bp->bchan[chan].list[i];
        while (ini) {
            spin_unlock_irqrestore(&scsi_target_lock, flags);
            add_sdata(ini, invchg);
            rem_sdata(ini);
            spin_lock_irqsave(&scsi_target_lock, flags);
            ini = ini->ini_next;
        }
    }
    spin_unlock_irqrestore(&scsi_target_lock, flags);
    
    /*if (en == 0) {
        scsi_free_disk(bp, chan, lun);
    } else {
        lp->wce = 1;
        lp->enabled = 1;
    }*/
    return (0);
}

EXPORT_SYMBOL(scsi_target_handler);
module_param(scsi_tdebug, int, 0);
#ifdef    MODULE_LICENSE
MODULE_LICENSE("Dual BSD/GPL");
#endif

Scsi_Target_Template isp_template = (Scsi_Target_Template)ISP_TARGET;

int init_module(void)
{
    int i;
    struct proc_dir_entry *e;

    e = create_proc_entry(SCSI_TARGET, S_IFREG|S_IRUGO|S_IWUSR, 0);
    if (e == NULL){
        printk(KERN_ERR "cannot make %s\n", SCSI_TARGET);
        return (-EIO);
    }
    e->proc_fops = &scsi_target_fops;
    
    if (register_target_template (&isp_template) < 0)
		return (-1);

    spin_lock_init(&scsi_target_lock);
    sema_init(&scsi_thread_sleep_semaphore, 0);
    sema_init(&scsi_thread_entry_exit_semaphore, 0);
    sema_init(&scsi_thread_tmd_semaphore, 1);
    kernel_thread(scsi_target_thread, NULL, 0);
    down(&scsi_thread_entry_exit_semaphore);
    for (i = 0; i < N_SENSE_BUFS; i++) {
        sdata_t *t = scsi_target_kalloc(sizeof (sdata_t), GFP_KERNEL);
        if (t) {
            t->next = sdp;
            sdp = t;
        } else {
            break;
        }
    }
    printk(KERN_INFO "Allocated %d sense buffers\n", i);
    /*for (i = 0; i < SGELEM_CACHE_COUNT; i++) {
        struct scatterlist *sg = scsi_target_kzalloc(SGELEM_CACHE_SIZE * sizeof (struct scatterlist), GFP_KERNEL);
        if (sg == NULL) {
            break;
        }
        sg_assign_page(sg, (struct page *) sg_cache);
        sg_cache = sg;
    }
    printk(KERN_INFO "Allocated %d cached sg elements\n", i);*/
    return (0);
}

/*
 * We can't get here until all hbas have deregistered
 */
void cleanup_module(void)
{
    scsi_target_thread_exit = 1;
    up(&scsi_thread_sleep_semaphore);
    down(&scsi_thread_entry_exit_semaphore);
    deregister_target_template(&isp_template);
    free_sdata_chain(sdp);
    /*while (sg_cache) {
        struct scatterlist *sg = (struct scatterlist *) sg_page(sg_cache);
        scsi_target_kfree(sg_cache, SGELEM_CACHE_SIZE * sizeof (struct scatterlist));
        sg_cache = sg;
    }*/
    remove_proc_entry(SCSI_TARGET, 0);
    printk("scsi_target exit\n");
}

int
isp_detect(Scsi_Target_Template* tmpt)
{
    ISP_Device_t *tmp;
    int i;
    stml_device * stmld;
    int lunnum;

    tmp = scsi_target_kzalloc(sizeof(ISP_Device_t), GFP_KERNEL);
    if(!tmp) {
        printk("kmalloc isp_device_t error\n");
        return -1;
    }

    tmp->dev = register_target_front_end(tmpt);	
    
    if (tmp->dev == NULL){
        printk("scsi_target: unable to register with mid scsi target driver\n");
        return -1;
    }

    for(i = 0; i<MAX_LUN; i++)
    {
        lun_list[i].state = 0;
        lun_list[i].mapstate = 0;
        lun_list[i].target = -1;
        lun_list[i].lun = -1;
        lun_list[i].user = NULL;
    }
    
    tmp->stml_devlist = get_devlist(0);
    if (tmp->stml_devlist == NULL){
        printk("scsi_target: no scsi device!\n");	
    }
    
    tmp->dev->dev_specific = (void *)tmp;
    tmp->next = dev_list;
    dev_list = tmp;

    lunnum = 0;
    stmld = tmp->stml_devlist;
    while(stmld != NULL)
    {
        lun_list[lunnum].target = stmld->stml_id;
        lun_list[lunnum].lun = stmld->lun;
        lun_list[lunnum].mapstate = 1;
        lun_list[lunnum].state = 1;
        printk("isp_detect: device %d name %s target %d lun %d.\n", lunnum, stmld->target_device_name, stmld->stml_id, stmld->lun);
        stmld = stmld->next;
        lunnum ++;
    }
    printk("isp_detect: total device %d\n",lunnum);
    tmp->devnum = lunnum;
    for(i = 0; i<lunnum; i++)
    {
        printk("in lun_list: lunlist[%d]: target %d lun %d state %d mapstate %d\n",i,
                lun_list[i].target, lun_list[i].lun, lun_list[i].state, lun_list[i].mapstate);
    }
    

#if 0    	
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,0) && defined(CONFIG_PROC_FS)
    		st = create_proc_info_entry("scsi/scsi_target_minor", 0, 0, stp);
#endif
#endif
    return 1;			//return value>=0.   find 1 device, sure we can
}

int
isp_release(Scsi_Target_Device* dev)
{
    int i;
    ISP_Device_t *tmp=NULL, *prev=NULL;

    for(tmp=dev_list; tmp!=NULL; tmp=tmp->next){
        if(tmp->dev == dev)
            break;
        prev = tmp;
    }

    if (tmp==NULL){
        printk("dev not found\n");
        return -1;
    }

    if(!prev)
        dev_list = tmp->next;
    else
        prev->next = tmp->next;		//remove from devlist

    //as a fact of fact, we need't devlist
    //and we can use dev->devspecfic instead of search the devlist
    	

    for (i = 0; i < MAX_BUS; i++) {
        if (busses[i].h.r_action) {
            int j;
            tmd_cmd_t tmd;
            tmd.cd_hba = busses[i].h.r_identity;
            (*busses[i].h.r_action)(QIN_HBA_UNREG, &tmd);
            busses[i].h.r_action = NULL;
            for (j = 0; j < HASH_WIDTH; j++) {
                ini_t *nptr = busses[i].bchan->list[j];
                while (nptr) {
                    ini_t *next = nptr->ini_next;
                    kfree(nptr);
                    nptr = next;
                }
            }
        }
    }
    
#if 0
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,0) && defined(CONFIG_PROC_FS)
    if (st) {
	remove_proc_entry("scsi/scsi_target", 0);
    }
#endif
#endif

    kfree (tmp);
    tmp = NULL;
    return 0;			//0 is succesful
}

int
isp_xmit_response(Target_Scsi_Cmnd* cmnd)
{
    tmd_cmd_t *tmd;
    //unsigned long flags;
    ISP_Device_t *tmp;
    //struct scatterlist * sglist;

    //spin_lock_irqsave(&scsi_target_lock, flags);
    tmp = (ISP_Device_t *)cmnd->device->dev_specific;
    if(tmp == NULL)
    {
        printk("isp_xmit_response:cmnd->device->dev_specific is NULL, error!\n");
        //spin_unlock_irqrestore(&scsi_target_lock, flags);
        return -1;
    }
    
    down(&scsi_thread_tmd_semaphore);
    tmd = (tmd_cmd_t *)cmnd->tmd_handle;
    up(&scsi_thread_tmd_semaphore);
    if(tmd == NULL)
    {
        printk("isp_xmit_response:can not find the match tmd,error!\n");
        //spin_unlock_irqrestore(&scsi_target_lock, flags);
        return -1;
    }
    
	/*
	 *	we come here to do
	 *	1. send data if we are READ and success
	 *	2. send status
	 *	
	 *	but we came here only once,so we must make them together.
	 */
    
    
    if (cmnd->data_direction == DMA_TO_DEVICE){
#ifdef DEBUG_ISP_XMIT
        printk("data to device done!\n");
#endif
	}
//	printk("xmit_response: cmnd %p cmnd->handle %d cd_totlen %d cd_xfrlen %d cd_scsi_status %d\n", cmnd, cmnd->cmnd_handle, tmd->cd_totlen, tmd->cd_xfrlen, tmd->cd_scsi_status);
    
    if (cmnd->result == SCSI_GOOD)
    {
#ifdef DEBUG_ISP_XMIT
        printk("isp_xmit_response:here a tmd executed ok id %d.\n",cmnd->id);
#endif
    }
    else {
#ifdef DEBUG_ISP_XMIT
        printk("xmit_response: command failed , err %d  sg %p  target %d lun %d\n get sense data : %s\n", cmnd->result, cmnd->sglist, (int)cmnd->target_id,(int) cmnd->lun, cmnd->sense);
#endif
    }
    if (scsi_target_done(cmnd) != 0) {
            printk("scsi_target_handler : Mid_target scsi_target_done return error \n");
        }

    count ++;
    if (count % (8192 * 8 / BLK_SIZE) == 0)
        printk("32M data has been transfered!\n");
    if (count % 256 == 128) {
        up(&scsi_thread_sleep_semaphore);
    }

    if (count % (256 * 256 * 8 / BLK_SIZE) == 0) {
    do_gettimeofday(&end);
    timeused = 1000000 * (end.tv_sec - start.tv_sec) + end.tv_usec - start.tv_usec;
    mtime = timeused % 1000000;
    timeused /= 1000000;
    printk("cmd %d Used Time:%lu.%lu \n",cmnd->id,timeused,mtime);
    }

    //spin_unlock_irqrestore(&scsi_target_lock, flags);
    return 0;
}

int
isp_rdy_to_xfer(Target_Scsi_Cmnd* cmnd)
{
	tmd_cmd_t * tmd;
    tmd_xact_t * xact;
	ini_t *ini;
	bus_t *bp;
	unsigned long flags;
	ISP_Device_t *tmp;

    printk("in isp_rdy_to_xfer\n");
		
	spin_lock_irqsave(&scsi_target_lock, flags);

	down(&scsi_thread_tmd_semaphore);
    tmp = (ISP_Device_t *)cmnd->device->dev_specific;
    if(tmp == NULL)
    {
        printk("isp_rdy_to_xfer:cmnd->device->dev_specific is NULL, error!\n");
        spin_unlock_irqrestore(&scsi_target_lock, flags);
        up(&scsi_thread_tmd_semaphore);
        return -1;
    }
    
    tmd = (tmd_cmd_t *)cmnd->tmd_handle;
    xact = &tmd->cd_xact;
    if(tmd == NULL)
    {
        printk("rdy_to_xfer:can not find the match tmd,error!\n");
        spin_unlock_irqrestore(&scsi_target_lock, flags);
        up(&scsi_thread_tmd_semaphore);
        return -1;
    }
    
    bp = bus_from_tmd(tmd);
    if (bp == NULL) {
        printk("\nisp_rdy_to_xfer:Lost the Bus?\n");
    }
    
    ini = ini_from_tmd(bp, tmd);
    if (ini == NULL) {
        ini_t *nptr;

        spin_unlock_irqrestore(&scsi_target_lock, flags);
        nptr = scsi_target_kzalloc(sizeof (ini_t), GFP_KERNEL|GFP_ATOMIC);
        spin_lock_irqsave(&scsi_target_lock, flags);

        /*
         * Check again to see if it showed while we were allocating...
         */
        ini = ini_from_tmd(bp, tmd);
        if (ini) {
            spin_unlock_irqrestore(&scsi_target_lock, flags);
            if (nptr) {
                scsi_target_kfree(nptr, sizeof (ini_t));
            }
        } else {
            if (nptr == NULL) {
                up(&scsi_thread_tmd_semaphore);
                spin_unlock_irqrestore(&scsi_target_lock, flags);
                tmd->cd_scsi_status = SCSI_BUSY;
                xact->td_hflags |= TDFH_STSVALID;
                xact->td_hflags &= ~TDFH_DATA_MASK;
                xact->td_xfrlen = 0;
                (*bp->h.r_action)(QIN_TMD_CONT, xact);
                return -1;
            }
            add_ini(bp, tmd->cd_channel, tmd->cd_iid, nptr);
            spin_unlock_irqrestore(&scsi_target_lock, flags);
            ini = nptr;
            /*
             * Start off with a Unit Attention condition.
             */
            add_sdata(ini, ua);
        }
    } else {
        spin_unlock_irqrestore(&scsi_target_lock, flags);
    }
        
    /*
     * we come here only to send rdy_xfr	
     */
    xact->td_data = cmnd->sglist;	
    tmd->cd_totlen = cmnd->cmnd_read_buffer;
    xact->td_xfrlen = min(cmnd->cmnd_read_buffer, cmnd->buf_len);
    if(xact->td_xfrlen == 0)
    {
        printk("rdy_to_xfer: no data to transfer, error!");
        spin_unlock_irqrestore(&scsi_target_lock, flags);
        up(&scsi_thread_tmd_semaphore);
        scsi_target_ldfree(bp, xact, 0);
        if((*bp->h.r_action)!=NULL)
            (*bp->h.r_action)(QIN_TMD_FIN, tmd);
        return 0;
    }
    tmd->cd_scsi_status = cmnd->result;
 #ifdef LUN_ALLOCATE
    /*if(scsi_check_role(tmd,cmnd) == 0)
    {
        printk("xmit_response: this tmd's initiator role not match.\n");
        spin_unlock_irqrestore(&scsi_target_lock, flags);
        up(&scsi_thread_tmdlist_semaphore);
        scsi_target_ldfree(tmd);
        tmd->cd_state = CDST_DONE;
        if((*bp->h.r_action)!=NULL)
            (*bp->h.r_action)(QIN_TMD_FIN, tmd);
        return 0;
    }*/
#endif
	
//	printk("rdy_to_xfer: cmnd %p cmnd->handle %d cd_totlen %d cd_xfrlen %d cd_scsi_status %d\n", cmnd, cmnd->cmnd_handle, tmd->cd_totlen, tmd->cd_xfrlen, tmd->cd_scsi_status);
    if (cmnd->data_direction == DMA_FROM_DEVICE)
        xact->td_hflags |= TDFH_DATA_IN;
    else if (cmnd->data_direction == DMA_TO_DEVICE)
        xact->td_hflags |= TDFH_DATA_OUT;
    tmd->cd_flags |= CDF_PRIVATE_0;
	//printk("isp_rdy_to_xfer:here a tmd executed ok.\n");
	xact->td_hflags &= ~TDFH_STSVALID;
    
	spin_unlock_irqrestore(&scsi_target_lock, flags);
 	up(&scsi_thread_tmd_semaphore);
    if((*bp->h.r_action)!=NULL)	
        (*bp->h.r_action)(QIN_TMD_CONT, xact);
    return 0;
}

//called when ABORT TASK
void
isp_task_mgmt_done(struct SM *the_message)
{
	printk("isp_task_mgmt_done: begin.\n");
	return;
}

//called when LUN RESET or TARGET RESET
void
isp_report_aen(int fn, u64 lun)
{
	printk("isp_report_aen: begin.\n");
	return;
}

/*
 * vim:ts=4:sw=4:expandtab
 */
