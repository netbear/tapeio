/* $Id: isp_solaris.h,v 1.46 2009/02/14 00:09:02 mjacob Exp $ */
/*
 *  Copyright (c) 1997-2008 by Matthew Jacob
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
 * Solaris Platform definitions for the Qlogic FC/SCSI/IP Host Adapter Driver
 */
#ifndef	_ISP_SOLARIS_H
#define	_ISP_SOLARIS_H

#ifdef	__cplusplus
extern "C" {
#endif

#include <sys/modctl.h>
#include <sys/scsi/scsi.h>
#include <sys/scsi/impl/scsi_reset_notify.h>
#include <sys/pci.h>
#include "isp_scsa.h"

#ifndef	ARGSUSED
#define	ARGSUSED(x)	x = x
#endif

#define	ISP_LOG_BLOWTHROUGH	0x10000
#ifndef	UNUSED_PARAMETER
#define	UNUSED_PARAMETER(x)	(void) x
#endif

#ifndef	_NREG_BLKS
#define	_NREG_BLKS	5
#endif

#define	HBA_PORTID_PROPNAME	"nvram-wwn"

#define	ISP_PLATFORM_VERSION_MAJOR	1
#define	ISP_PLATFORM_VERSION_MINOR	0

struct gg {
	uint16_t 		geom;
	uint16_t 		ssiz;
	uint32_t 		nsec;
	uint32_t
				:	29,
		coming		:	1,
		going		:	1,
		here		:	1;
	scsi_hba_tran_t *	tranp;
};

struct isposinfo {
	struct scsi_hba_tran *	tran;		/* ISP HBA Transport ... */
	dev_info_t *		dip;		/* device info */
	ddi_iblock_cookie_t 	ibc;		/* IBLOCK cookie */
	kmutex_t		lock;		/* lock on this instance */
	kcondvar_t		mbox_cv;	/* cv on mailbox commands */
	ksema_t			mbox_sema;	/* sema on mailbox commands */
	ksema_t			fcs_sema;	/* sema on fc scratch area */
	ddi_device_acc_attr_t 	dev_attr;
	ddi_acc_handle_t 	pci_handle;
	ddi_acc_handle_t 	reg_acc_handle;
	ddi_acc_handle_t 	cmd_acc_handle;
	ddi_dma_handle_t 	dma_chandle;
	ddi_dma_attr_t		dma_attr;
	ddi_dma_handle_t 	dma_handle;
	ddi_dma_cookie_t 	dcookie;
	caddr_t			regs;
	uint16_t 		regoff[_NREG_BLKS];
	struct gg *		ggp;
	uint64_t		default_portwwn;
	uint64_t		default_nodewwn;
	uint64_t		portwwn;
	uint64_t		nodewwn;
	uint16_t		default_framesize;
	uint16_t		default_exec_throttle;
	unsigned int			: 16,
		default_id		: 8,
					: 5,
		default_role		: 3;
	isp_cmd_t *		wqf;
	isp_cmd_t *		wqt;
	struct scsi_reset_notify_entry *rlf;
	struct kmem_cache *	isp_mcache;
	timeout_id_t		task_kick;
	timeout_id_t		kickt;
	char *			name;
	int			instance;
	ddi_taskq_t *		tq;
	kcondvar_t		drain_cv;	/* cv on draining */
	unsigned volatile int		: 20,
		dchange_needed		: 1,
		dchange_pending		: 1,
		scan_needed		: 1,
		scan_pending		: 1,
		reset_pending		: 1,
		draining		: 1,
		blocked			: 1,
		isp_onintstack		: 1,
		isp_ints_ok		: 1,
		isp_mbox_cmd_done	: 1,
		isp_mbox_waiting	: 1,
		isp_suspended		: 1;
	size_t			pamt;
};

/*
 * Required Macros/Defines
 */
#ifdef	__GNUC__
#define	INLINE		inline
#else
#define	INLINE
#endif

#define	ISP_FC_SCRLEN		0x1000

#define	ISP_MEMZERO		bzero
#define	ISP_MEMCPY(d, s, a)	bcopy((s), (d), (a))
#define	ISP_SNPRINTF		isp_snprintf
#define	ISP_DELAY		drv_usecwait
#define	ISP_SLEEP(isp, x)					\
	ISP_UNLOCK(isp);					\
	if (isp->isp_osinfo.isp_ints_ok == 0 ||			\
	    isp->isp_osinfo.isp_onintstack) {			\
		drv_usecwait(x);				\
	} else {						\
		delay(drv_usectohz(x));				\
	}							\
	ISP_LOCK(isp);						\

#define	NANOTIME_T		hrtime_t
#define	GET_NANOTIME(x)		(*(x)) = gethrtime()
#define	GET_NANOSEC(x)		(*(x))
#define	NANOTIME_SUB(b, a)	((*(b) - *(a)) == 0? 1 : (*(b) - *(a)))

#define	MAXISPREQUEST(isp)	256

#define	MEMORYBARRIER(isp, type, offset, size)			\
switch (type) {							\
case SYNC_REQUEST:						\
{								\
	off_t off = (off_t) offset * QENTRY_LEN;		\
	ddi_dma_sync(isp->isp_osinfo.dma_chandle, off,		\
	    size, DDI_DMA_SYNC_FORDEV);				\
	break;							\
}								\
case SYNC_RESULT:						\
{								\
	off_t off = (off_t) offset * QENTRY_LEN;		\
	off += ISP_QUEUE_SIZE(RQUEST_QUEUE_LEN(isp));		\
	ddi_dma_sync(isp->isp_osinfo.dma_chandle, off,		\
	    size, DDI_DMA_SYNC_FORKERNEL);			\
	break;							\
}								\
case SYNC_SFORDEV:						\
	ddi_dma_sync(isp->isp_osinfo.dma_chandle,		\
	    ISP_QUEUE_SIZE(RQUEST_QUEUE_LEN(isp)) +		\
	    ISP_QUEUE_SIZE(RESULT_QUEUE_LEN(isp)) + offset,	\
	    size, DDI_DMA_SYNC_FORDEV);				\
	break;							\
case SYNC_SFORCPU:						\
	ddi_dma_sync(isp->isp_osinfo.dma_chandle,		\
	    ISP_QUEUE_SIZE(RQUEST_QUEUE_LEN(isp)) +		\
	    ISP_QUEUE_SIZE(RESULT_QUEUE_LEN(isp)) + offset,	\
	    size, DDI_DMA_SYNC_FORKERNEL);			\
	break;							\
case SYNC_REG:							\
default:							\
	break;							\
}

#define	MBOX_ACQUIRE		isp_mbox_acquire
#define	MBOX_WAIT_COMPLETE	isp_mbox_wait_complete
#define	MBOX_NOTIFY_COMPLETE(isp)					\
	isp->isp_osinfo.isp_mbox_cmd_done = 1;				\
	if (isp->isp_osinfo.isp_mbox_waiting) {				\
		cv_signal(&(isp)->isp_mbox_cv);				\
	}
#define	MBOX_RELEASE(isp)		sema_v(&(isp)->isp_mbox_sema)

#define	FC_SCRATCH_ACQUIRE(isp, chan)	0
#define	FC_SCRATCH_RELEASE(isp, chan)	do { } while (0)

#ifndef	SCSI_GOOD
#define	SCSI_GOOD	STATUS_GOOD
#endif
#ifndef	SCSI_CHECK
#define	SCSI_CHECK	STATUS_CHECK
#endif
#ifndef	SCSI_BUSY
#define	SCSI_BUSY	STATUS_BUSY
#endif
#ifndef	SCSI_QFULL
#define	SCSI_QFULL	STATUS_QFULL
#endif

#define	XS_T			struct scsi_pkt
#define	XS_DMA_ADDR_T		uint32_t
#define	XS_ISP(xs)		((ispsoftc_t *) (xs)->pkt_ha_private)

#define	XS_CHANNEL(xs)		0
#define	XS_TGT(xs)		((int) (xs)->pkt_address.a_target)
#define	XS_LUN(xs)		((int) (xs)->pkt_address.a_lun)
#define	XS_CDBP(xs)		(xs)->pkt_cdbp
#define	XS_CDBLEN(xs)		PKT2CMD(xs)->cmd_cdblen
#define	XS_XFRLEN(xs)		PKT2CMD(xs)->cmd_dmacount
#define	XS_TIME(xs)		((xs)->pkt_time * 1000)
#define	XS_SET_RESID(xs, val)	(xs)->pkt_resid = val
#define	XS_GET_RESID(xs)	(xs)->pkt_resid
#define	XS_STSP(xs)		(xs)->pkt_scbp
#define	XS_SNSP(xs)		&(PKT2CMD(xs))->cmd_arqstatus.sts_sensedata
#define	XS_SNSLEN(xs)		(sizeof (struct scsi_extended_sense))
#define	XS_SNSKEY(xs)		\
	(PKT2CMD(xs))->cmd_arqstatus.sts_sensedata.es_key

#define	XS_TAG_P		isp_tag_p
#define	XS_TAG_TYPE(xs)		(((xs)->pkt_flags & FLAG_TAGMASK) >> 11)

#define	XS_SETERR(xs, v)	(xs)->pkt_reason = v

#	define	HBA_NOERROR		100
#	define	HBA_BOTCH		101
#	define	HBA_CMDTIMEOUT		102
#	define	HBA_SELTIMEOUT		103
#	define	HBA_TGTBSY		104
#	define	HBA_BUSRESET		105
#	define	HBA_ABORTED		106
#	define	HBA_DATAOVR		107
#	define	HBA_ARQFAIL		108
#	define	HBA_BOGUS_ERROR		127

#define	XS_ERR(xs)		(xs)->pkt_reason
#define	XS_NOERR(xs)		(xs)->pkt_reason == HBA_BOGUS_ERROR || (xs)->pkt_reason == HBA_NOERROR
#define	XS_INITERR(xs)		\
	(xs)->pkt_scbp[0] = STATUS_GOOD, \
	(xs)->pkt_reason = HBA_BOGUS_ERROR, \
	PKT2CMD(xs)->cmd_flags &= ~XS_PSTS_ALL

#define	XS_SAVE_SENSE(xs, s, l)				\
	bcopy(s, XS_SNSP(xs), min(XS_SNSLEN(xs), l)),	\
	PKT2CMD(xs)->cmd_flags |= XS_PSTS_HAS_SENSE

#define	DEFAULT_IID(isp, chan)		(isp)->isp_osinfo.default_id
#define	DEFAULT_LOOPID(isp, chan)	(isp)->isp_osinfo.default_id
#define	DEFAULT_NODEWWN(isp, chan)	(isp)->isp_osinfo.default_nodewwn
#define	DEFAULT_PORTWWN(isp, chan)	(isp)->isp_osinfo.default_portwwn
#define	ACTIVE_NODEWWN(isp, chan)	(isp)->isp_osinfo.nodewwn? (isp)->isp_osinfo.nodewwn : FCPARAM(isp, chan)->isp_wwnn_nvram
#define	ACTIVE_PORTWWN(isp, chan)	(isp)->isp_osinfo.portwwn? (isp)->isp_osinfo.portwwn : FCPARAM(isp, chan)->isp_wwpn_nvram
#define	DEFAULT_FRAMESIZE(isp)		(isp)->isp_osinfo.default_framesize
#define	DEFAULT_EXEC_THROTTLE(isp)	(isp)->isp_osinfo.default_exec_throttle

#define	GET_DEFAULT_ROLE(isp, c)	isp->isp_osinfo.default_role
#define	SET_DEFAULT_ROLE(isp, c, role)	isp->isp_osinfo.default_role = role

#if	0
#define	ISP_IOXPUT_8(isp, s, d)						\
	ddi_put8(isp->isp_cmd_acc_handle, (uint8_t *)(d), (s))
#define	ISP_IOXPUT_16(isp, s, d)					\
	ddi_put16(isp->isp_cmd_acc_handle, (uint16_t *)(d), (s))
#define	ISP_IOXPUT_32(isp, s, d)					\
	ddi_put32(isp->isp_cmd_acc_handle, (uint32_t *)(d), (s))
#define	ISP_IOXGET_8(isp, s, d)						\
	(d) = ddi_get8(isp->isp_cmd_acc_handle, (uint8_t *)(s))
#define	ISP_IOXGET_16(isp, s, d)					\
	(d) = ddi_get16(isp->isp_cmd_acc_handle, (uint16_t *)(s))
#define	ISP_IOXGET_32(isp, s, d)					\
	(d) = ddi_get32(isp->isp_cmd_acc_handle, (uint32_t *)(s))
#endif

#if	defined(_BIG_ENDIAN)
#define	ISP_SWIZZLE_NVRAM_WORD(isp, rp)	*rp = ddi_swap16(*rp)
#define	ISP_SWIZZLE_NVRAM_LONG(isp, rp)	*rp = ddi_swap32(*rp)

#define	ISP_IOZGET_8(isp, s, d)		\
	d = ddi_get8(isp->isp_cmd_acc_handle, (uint8_t *)s)

#define	ISP_IOZGET_16(isp, s, d)	\
	d = ddi_get16(isp->isp_cmd_acc_handle, (uint16_t *)s)

#define	ISP_IOZGET_32(isp, s, d)	\
	d = ddi_get32(isp->isp_cmd_acc_handle, (uint32_t *)s)

#define	ISP_IOZPUT_8(isp, s, d)		\
	ddi_put8(isp->isp_cmd_acc_handle, (uint8_t *)d, s)

#define	ISP_IOZPUT_16(isp, s, d)	\
	ddi_put16(isp->isp_cmd_acc_handle, (uint16_t *)d, s)

#define	ISP_IOZPUT_32(isp, s, d)	\
	ddi_put32(isp->isp_cmd_acc_handle, (uint32_t *)d, s)

#define	ISP_IOXGET_8(isp, s, d)		\
	d = ddi_get8(isp->isp_cmd_acc_handle, (uint8_t *)s)

#define	ISP_IOXGET_16(isp, s, d)	\
	d = ddi_swap16(ddi_get16(isp->isp_cmd_acc_handle, (uint16_t *)s))

#define	ISP_IOXGET_32(isp, s, d)	\
	d = ddi_swap32(ddi_get32(isp->isp_cmd_acc_handle, (uint32_t *)s))

#define	ISP_IOXPUT_8(isp, s, d)		\
	ddi_put8(isp->isp_cmd_acc_handle, (uint8_t*)d, s)

#define	ISP_IOXPUT_16(isp, s, d)	\
	ddi_put16(isp->isp_cmd_acc_handle, (uint16_t *)d, ddi_swap16(s))

#define	ISP_IOXPUT_32(isp, s, d)	\
	ddi_put32(isp->isp_cmd_acc_handle, (uint32_t *)d, ddi_swap32(s))

#else
#define	ISP_SWIZZLE_NVRAM_WORD(isp, rp)
#define	ISP_SWIZZLE_NVRAM_LONG(isp, rp)
#define	ISP_IOZGET_8(isp, s, d)		d = (*((uint8_t *)s))
#define	ISP_IOZGET_16(isp, s, d)	d = (((*((uint16_t *)s)) & 0xff) << 8) | (((*((uint16_t *)s)) & 0xff00) >> 8)
#define	ISP_IOZGET_32(isp, s, d)	d = \
	(((*((uint32_t *)s)) & 0x000000ff) << 24) |	\
	(((*((uint32_t *)s)) & 0x0000ff00) <<  8) |	\
	(((*((uint32_t *)s)) & 0x00ff0000) >>  8) |	\
	(((*((uint32_t *)s)) & 0xff000000) >> 24)
#define	ISP_IOZPUT_8(isp, s, d)		*(d) = s
#define	ISP_IOZPUT_16(isp, s, d)	*(d) = ((s & 0xff00) >> 8) | ((s & 0xff) << 8)
#define	ISP_IOZPUT_32(isp, s, d)	*(d) = \
	((s & 0x000000ff) << 24) |	\
	((s & 0x0000ff00) <<  8) |	\
	((s & 0x00ff0000) >>  8) |	\
	((s & 0xff000000) >> 24)

#define	ISP_IOXGET_8(isp, s, d)		d = (*((uint8_t *)s))
#define	ISP_IOXGET_16(isp, s, d)	d = (*((uint16_t *)s))
#define	ISP_IOXGET_32(isp, s, d)	d = (*((uint32_t *)s))
#define	ISP_IOXPUT_8(isp, s, d)		*(d) = s
#define	ISP_IOXPUT_16(isp, s, d)	*(d) = s
#define	ISP_IOXPUT_32(isp, s, d)	*(d) = s
#endif
#define	ISP_SWAP16(a, b)	ddi_swap16(b)
#define	ISP_SWAP32(a, b)	ddi_swap32(b)

/*
 * Includes of common header files
 */
#define	ISP_EXEC_THROTTLE	128

#include "ispreg.h"
#include "isp_stds.h"
#include "ispmbox.h"
#include "ispvar.h"

#if defined(__GNUC__) || defined(__INTEL_COMPILER)
void isp_prt(ispsoftc_t *, int level, const char *, ...) __attribute__((__format__(__printf__,3,4)));
#else
void isp_prt(ispsoftc_t *, int level, const char *, ...);
#endif

/*
 * isp_osinfo definitions, extensions and shorthand.
 */

#define	isp_tran		isp_osinfo.tran
#define	isp_dip			isp_osinfo.dip
#define	isp_iblock		isp_osinfo.ibc
#define	isp_lock		isp_osinfo.lock
#define	isp_mbox_cv		isp_osinfo.mbox_cv
#define	isp_mbox_sema		isp_osinfo.mbox_sema
#define	isp_fcscratch_sema	isp_osinfo.fcs_sema
#define	isp_dev_attr		isp_osinfo.dev_attr
#define	isp_phandle		isp_osinfo.pci_handle
#define	isp_reg_acc_handle	isp_osinfo.reg_acc_handle
#define	isp_cmd_acc_handle	isp_osinfo.cmd_acc_handle
#define	isp_dma_chandle		isp_osinfo.dma_chandle
#define	isp_dma_attr		isp_osinfo.dma_attr
#define	isp_dma_handle		isp_osinfo.dma_handle
#define	isp_cdmacookie		isp_osinfo.dcookie
#define	isp_regs		isp_osinfo.regs
#define	isp_regoff		isp_osinfo.regoff
#define	isp_name		isp_osinfo.name
#define	isp_unit		isp_osinfo.instance
#define	isp_blocked		isp_osinfo.blocked
#define	isp_draining		isp_osinfo.draining

/*
 * Driver prototypes..
 */

extern int isp_rd_isr(ispsoftc_t *, uint32_t *, uint16_t *, uint16_t *);
extern int isp_rd_isr_2300(ispsoftc_t *, uint32_t *, uint16_t *, uint16_t *);
extern uint32_t isp_rd_reg(ispsoftc_t *, int);
extern void isp_wr_reg(ispsoftc_t *, int, uint32_t);
extern uint32_t isp_rd_reg_1080(ispsoftc_t *, int);
extern void isp_reset0(ispsoftc_t *);
extern void isp_reset1(ispsoftc_t *);
extern void isp_wr_reg_1080(ispsoftc_t *, int, uint32_t);
extern void isp_dump_regs(ispsoftc_t *, const char *);
extern int isp_mbxdma_setup(ispsoftc_t *);
extern int isp_dma_setup(ispsoftc_t *, XS_T *, ispreq_t *, uint32_t *, uint32_t);
extern int isp_scsa_init(ispsoftc_t *);
extern void isp_update_scsi_props(ispsoftc_t *, int, int, int, int, int);
extern void isp_update_fc_props(ispsoftc_t *, int, int);
extern void isp_update_this_prop(ispsoftc_t *, char *, caddr_t, int, int);

extern void isp_kick(void *);
extern int isp_drain_reset(ispsoftc_t *, char *);
extern int isp_drain(ispsoftc_t *, char *);
extern void isp_scsa_reset_notify_cb(kmutex_t *, struct scsi_reset_notify_entry **);
#ifdef	ISP_TARGET_MODE
void	isp_attach_target(ispsoftc_t *);
void	isp_detach_target(ispsoftc_t *);
int	isp_target_notify(ispsoftc_t *, void *, uint32_t *);
int	isp_target_async(ispsoftc_t *, int, int);
#endif
int	isp_kmem_cache_constructor(void *, void *, int);
void	isp_kmem_cache_destructor(void *, void *);

/*
 * Driver wide data...
 */
extern void *isp_softc_state;
extern int isp_maxlun;


/*
 * Locking macros...
 */

#define	ISP_LOCK(isp)		mutex_enter(&isp->isp_lock)
#define	ISP_UNLOCK(isp)		mutex_exit(&isp->isp_lock)
#define	ISP_LOCKP(isp)		&(isp)->isp_lock
#define	ISP_MBOX_CV(isp)	&(isp)->isp_mbox_cv
#define	ISP_MBOX_SEMA(isp)	&(isp)->isp_mbox_sema
#define	ISP_DRAIN_CV(isp)	&(isp)->isp_mbox_cv

/*
 * Platform private flags
 */
#define	XS_PSTS_INWDOG		0x1000
#define	XS_PSTS_GRACE		0x2000
#define	XS_PSTS_DONE		0x4000
#define	XS_PSTS_HAS_SENSE	0x8000
#define	XS_PSTS_ALL		0xf000

#define	XS_CMD_S_WDOG(xs)	PKT2CMD(xs)->cmd_flags |= XS_PSTS_INWDOG
#define	XS_CMD_C_WDOG(xs)	PKT2CMD(xs)->cmd_flags &= ~XS_PSTS_INWDOG
#define	XS_CMD_WDOG_P(xs)	((PKT2CMD(xs)->cmd_flags & XS_PSTS_INWDOG) != 0)

#define	XS_CMD_S_GRACE(xs)	PKT2CMD(xs)->cmd_flags |= XS_PSTS_GRACE
#define	XS_CMD_C_GRACE(xs)	PKT2CMD(xs)->cmd_flags &= ~XS_PSTS_GRACE
#define	XS_CMD_GRACE_P(xs)	((PKT2CMD(xs)->cmd_flags & XS_PSTS_GRACE) != 0)

#define	XS_CMD_S_DONE(xs)	PKT2CMD(xs)->cmd_flags |= XS_PSTS_DONE
#define	XS_CMD_C_DONE(xs)	PKT2CMD(xs)->cmd_flags &= ~XS_PSTS_DONE
#define	XS_CMD_DONE_P(xs)	((PKT2CMD(xs)->cmd_flags & XS_PSTS_DONE) != 0)

#define	XS_CMD_S_CLEAR(xs)	PKT2CMD(xs)->cmd_flags &= ~XS_PSTS_ALL


#ifdef	__sparc__
#define	DMA_ATTR_SEG	0x00ffffff
#define	DMA_ATTR_GRAN	512
#else
#define	DMA_ATTR_SEG	0x0000ffff
#define	DMA_ATTR_GRAN	64
#endif

#define	_ISPNDS0(isp)		(IS_FC(isp)? ISP_RQDSEG_T2 : ISP_RQDSEG)
#ifdef	__sparc__
#define	ISP_SGLLEN(isp)		1
#else
#define ISP_SGLLEN(isp)	\
	(_ISPNDS0(isp) + ((MAXISPREQUEST(isp) - 2) * ISP_CDSEG))
#endif

#define	SDEV2TRAN(sd)		((sd)->sd_address.a_hba_tran)
#define	SDEV2ADDR(sd)		(&((sd)->sd_address))
#define	PKT2TRAN(pkt)		((pkt)->pkt_address.a_hba_tran)
#define	ADDR2TRAN(ap)		((ap)->a_hba_tran)

#define	TRAN2ISP(tran)		((ispsoftc_t *)(tran)->tran_hba_private)
#define	SDEV2ISP(sd)		(TRAN2ISP(SDEV2TRAN(sd)))
#define	PKT2ISP(pkt)		(TRAN2ISP(PKT2TRAN(pkt)))
#define	ADDR2ISP(ap)		(TRAN2ISP(ADDR2TRAN(ap)))

#define	CMD2ADDR(cmd)		(&CMD2PKT(cmd)->pkt_address)
#define	CMD2TRAN(cmd)		(CMD2PKT(cmd)->pkt_address.a_hba_tran)
#define	CMD2ISP(cmd)		(TRAN2ISP(CMD2TRAN(cmd)))

#define	ISP_RESET_NOTIFY(x)	isp_scsa_reset_notify_cb(ISP_LOCKP(x), &(x)->isp_osinfo.rlf)
#define	SCHED_KICK		isp_sched_kick

/*
 *
 */

#ifndef	PCI_VENDOR_QLOGIC
#define	PCI_VENDOR_QLOGIC		0x1077
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP1020
#define	PCI_PRODUCT_QLOGIC_ISP1020	0x1020
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP1080
#define	PCI_PRODUCT_QLOGIC_ISP1080	0x1080
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP1240
#define	PCI_PRODUCT_QLOGIC_ISP1240	0x1240
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP1280
#define	PCI_PRODUCT_QLOGIC_ISP1280	0x1280
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP10160
#define	PCI_PRODUCT_QLOGIC_ISP10160	0x1016
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP12160
#define	PCI_PRODUCT_QLOGIC_ISP12160	0x1216
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP2100
#define	PCI_PRODUCT_QLOGIC_ISP2100	0x2100
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP2200
#define	PCI_PRODUCT_QLOGIC_ISP2200	0x2200
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP2300
#define	PCI_PRODUCT_QLOGIC_ISP2300	0x2300
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP2312
#define	PCI_PRODUCT_QLOGIC_ISP2312	0x2312
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP2322
#define	PCI_PRODUCT_QLOGIC_ISP2322	0x2322
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP2422
#define	PCI_PRODUCT_QLOGIC_ISP2422	0x2422
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP2432
#define	PCI_PRODUCT_QLOGIC_ISP2432	0x2432
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP6312
#define	PCI_PRODUCT_QLOGIC_ISP6312	0x6312
#endif
#ifndef	PCI_PRODUCT_QLOGIC_ISP6322
#define	PCI_PRODUCT_QLOGIC_ISP6322	0x6322
#endif

#define	PCI_QLOGIC_ISP	\
	((PCI_PRODUCT_QLOGIC_ISP1020 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP1080	\
	((PCI_PRODUCT_QLOGIC_ISP1080 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP1240	\
	((PCI_PRODUCT_QLOGIC_ISP1240 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP1280	\
	((PCI_PRODUCT_QLOGIC_ISP1280 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP10160	\
	((PCI_PRODUCT_QLOGIC_ISP10160 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP12160	\
	((PCI_PRODUCT_QLOGIC_ISP12160 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP2100	\
	((PCI_PRODUCT_QLOGIC_ISP2100 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP2200	\
	((PCI_PRODUCT_QLOGIC_ISP2200 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP2300	\
	((PCI_PRODUCT_QLOGIC_ISP2300 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP2312	\
	((PCI_PRODUCT_QLOGIC_ISP2312 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP2322	\
	((PCI_PRODUCT_QLOGIC_ISP2322 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP2422	\
	((PCI_PRODUCT_QLOGIC_ISP2422 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP2432	\
	((PCI_PRODUCT_QLOGIC_ISP2432 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP6312	\
	((PCI_PRODUCT_QLOGIC_ISP6312 << 16) | PCI_VENDOR_QLOGIC)

#define	PCI_QLOGIC_ISP6322	\
	((PCI_PRODUCT_QLOGIC_ISP6322 << 16) | PCI_VENDOR_QLOGIC)

#define	SBUS_QLOGIC_ISP		0x00010002

#define	PCI_DFLT_LTNCY	0x40
#define	PCI_DFLT_LNSZ	0x10


/*
 * Platform specific 'inline' or support functions
 */
#ifdef	lint
#define	static
#endif

extern void isp_wdog(void *);
extern void isp_tkick(void *);
extern char *isp_snprintf(char *, size_t, const char *, ...);
extern int isp_mbox_acquire(ispsoftc_t *);
extern void isp_mbox_wait_complete(ispsoftc_t *, mbreg_t *);
extern void isp_scsa_sst(ispsoftc_t *, struct scsi_pkt *, ispstatusreq_t *);

#define	ISPFW_VERSION	0
extern void isp_get_firmware(int, int, uint32_t, const uint16_t **);

static INLINE int isp_tag_p(struct scsi_pkt *);
static INLINE int
isp_tag_p(struct scsi_pkt *pkt)
{
	if ((pkt->pkt_flags & FLAG_TAGMASK) == 0) {
		return(0);
	}
	if (IS_SCSI(XS_ISP(pkt))) {
		sdparam *sdp;
		uint8_t flags;
		sdp = SDPARAM(XS_ISP(pkt), XS_CHANNEL(pkt));
		flags = sdp->isp_devparam[XS_TGT(pkt)].actv_flags;
		if ((flags & DPARM_TQING) == 0) {
			return(0);
		}
	}
	return(1);
}

static INLINE void isp_sched_kick(ispsoftc_t *);
static INLINE void
isp_sched_kick(ispsoftc_t *isp)
{
	if (isp->isp_osinfo.kickt == 0) {
		int amt;
		if (isp->isp_state == ISP_RUNSTATE) {
			amt = 1;
		} else {
			amt = 2 * drv_usectohz(1000000);
		}
		isp->isp_osinfo.kickt = timeout(isp_kick, isp, amt);
	}
}

#ifndef	__GNUC__
#pragma	inline (isp_tag_p)
#pragma	inline (isp_sched_kick)
#endif

/*
 * Common inline functions
 */
#include "isp_library.h"

#ifdef	__cplusplus
}
#endif
#endif	/* _ISP_SOLARIS_H */
