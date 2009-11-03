/* $Id: isp_scsa.c,v 1.49 2009/02/14 00:09:02 mjacob Exp $ */
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
 * SCSI (SCSA) midlayer interface for Qlogic FC/SCSI/IP Host Adapter Driver
 */


#include "isp_solaris.h"
#include "isp_ioctl.h"
#include <sys/disp.h>
#include <sys/t_lock.h>
#include <sys/callb.h>

static int isp_scsa_tran_tgt_init(dev_info_t *, dev_info_t *, scsi_hba_tran_t *, struct scsi_device *);
static void isp_scsa_tran_tgt_free(dev_info_t *, dev_info_t *, scsi_hba_tran_t *, struct scsi_device *);
static int isp_scsa_start(struct scsi_address *, struct scsi_pkt *);
static int isp_scsa_abort(struct scsi_address *, struct scsi_pkt *);
static int isp_scsa_reset(struct scsi_address *, int);
static int isp_scsa_getcap(struct scsi_address *, char *, int);
static int isp_scsa_setcap(struct scsi_address *, char *, int, int);
static struct scsi_pkt *isp_scsa_init_pkt(struct scsi_address *, struct scsi_pkt *, struct buf *, int, int, int, int, int (*)(caddr_t), caddr_t);
static void isp_scsa_destroy_pkt(struct scsi_address *, struct scsi_pkt *);
static void isp_scsa_dmafree(struct scsi_address *, struct scsi_pkt *);
static void isp_scsa_sync_pkt(struct scsi_address *, struct scsi_pkt *);
static int isp_scsa_reset_notify(struct scsi_address *, int, void (*)(caddr_t), caddr_t);
static int isp_scsa_hba_reset_notify_setup(struct scsi_address *, int, void (*)(caddr_t), caddr_t, kmutex_t *, struct scsi_reset_notify_entry **);
static int isp_scsa_get_bus_addr(struct scsi_device *, char *, int);
static int isp_scsa_get_name(struct scsi_device *, char *, int);
static int isp_scsa_bus_config(dev_info_t *, uint_t, ddi_bus_config_op_t, void *, dev_info_t **);
static int isp_scsa_bus_unconfig(dev_info_t *, uint_t, ddi_bus_config_op_t, void *);

static int isp_extpkt_alloc(ispsoftc_t *, isp_cmd_t *, int, int, int, int);
static int isp_cap(struct scsi_address *, char *, int, int, int, int);
static int isp_scsa_polled(ispsoftc_t *, isp_cmd_t *);

int
isp_scsa_init(ispsoftc_t *isp)
{
	scsi_hba_tran_t *tran;

	/*
	 * Allocate a transport structure
	 */
	tran = scsi_hba_tran_alloc(isp->isp_dip, SCSI_HBA_CANSLEEP);
	if (tran == NULL) {
		isp_prt(isp, ISP_LOGERR, "scsi_hba_tran_alloc failed");
		return (DDI_FAILURE);
	}

	tran->tran_hba_private	= isp;
	tran->tran_tgt_private	= NULL;
	tran->tran_tgt_init	= isp_scsa_tran_tgt_init;
	tran->tran_tgt_free	= isp_scsa_tran_tgt_free;

	tran->tran_start	= isp_scsa_start;
	tran->tran_abort	= isp_scsa_abort;
	tran->tran_reset	= isp_scsa_reset;
	tran->tran_getcap	= isp_scsa_getcap;
	tran->tran_setcap	= isp_scsa_setcap;
	tran->tran_init_pkt	= isp_scsa_init_pkt;
	tran->tran_destroy_pkt	= isp_scsa_destroy_pkt;
	tran->tran_dmafree	= isp_scsa_dmafree;
	tran->tran_sync_pkt	= isp_scsa_sync_pkt;
	tran->tran_reset_notify = isp_scsa_reset_notify;

	tran->tran_get_bus_addr	= isp_scsa_get_bus_addr;
	tran->tran_get_name	= isp_scsa_get_name;
	tran->tran_bus_config	= isp_scsa_bus_config;
	tran->tran_bus_unconfig	= isp_scsa_bus_unconfig;

	/*
	 * Attach this instance of the hba
	 */
	if (scsi_hba_attach_setup(isp->isp_dip, &isp->isp_dma_attr, tran, SCSI_HBA_TRAN_CLONE)) {
		scsi_hba_tran_free(tran);
		isp_prt(isp, ISP_LOGERR, "scsi_hba_attach failed");
		return (DDI_FAILURE);
	}
	isp->isp_tran = tran;

	isp->isp_osinfo.task_kick = timeout(isp_tkick, isp, drv_usectohz(30000));

	return (DDI_SUCCESS);
}

/*
 * scsi start support routines
 */

static INLINE void append_to_waitq_tail(ispsoftc_t *, isp_cmd_t *);
static INLINE void insert_in_waitq_front(ispsoftc_t *, isp_cmd_t *);
static INLINE isp_cmd_t *remove_from_waitq_front(ispsoftc_t *);
static INLINE isp_cmd_t *remove_from_waitq(ispsoftc_t *, isp_cmd_t *);

static INLINE void
append_to_waitq_tail(ispsoftc_t *isp, isp_cmd_t *sp)
{
	isp_prt(isp, ISP_LOGDEBUG2, "append_to_waitq_tail(%p)", CMD2PKT(sp));
	if (isp->isp_osinfo.wqf) {
		isp->isp_osinfo.wqt->cmd_next = sp;
	} else {
		isp->isp_osinfo.wqf = sp;
	}
	isp->isp_osinfo.wqt = sp;
	sp->cmd_next = NULL;
}

static INLINE void
insert_in_waitq_front(ispsoftc_t *isp, isp_cmd_t *sp)
{
	sp->cmd_next = isp->isp_osinfo.wqf;
	if (isp->isp_osinfo.wqf == NULL)
		isp->isp_osinfo.wqt = sp;
	isp->isp_osinfo.wqf = sp;
	isp_prt(isp, ISP_LOGDEBUG2, "insert_in_waitq_front(%p)", CMD2PKT(sp));
}

static INLINE isp_cmd_t *
remove_from_waitq_front(ispsoftc_t *isp)
{
	isp_cmd_t *sp = isp->isp_osinfo.wqf;
	if (sp) {
		isp->isp_osinfo.wqf = sp->cmd_next;
		sp->cmd_next = NULL;
		if (isp->isp_osinfo.wqf == NULL)
			isp->isp_osinfo.wqt = NULL;
	}
	isp_prt(isp, ISP_LOGDEBUG2, "remove_from_waitq_front(%p)", CMD2PKT(sp));
	return (sp);
}

static INLINE isp_cmd_t *
remove_from_waitq(ispsoftc_t *isp, isp_cmd_t *wantedsp)
{
	isp_cmd_t *sp = isp->isp_osinfo.wqf;
	if (sp == NULL || wantedsp == NULL) {
		return (NULL);
	}
	if (sp == wantedsp) {
		isp->isp_osinfo.wqf = sp->cmd_next;
		sp->cmd_next = NULL;
		if (isp->isp_osinfo.wqf == NULL)
			isp->isp_osinfo.wqt = NULL;
		isp_prt(isp, ISP_LOGDEBUG2, "remove_from_waitq(%p)=%p",
		    CMD2PKT(wantedsp), CMD2PKT(sp));
		return (sp);
	}
	while (sp->cmd_next) {
		if (sp->cmd_next == wantedsp) {
			sp->cmd_next = wantedsp->cmd_next;
			wantedsp->cmd_next = NULL;
			if (isp->isp_osinfo.wqt == wantedsp) {
				isp->isp_osinfo.wqt = sp;
			}
			sp = wantedsp;
			break;
		}
		sp = sp->cmd_next;
	}
	isp_prt(isp, ISP_LOGDEBUG2, "remove_from_waitq(%p)=%p",
	    CMD2PKT(wantedsp), CMD2PKT(sp));
	return (sp);
}

void
isp_kick(void *arg)
{
	ispsoftc_t *isp = arg;
	isp_cmd_t *sp;
	int keepgoing;

	ISP_LOCK(isp);

	isp_prt(isp, ISP_LOGDEBUG1, "isp_kick(rst=%d block=%u drain=%u wqf=%p)",
	    isp->isp_osinfo.reset_pending, isp->isp_blocked, isp->isp_draining,
	    isp->isp_osinfo.wqf);

	if (isp->isp_osinfo.reset_pending) {
		isp->isp_osinfo.reset_pending = 0;
		if (isp_drain_reset(isp, "isp_kick")) {
			ISP_UNLOCK(isp);
			return;
		}
	}
	isp->isp_osinfo.kickt = 0;

	if (isp->isp_osinfo.wqf == NULL) {
		ISP_UNLOCK(isp);
		return;
	}
	if (isp->isp_blocked || isp->isp_draining) {
		SCHED_KICK(isp);
		ISP_UNLOCK(isp);
		return;
	}
	keepgoing = 1;
	while (isp->isp_osinfo.wqf != NULL && keepgoing) {
		sp = remove_from_waitq_front(isp);
		switch (isp_start(CMD2PKT(sp))) {
		case CMD_QUEUED:
			isp_prt(isp, ISP_LOGDEBUG1, "kick(%p) QUEUED", sp);
			sp->cmd_flags |= CFLAG_RUNNING;
			/*
			 * Be fair. Restart the timer for the
			 * command now that we have it going.
			 */
			RESTART_TIMER(sp);
			break;
		case CMD_EAGAIN:
		case CMD_RQLATER:
			isp_prt(isp, ISP_LOGDEBUG1, "kick(%p) RQLATER/EAGAIN", sp);
			keepgoing = 0;
			insert_in_waitq_front(isp, sp);
			SCHED_KICK(isp);
			break;
		case CMD_COMPLETE:
			isp_prt(isp, ISP_LOGDEBUG1, "kick(%p) CMPLT", sp);
			isp_done(CMD2PKT(sp));
			break;
		default:
			break;
		}

	}
	ISP_UNLOCK(isp);
}

int
isp_drain_reset(ispsoftc_t *isp, char *msg)
{
	/*
	 * Drain active commands.
	 */
	if (isp_drain(isp, msg)) {
		isp->isp_osinfo.kickt = 0;
		SCHED_KICK(isp);
		return (-1);
	}
	isp->isp_state = ISP_NILSTATE;
	isp_reset(isp);
	if (isp->isp_state != ISP_RESETSTATE) {
		isp->isp_osinfo.kickt = 0;
		isp->isp_blocked = 1;
		SCHED_KICK(isp);
		isp_prt(isp, ISP_LOGWARN, "%s: failed to reset HBA", msg);
		return (-1);
	}
	isp_init(isp);
	if (isp->isp_state != ISP_INITSTATE) {
		isp->isp_osinfo.kickt = 0;
		isp->isp_blocked = 1;
		SCHED_KICK(isp);
		isp_prt(isp, ISP_LOGWARN,
		    "%s: did not enter init state after reset", msg);
		return (-1);
	}
	isp->isp_state = ISP_RUNSTATE;
	isp->isp_blocked = 0;
	return (0);
}

int
isp_drain(ispsoftc_t *isp, char *whom)
{
	isp->isp_draining = 1;
	while (isp->isp_nactive) {
		clock_t tmo;

		drv_getparm(LBOLT, &tmo);
		tmo += drv_usectohz(60*1000000);
		isp_prt(isp, ISP_LOGDEBUG1, "%s: draining %u",
		    whom, isp->isp_nactive);
		tmo = cv_timedwait_sig(ISP_DRAIN_CV(isp), ISP_LOCKP(isp), tmo);
		if (tmo <= 0) {
			isp_prt(isp, ISP_LOGERR,
			    "%s: command drainoff timed out/interrupted", whom);
			isp->isp_draining = 0;
			return (-1);
		}
	}
	isp->isp_draining = 0;
	SCHED_KICK(isp);
	return (0);
}

/*
 * SCSA entry points
 */


static int
isp_scsa_tran_tgt_init(dev_info_t *hba_dip, dev_info_t *tgt_dip, scsi_hba_tran_t *tran, struct scsi_device *sd)
{
	int maxlun, maxtgt;
	ispsoftc_t *isp = TRAN2ISP(tran);
	struct gg *ggp;

	if (isp == NULL) {
		return (DDI_FAILURE);
	}

#ifdef	__sparc__
	if (IS_FC(isp) && strcmp(ddi_get_name(tgt_dip), "sd") == 0) {
		return (DDI_FAILURE);
	}
#endif

	maxtgt = ISP_MAX_TARGETS(isp);
	if (sd->sd_address.a_target >= maxtgt) {
		return (DDI_NOT_WELL_FORMED);
	}
	ggp = &isp->isp_osinfo.ggp[sd->sd_address.a_target];
	isp_prt(isp, ISP_LOGDEBUG0, "isp_scsa_tran_tgt_init: target %d lun %d", sd->sd_address.a_target, sd->sd_address.a_lun);
	if (ggp->tranp == NULL) {
		ggp->tranp = tran;
		tran->tran_tgt_private = ggp;
		return (DDI_SUCCESS);
	} else if (ggp->tranp == tran && tran->tran_tgt_private == ggp) {
		isp_prt(isp, ISP_LOGALL, "re-init tgt %d", sd->sd_address.a_target);
		return (DDI_SUCCESS);
	} else {
		return (DDI_FAILURE);
	}
}

static void
isp_scsa_tran_tgt_free(dev_info_t *hba_dip, dev_info_t *tgt_dip, scsi_hba_tran_t *tran, struct scsi_device *sd)
{
	ispsoftc_t *isp = ADDR2ISP(&sd->sd_address);
	struct gg *ggp;

	UNUSED_PARAMETER(hba_dip);
	UNUSED_PARAMETER(tran);
	UNUSED_PARAMETER(tgt_dip);

	isp_prt(isp, ISP_LOGDEBUG0, "isp_scsa_tran_tgt_free: target %d lun %d", sd->sd_address.a_target, sd->sd_address.a_lun);
	ggp = &isp->isp_osinfo.ggp[sd->sd_address.a_target];
	ggp->tranp = NULL;
	tran->tran_tgt_private = NULL;
}

static int
isp_scsa_start(struct scsi_address *ap, struct scsi_pkt *pkt)
{
	isp_cmd_t *sp = PKT2CMD(pkt);
	ispsoftc_t *isp = ADDR2ISP(ap);
	int result;

	pkt->pkt_state = pkt->pkt_statistics = 0;
	pkt->pkt_reason = CMD_INCOMPLETE;

	isp_prt(isp, ISP_LOGDEBUG1, "isp_scsa_start(%p): %d.%d.%d", pkt, XS_CHANNEL(pkt), XS_TGT(pkt), XS_LUN(pkt));

	ISP_LOCK(isp);
	/*
	 * Start a timer for this command if it isn't polled.
	 * This means we time things on wait queues too.
	 */
	if ((pkt->pkt_flags & FLAG_NOINTR) == 0) {
		START_TIMER(sp);
	}

	/*
	 * Check for queue blockage...
	 */
	if (isp->isp_blocked || isp->isp_draining) {
isp_prt(isp, ISP_LOGALL, "BLOCKED");
		if (pkt->pkt_flags & FLAG_NOINTR) {
			ISP_UNLOCK(isp);
			return (TRAN_BUSY);
		}
		append_to_waitq_tail(isp, sp);
		SCHED_KICK(isp);
		ISP_UNLOCK(isp);
		return (TRAN_ACCEPT);
	}

	if (pkt->pkt_flags & FLAG_NOINTR) {
		result = isp_scsa_polled(isp, PKT2CMD(pkt));
		ISP_UNLOCK(isp);
		return (result);
	}

	result = isp_start(pkt);

	isp_prt(isp, ISP_LOGDEBUG2, "isp_scsa_start(start=%d, %p)", result, pkt);

	switch (result) {
	case CMD_QUEUED:
		sp->cmd_flags |= CFLAG_RUNNING;
		/*
		 * Be fair. Restart the timer for the
		 * command now that we have it going.
		 */
		if ((pkt->pkt_flags & FLAG_NOINTR) == 0) {
			RESTART_TIMER(sp);
		}
		result = TRAN_ACCEPT;
		break;
	case CMD_EAGAIN:
		isp_prt(isp, ISP_LOGDEBUG0, "isp_scsa_start(%p) EAGAIN", pkt);
		result = TRAN_BUSY;
		break;
	case CMD_RQLATER:
isp_prt(isp, ISP_LOGALL, "RQLATER");
		isp_prt(isp, ISP_LOGDEBUG0, "isp_scsa_start(%p) RQLATER", pkt);
		SCHED_KICK(isp);
		append_to_waitq_tail(isp, sp);
		result = TRAN_ACCEPT;
		break;
	case CMD_COMPLETE:
		isp_prt(isp, ISP_LOGDEBUG0, "isp_scsa_start(%p) CMPLT", pkt);
		isp_done(pkt);
		result = TRAN_ACCEPT;
		break;
	default:
		isp_prt(isp, ISP_LOGERR, "isp_scsa_start(%p) 0x%x?", pkt, result);
		result = TRAN_FATAL_ERROR;
		break;
	}
	ISP_UNLOCK(isp);
	return (result);
}

static int
isp_scsa_polled(ispsoftc_t *isp, isp_cmd_t *sp)
{
	int result, rval = TRAN_FATAL_ERROR;
	struct scsi_pkt *pkt = CMD2PKT(sp);
	int infinite = 0, mswait;
	int iok = isp->isp_osinfo.isp_ints_ok;

	isp->isp_osinfo.isp_ints_ok = 0;
	result = isp_start(CMD2PKT(sp));
	isp->isp_osinfo.isp_ints_ok = iok;

	isp_prt(isp, ISP_LOGDEBUG0, "isp_scsa_polled(start=%d, %p)", result, pkt);

	switch (result) {
	case CMD_COMPLETE:
	case CMD_QUEUED:
		rval = TRAN_ACCEPT;
		break;
	case CMD_RQLATER:
	case CMD_EAGAIN:
		if (XS_NOERR(pkt)) {
			XS_SETERR(pkt, HBA_BOTCH);
		}
		rval = TRAN_BUSY;
		break;
	}

	if (result != CMD_COMPLETE) {
		return (rval);
	}

	if ((mswait = XS_TIME(pkt)) == 0) {
		infinite = 1;
	} else {
		mswait *= 1000;
	}

	while (mswait > 0 || infinite) {
		uint32_t isr;
		uint16_t sema, mbox;
		hrtime_t hrstart, elapsed;

		hrstart = gethrtime();
		if (ISP_READ_ISR(isp, &isr, &sema, &mbox)) {
			isp_intr(isp, isr, sema, mbox);
			if (XS_CMD_DONE_P(pkt)) {
				break;
			}
		}
		elapsed = gethrtime() - hrstart;
		/*
		 * If the elapsed time is greater than a millisecond, don't delay...
		 */
		if (elapsed > (1000 * 1000)) {
			mswait -= (elapsed / (1000 * 1000));
		} else {
			ISP_DELAY(1000 - (elapsed / 1000));
			mswait -= 1;
		}
	}

	/*
	 * If no other error occurred but we didn't finish,
	 * something bad happened.
	 */
	if (XS_CMD_DONE_P(pkt) == 0) {
		XS_SETERR(pkt, HBA_BOTCH);
	}
	if (pkt->pkt_comp) {
		ISP_UNLOCK(isp);
		(*pkt->pkt_comp)(pkt);
		ISP_LOCK(isp);
	}
	return (rval);
}

static int
isp_scsa_abort(struct scsi_address *ap, struct scsi_pkt *pkt)
{
	isp_cmd_t *sp = PKT2CMD(pkt);
	ispsoftc_t *isp = ADDR2ISP(ap);
	int rv = FALSE;

	ISP_LOCK(isp);
	STOP_TIMER(sp);
	if (remove_from_waitq(isp, sp) != NULL) {
		XS_SETERR(pkt, HBA_ABORTED);
		isp_done(pkt);
		rv = TRUE;
	} else {
		sp->cmd_flags |= CFLAG_ABORTING;
	 	if (isp_control(isp, ISPCTL_ABORT_CMD, pkt) == 0) {
			rv = TRUE;
		}
		/* the command will eventually get returned if this succeeded */
	}
	ISP_UNLOCK(isp);
	return (rv);
}

static int
isp_scsa_reset(struct scsi_address *ap, int level)
{
	ispsoftc_t *isp = ADDR2ISP(ap);
	int targ, chan, r;

	targ = ap->a_target;
	chan = 0;
	ISP_LOCK(isp);
	if (level == RESET_TARGET) {
		r = isp_control(isp, ISPCTL_RESET_DEV, chan, targ);
	} else {
		r = isp_control(isp, ISPCTL_RESET_BUS, chan);
	}
	ISP_UNLOCK(isp);
	if (r) {
		return (FALSE);
	} else {
		return (TRUE);
	}
}

static int
isp_cap(struct scsi_address *ap, char *cap, int is_fc, int val, int tonly, int set)
{
	ispsoftc_t *isp = ADDR2ISP(ap);
	sdparam *sdp;
	int cidx;
	int bus, tgt, i, start, end;
	int rval = FALSE;

	cidx = scsi_hba_lookup_capstr(cap);
	if (cidx == -1) {
		return (UNDEFINED);
	}
	tgt = ap->a_target;
	if (tonly) {
		start = tgt;
		end = start + 1;
	} else {
		start = 0;
		end = MAX_TARGETS;
	}
	bus = 0;
	sdp = SDPARAM(isp, bus);

	ISP_LOCK(isp);
	switch (cidx) {
	case SCSI_CAP_DMA_MAX:
		if (set == 0) {
			rval = isp->isp_dma_attr.dma_attr_count_max;
		}
		break;
	case SCSI_CAP_MSG_OUT:
		if (set == 0) {
			rval = TRUE;
		}
		break;
	case SCSI_CAP_DISCONNECT:
		if (is_fc) {
			rval = TRUE;
		} else if (set) {
			for (i = start; i < end; i++) {
				if (val) {
					sdp->isp_devparam[i].goal_flags |= DPARM_DISC;
				} else {
					sdp->isp_devparam[i].goal_flags &= ~DPARM_DISC;
				}
				sdp->isp_devparam[i].dev_update = 1;
			}
			sdp->update = 1;
		} else {
			if (tonly) {
				if ((sdp->isp_devparam[tgt].nvrm_flags & DPARM_DISC)) {
					rval = TRUE;
				}
			} else {
				rval = TRUE;
			}
		}
		break;
	case SCSI_CAP_SYNCHRONOUS:
		if (is_fc) {
			rval = TRUE;
		} else if (set) {
			for (i = start; i < end; i++) {
				if (val) {
					sdp->isp_devparam[i].goal_flags |= DPARM_SYNC;
				} else {
					sdp->isp_devparam[i].goal_flags &= ~DPARM_SYNC;
				}
				sdp->isp_devparam[i].dev_update = 1;
			}
			sdp->update = 1;
		} else {
			if (tonly) {
				if ((sdp->isp_devparam[tgt].nvrm_flags & DPARM_SYNC)) {
					rval = TRUE;
				}
			} else {
				rval = TRUE;
			}
		}
		break;
	case SCSI_CAP_WIDE_XFER:
		if (is_fc) {
			rval = TRUE;
		} else if (set) {
			for (i = start; i < end; i++) {
				if (val) {
					sdp->isp_devparam[i].goal_flags |= DPARM_WIDE;
				} else {
					sdp->isp_devparam[i].goal_flags &= ~DPARM_WIDE;
				}
				sdp->isp_devparam[i].dev_update = 1;
			}
			sdp->update = 1;
		} else {
			if (tonly) {
				if ((sdp->isp_devparam[tgt].nvrm_flags & DPARM_WIDE)) {
					rval = TRUE;
				}
			} else {
				rval = TRUE;
			}
		}
		break;
	case SCSI_CAP_PARITY:
		if (is_fc) {
			rval = TRUE;
		} else if (set) {
			for (i = start; i < end; i++) {
				if (val) {
					sdp->isp_devparam[i].goal_flags |= DPARM_PARITY;
				} else {
					sdp->isp_devparam[i].goal_flags &= ~DPARM_PARITY;
				}
				sdp->isp_devparam[i].dev_update = 1;
			}
			sdp->update = 1;
		} else {
			if (tonly) {
				if ((sdp->isp_devparam[tgt].nvrm_flags & DPARM_PARITY)) {
					rval = TRUE;
				}
			} else {
				rval = TRUE;
			}
		}
		break;
	case SCSI_CAP_INITIATOR_ID:
		if (is_fc) {
			rval = TRUE;
		} else if (set) {
			if (tonly == 0 || val < 0 || val >= MAX_TARGETS) {
				break;
			}
			sdp->isp_initiator_id = val;
			sdp->update = 1;
			rval = TRUE;
		} else {
			rval = sdp->isp_initiator_id;
		}
		break;
	case SCSI_CAP_UNTAGGED_QING:
		break;
	case SCSI_CAP_TAGGED_QING:
		if (is_fc) {
			rval = TRUE;
		} else if (set) {
			for (i = start; i < end; i++) {
				if (val) {
					sdp->isp_devparam[i].goal_flags |= DPARM_TQING;
				} else {
					sdp->isp_devparam[i].goal_flags &= ~DPARM_TQING;
				}
				sdp->isp_devparam[i].dev_update = 1;
			}
			sdp->update = 1;
		} else {
			if (tonly) {
				if ((sdp->isp_devparam[tgt].nvrm_flags & DPARM_TQING)) {
					rval = TRUE;
				}
			} else {
				rval = TRUE;
			}
		}
		break;
	case SCSI_CAP_ARQ:
		if (is_fc) {
			if (set) {
				rval = TRUE;
			}
		} else if (set) {
			for (i = start; i < end; i++) {
				if (val) {
					sdp->isp_devparam[i].goal_flags |= DPARM_ARQ;
				} else {
					sdp->isp_devparam[i].goal_flags &= ~DPARM_ARQ;
				}
				sdp->isp_devparam[i].dev_update = 1;
			}
			sdp->update = 1;
		} else {
			if (tonly) {
				if ((sdp->isp_devparam[tgt].nvrm_flags & DPARM_ARQ)) {
					rval = TRUE;
				}
			} else {
				rval = TRUE;
			}
		}
		break;
	case SCSI_CAP_SECTOR_SIZE:
		if (tonly) {
			if (set) {
				isp->isp_osinfo.ggp[tgt].ssiz = (uint16_t) val;
				rval = TRUE;
			} else {
				rval = isp->isp_osinfo.ggp[tgt].nsec;
			}
		}
		break;
	case SCSI_CAP_TOTAL_SECTORS:
		if (tonly) {
			if (set) {
				isp->isp_osinfo.ggp[tgt].nsec = val;
				rval = TRUE;
			} else {
				rval = isp->isp_osinfo.ggp[tgt].nsec;
			}
		}
		break;
	case SCSI_CAP_GEOMETRY:
		if (tonly) {
			if (set) {
				isp->isp_osinfo.ggp[tgt].geom = (uint16_t) val;
				rval = TRUE;
			} else {
				if (isp->isp_osinfo.ggp[tgt].geom) {
					rval = isp->isp_osinfo.ggp[tgt].geom;
				} else {
					rval = (63 << 16) | 255;
				}
			}
		}
		break;
	case SCSI_CAP_RESET_NOTIFICATION:
		rval = TRUE;
		break;
	case SCSI_CAP_QFULL_RETRIES:
		break;
	case SCSI_CAP_QFULL_RETRY_INTERVAL:
		break;
	case SCSI_CAP_SCSI_VERSION:
		if (set == 0) {
			rval = SCSI_VERSION_3;
		} else {
			rval = FALSE;
		}
		break;
	case SCSI_CAP_INTERCONNECT_TYPE:
		if (set == 0) {
			if (is_fc) {
				rval = INTERCONNECT_FIBRE;
			} else {
				rval = INTERCONNECT_PARALLEL;
			}
		} else {
			rval = FALSE;
		}
		break;
	case SCSI_CAP_LUN_RESET:
		rval = FALSE;
		break;
#ifdef	SCSI_CAP_CDB_LEN
	case SCSI_CAP_CDB_LEN:
		if (set == 0) {
			if (is_fc) {
				rval = 16;
			} else {
				rval = 12;
			}
		} else {
			rval = FALSE;
		}
		break;
#endif
	default:
		rval = UNDEFINED;
		break;
	}
	ISP_UNLOCK(isp);
	return (rval);
}

static int
isp_scsa_getcap(struct scsi_address *ap, char *cap, int whom)
{
	if (cap == NULL) {
		return (FALSE);
	}
	return (isp_cap(ap, cap, IS_FC(ADDR2ISP(ap)), 0, whom, 0));
}

static int
isp_scsa_setcap(struct scsi_address *ap, char *cap, int value, int whom)
{
	if (cap == NULL) {
		return (FALSE);
	}
	return (isp_cap(ap, cap, IS_FC(ADDR2ISP(ap)), 0, whom, 1));
}

static struct scsi_pkt *
isp_scsa_init_pkt(struct scsi_address *ap, struct scsi_pkt *pkt, struct buf *bp, int cmdlen, int stslen, int tgtlen, int flags, int (*callback)(), caddr_t arg)
{
	ispsoftc_t *isp = ADDR2ISP(ap);
	isp_cmd_t *sp;
	int dma_flags, rval, kf;
	uint16_t cmd_flags;
	uint_t ccnt;

	kf = (callback == SLEEP_FUNC)? KM_SLEEP : KM_NOSLEEP;
	if (pkt == NULL) {
		sp = kmem_cache_alloc(isp->isp_osinfo.isp_mcache, kf);
		if (sp == NULL) {
			return (NULL);
		}
		pkt = CMD2PKT(sp);
	} else {
		sp = PKT2CMD(pkt);
		if (sp->cmd_dmahandle && (sp->cmd_flags & CFLAG_DMA_MAPPED)) {
			sp->cmd_flags &= ~CFLAG_DMA_MAPPED;
			(void) ddi_dma_unbind_handle(sp->cmd_dmahandle);
		}
	}

	sp->cmd_timer		= 0;
	sp->cmd_cdblen		= cmdlen;
	sp->cmd_scblen		= sizeof (sp->cmd_arqstatus);
	sp->cmd_privlen		= tgtlen;
	sp->cmd_flags		= 0;
	bzero((caddr_t)&sp->cmd_dmacookie, sizeof (ddi_dma_cookie_t));
	sp->cmd_dmacount	= 0;
	sp->cmd_ccnt		= 0;
	pkt->pkt_scbp		= (opaque_t)&sp->cmd_arqstatus;
	pkt->pkt_cdbp		= (opaque_t)&sp->cmd_cdb;
	pkt->pkt_address	= *ap;
	pkt->pkt_comp		= NULL;
	pkt->pkt_flags		= 0;
	pkt->pkt_time		= 0;
	pkt->pkt_resid		= 0;
	pkt->pkt_statistics	= 0;
	pkt->pkt_reason		= 0;
	pkt->pkt_private	= sp->cmd_pkt_private;
	pkt->pkt_ha_private	= isp;

	/* zero cdbp and pkt_private */
	bzero((caddr_t) sp->cmd_cdb, sizeof (sp->cmd_cdb));
	bzero((caddr_t) sp->cmd_pkt_private, sizeof (sp->cmd_pkt_private));

	if ((cmdlen > (int) sizeof (sp->cmd_cdb)) || (tgtlen > (int) PKT_PRIV_LEN) || (stslen > (int) SECMDS_STATUS_SIZE)) {
		if (isp_extpkt_alloc(isp, sp, cmdlen, tgtlen, stslen, kf)) {
			isp_prt(isp, ISP_LOGERR, "extpkt failed");
			kmem_cache_free(isp->isp_osinfo.isp_mcache, sp);
			return (NULL);
		}
	}
	isp_prt(isp, ISP_LOGDEBUG2, "isp_scsa_init_pkt(%p): new for %d.%d.%d (cdblen=%d stslen=%d)", pkt, 0, ap->a_target, ap->a_lun, cmdlen, stslen);

	if (bp == NULL || bp->b_bcount == 0) {
		return (pkt);
	}

	/*
	 * Second (optional) step of isp_scsa_init_pkt:  dma allocation
	 */
	cmd_flags = sp->cmd_flags;

	/*
	 * Set some flags.
	 */
	if (bp->b_flags & B_READ) {
		cmd_flags &= ~CFLAG_DMASEND;
		dma_flags = DDI_DMA_READ;
	} else {
		cmd_flags |= CFLAG_DMASEND;
		dma_flags = DDI_DMA_WRITE;
	}
	if (flags & PKT_CONSISTENT) {
		cmd_flags |= CFLAG_CMDIOPB;
		dma_flags |= DDI_DMA_CONSISTENT;
	}

	rval = ddi_dma_buf_bind_handle(sp->cmd_dmahandle, bp, dma_flags, callback, arg, &sp->cmd_dmacookie, &ccnt);

	isp_prt(isp, ISP_LOGDEBUG2, "isp_scsa_init_pkt(%p): dma cookie count %d, %ld byte %s rval=%d",
	    pkt, ccnt, bp->b_bcount, ((bp->b_flags & B_READ) == B_READ)? "read" : "write", rval);

	if (rval != DDI_DMA_MAPPED || ccnt > 65535) {
		switch (rval) {
		default:
			bioerror(bp, EIO);
			break;
		case DDI_DMA_NORESOURCES:
			bioerror(bp, 0);
			break;
		case DDI_DMA_NOMAPPING:
		case DDI_DMA_BADATTR:
			bioerror(bp, EFAULT);
			break;
		case DDI_DMA_TOOBIG:
			bioerror(bp, EINVAL);
			break;
		}
		isp_scsa_destroy_pkt(ap, pkt);
		isp_prt(isp, ISP_LOGDEBUG2, "dma setup failed (%d)", geterror(bp));
		return (NULL);
	}
	cmd_flags |= CFLAG_DMA_MAPPED;
	sp->cmd_ccnt = (uint16_t) ccnt;
	sp->cmd_dmacount = bp->b_bcount;
	sp->cmd_flags = cmd_flags;
	return (pkt);
}

static int
isp_extpkt_alloc(ispsoftc_t *isp, isp_cmd_t *sp, int cmdlen, int tgtlen, int stslen, int kf)
{
	struct scsi_pkt *pkt = CMD2PKT(sp);
	caddr_t cdbp, scbp, tgt;
	int failure = 0;

	tgt = cdbp = scbp = NULL;
	if (IS_FC(isp) && cmdlen > 16) {
		return (1);
	}
	if (cmdlen > (int) sizeof (union scsi_cdb)) {
		if ((cdbp = kmem_zalloc((size_t)cmdlen, kf)) == NULL) {
			failure++;
		} else {
			pkt->pkt_cdbp = (opaque_t)cdbp;
			sp->cmd_flags |= CFLAG_CDBEXTERN;
		}
	}
	if (tgtlen > (int) PKT_PRIV_LEN) {
		if ((tgt = kmem_zalloc(tgtlen, kf)) == NULL) {
			failure++;
		} else {
			sp->cmd_flags |= CFLAG_PRIVEXTERN;
			pkt->pkt_private = tgt;
		}
	}
	if (stslen > (int) SECMDS_STATUS_SIZE) {
		if ((scbp = kmem_zalloc((size_t)stslen, kf)) == NULL) {
			failure++;
		} else {
			sp->cmd_flags |= CFLAG_SCBEXTERN;
			pkt->pkt_scbp = (opaque_t)scbp;
		}
	}
	if (failure) {
		if (tgt) {
			kmem_free(tgt, tgtlen);
		}
		if (cdbp) {
			kmem_free(cdbp, (size_t)cmdlen);
		}
		if (scbp) {
			kmem_free(scbp, (size_t)stslen);
		}
	}
	return (failure);
}

static void
isp_scsa_destroy_pkt(struct scsi_address *ap, struct scsi_pkt *pkt)
{
	isp_cmd_t *sp = PKT2CMD(pkt);
	ispsoftc_t *isp = ADDR2ISP(ap);

	isp_prt(XS_ISP(pkt), ISP_LOGDEBUG2, "isp_scsa_destroy_pkt(%p): %d.%d.%d", pkt, XS_CHANNEL(pkt), XS_TGT(pkt), XS_LUN(pkt));
	if (sp->cmd_dmahandle && (sp->cmd_flags & CFLAG_DMA_MAPPED)) {
		/*
		 * Free the mapping.
		 */
		sp->cmd_flags &= ~CFLAG_DMA_MAPPED;
		(void) ddi_dma_unbind_handle(sp->cmd_dmahandle);
	}
	if (sp->cmd_flags & CFLAG_CDBEXTERN) {
		kmem_free((caddr_t)pkt->pkt_cdbp, (size_t)sp->cmd_cdblen);
		sp->cmd_flags &= ~CFLAG_CDBEXTERN;
		pkt->pkt_cdbp = (opaque_t)&sp->cmd_cdb;

	}
	if (sp->cmd_flags & CFLAG_SCBEXTERN) {
		sp->cmd_flags &= ~CFLAG_SCBEXTERN;
		kmem_free((caddr_t)pkt->pkt_scbp, (size_t)sp->cmd_scblen);
		pkt->pkt_scbp = (opaque_t)&sp->cmd_arqstatus;
	}
	if (sp->cmd_flags & CFLAG_PRIVEXTERN) {
		sp->cmd_flags &= ~CFLAG_PRIVEXTERN;
		kmem_free((caddr_t)pkt->pkt_private, (size_t)sp->cmd_privlen);
		pkt->pkt_private = sp->cmd_pkt_private;
	}
	kmem_cache_free(isp->isp_osinfo.isp_mcache, sp);
}

/*
 * kmem cache constructor and destructor:
 */
int
isp_kmem_cache_constructor(void *buf, void *cdrarg, int kmflags)
{
	isp_cmd_t *sp = buf;
	ispsoftc_t *isp = cdrarg;
	int rval, (*callback)(caddr_t) = (kmflags & KM_SLEEP) ? DDI_DMA_SLEEP : DDI_DMA_DONTWAIT;
	rval = ddi_dma_alloc_handle(isp->isp_dip, &isp->isp_dma_attr, callback, NULL, &sp->cmd_dmahandle);
	if (rval != DDI_SUCCESS) {
		return (-1);
	}
	return (0);
}

/*ARGSUSED*/
void
isp_kmem_cache_destructor(void *buf, void *cdrarg)
{
	isp_cmd_t *sp = buf;
	UNUSED_PARAMETER(cdrarg);
	if (sp->cmd_dmahandle) {
		ddi_dma_free_handle(&sp->cmd_dmahandle);
		sp->cmd_dmahandle = 0;
	}
}


static void
isp_scsa_dmafree(struct scsi_address *ap, struct scsi_pkt *pkt)
{
	isp_cmd_t *sp = PKT2CMD(pkt);
	UNUSED_PARAMETER(ap);
	if (sp->cmd_dmahandle && (sp->cmd_flags & CFLAG_DMA_MAPPED)) {
		sp->cmd_flags &= ~CFLAG_DMA_MAPPED;
		/*
		 * Free the mapping.
		 */
		(void) ddi_dma_unbind_handle(sp->cmd_dmahandle);
	}
}

static void
isp_scsa_sync_pkt(struct scsi_address *ap, struct scsi_pkt *pkt)
{
	isp_cmd_t *sp = PKT2CMD(pkt);
	UNUSED_PARAMETER(ap);
	if (sp->cmd_dmahandle) {
		int dir = (sp->cmd_flags & CFLAG_DMASEND) ? DDI_DMA_SYNC_FORDEV : DDI_DMA_SYNC_FORCPU;
		if (ddi_dma_sync(sp->cmd_dmahandle, 0, 0, dir) != DDI_SUCCESS) {
			isp_prt(XS_ISP(pkt), ISP_LOGERR, "scsi_sync_pkt");
		}
	}
}

/*
 * routines for reset notification setup, to register or cancel.
 */

static int
isp_scsa_reset_notify(struct scsi_address *ap, int flag, void (*callback)(caddr_t), caddr_t arg)
{
	ispsoftc_t *isp = ADDR2ISP(ap);
	return (scsi_hba_reset_notify_setup(ap, flag, callback, arg, ISP_LOCKP(isp), &isp->isp_osinfo.rlf));
}

/*
 * Routine to perform the notification callbacks after a reset.
 */
void
isp_scsa_reset_notify_cb(kmutex_t *m, struct scsi_reset_notify_entry **listp)
{
	int	i, count;
	struct	scsi_reset_notify_entry	*p;
	struct	notify_entry {
		void	(*callback)(caddr_t);
		caddr_t	arg;
	} *list;

	if ((p = *listp) == NULL) {
		return;
	}

	count = 0;
	while (p != NULL) {
		count++;
		p = p->next;
	}
	list = (struct notify_entry *)kmem_alloc((size_t)(count * sizeof (struct notify_entry)), KM_NOSLEEP);
	if (list == NULL) {
		cmn_err(CE_WARN, "scsi_reset_notify: kmem_alloc failed");
		return;
	}

	for (i = 0, p = *listp; i < count; i++, p = p->next) {
		list[i].callback = p->callback;
		list[i].arg = p->arg;
	}
	mutex_exit(m);
	for (i = 0; i < count; i++) {
		if (list[i].callback != NULL) {
			(void) (*list[i].callback)(list[i].arg);
		}
	}
	kmem_free((caddr_t)list, (size_t)(count * sizeof (struct notify_entry)));
	mutex_enter(m);
}

static fcportdb_t *
isp_tgt2pdb(ispsoftc_t *isp, int chan, int target)
{
	fcportdb_t *lp;
	int hdlidx;

	if (target >= MAX_FC_TARG || target < 0) {
		return (NULL);
	}
	hdlidx = FCPARAM(isp, chan)->isp_dev_map[target] - 1;
	if (hdlidx < 0 || hdlidx >= MAX_FC_TARG) {
		return (NULL);
	}
	lp = &FCPARAM(isp, chan)->portdb[hdlidx];
	if (lp->state != FC_PORTDB_STATE_VALID) {
		return (NULL);
	}
	return (lp);
}

static int
isp_scsa_get_bus_addr(struct scsi_device *sd, char *name, int len)
{
	ispsoftc_t *isp = ADDR2ISP(&sd->sd_address);
	if (isp == NULL) {
		return (0);
	}
	if (IS_FC(isp)) {
		fcportdb_t *lp = isp_tgt2pdb(isp, 0, sd->sd_address.a_target);
		if (lp == NULL) {
			return (0);
		}
		(void) isp_snprintf(name, len, "%x", lp->portid);
	} else {
		(void) isp_snprintf(name, len, "%x,%x", sd->sd_address.a_target, sd->sd_address.a_lun);
	}
	return (1);
}

static int
isp_scsa_get_name(struct scsi_device *sd, char *name, int len)
{
	ispsoftc_t *isp = ADDR2ISP(&sd->sd_address);
	if (isp == NULL) {
		return (0);
	}
	if (IS_FC(isp)) {
		fcportdb_t *lp = isp_tgt2pdb(isp, 0, sd->sd_address.a_target);
		if (lp == NULL) {
			return (0);
		}
		(void) isp_snprintf(name, (size_t) len, "w%08x%08x,%x", (uint32_t) (lp->port_wwn >> 32), (uint32_t) (lp->port_wwn & 0xffffffff), sd->sd_address.a_lun);
	} else {
		(void) isp_snprintf(name, (size_t) len, "%x,%x", sd->sd_address.a_target, sd->sd_address.a_lun);
	}
	return (1);
}

static int
isp_scsa_bus_config(dev_info_t *p, uint_t flag, ddi_bus_config_op_t op, void *arg, dev_info_t **childp)
{
	return (ndi_busop_bus_config(p, flag, op, arg, childp, 0));
}

static int
isp_scsa_bus_unconfig(dev_info_t *p, uint_t flag, ddi_bus_config_op_t op, void *arg)
{
	return (ndi_busop_bus_unconfig(p, flag, op, arg));
}

/*
 * 'done' routine- we're called with a lock held.
 */
#define	GOT_ARQ	(STATE_GOT_STATUS | STATE_ARQ_DONE)

void
isp_done(struct scsi_pkt *pkt)
{
	ispsoftc_t *isp = XS_ISP(pkt);
	isp_cmd_t *sp = PKT2CMD(pkt);

	XS_CMD_S_DONE(pkt);
	if (XS_CMD_WDOG_P(pkt)) {
		return;
	}
	STOP_TIMER(sp);
	if (isp->isp_draining) {
		cv_signal(ISP_DRAIN_CV(isp));
	}

	if (sp->cmd_flags & XS_PSTS_HAS_SENSE) {
		pkt->pkt_state |= STATE_ARQ_DONE;
	}

	/*
	 * If the pkt_reason field hasn't been set to something from what
	 * we started with, then we're completing a command with a status
	 * to be synthesized (since we didn't actually run the command).
	 * In this case, we have to synthesize pkt_state, pkt_statistics
	 * and pkt_reason.
	 */
	if (pkt->pkt_reason >= HBA_NOERROR) {
		int wasdata = (sp->cmd_dmahandle)? STATE_XFERRED_DATA : 0;
		switch (pkt->pkt_reason) {
		case HBA_NOERROR:
			pkt->pkt_reason = CMD_CMPLT;
			pkt->pkt_state |= STATE_GOT_BUS | STATE_GOT_TARGET | STATE_SENT_CMD | STATE_GOT_STATUS;
			break;
		case HBA_BOTCH:
			wasdata = 0;
			pkt->pkt_reason = CMD_TRAN_ERR;
			break;
		case HBA_CMDTIMEOUT:
			pkt->pkt_reason = CMD_TIMEOUT;
			pkt->pkt_statistics = STAT_TIMEOUT;
			pkt->pkt_state |= STATE_GOT_BUS | STATE_GOT_TARGET | STATE_SENT_CMD;
			break;
		case HBA_SELTIMEOUT:
			wasdata = 0;
			pkt->pkt_reason = CMD_INCOMPLETE;
			pkt->pkt_state |= STATE_GOT_BUS;
			break;
		case HBA_TGTBSY:
			wasdata = 0;
			*XS_STSP(pkt) = STATUS_BUSY;
			pkt->pkt_state |= STATE_GOT_BUS | STATE_GOT_TARGET | STATE_SENT_CMD | STATE_GOT_STATUS;
			break;
		case HBA_BUSRESET:
			pkt->pkt_reason = CMD_RESET;
			pkt->pkt_statistics = STAT_BUS_RESET;
			pkt->pkt_state |= STATE_GOT_BUS | STATE_GOT_TARGET;
			break;
		case HBA_ABORTED:
			pkt->pkt_reason = CMD_ABORTED;
			pkt->pkt_statistics = STAT_DEV_RESET;
			pkt->pkt_state |= STATE_GOT_BUS | STATE_GOT_TARGET | STATE_SENT_CMD;
			break;
		case HBA_ARQFAIL:
		case HBA_DATAOVR:
			pkt->pkt_reason = CMD_TRAN_ERR;
			pkt->pkt_state |= STATE_GOT_BUS | STATE_GOT_TARGET | STATE_SENT_CMD;
			break;
		}
		pkt->pkt_state |= wasdata;
	}

	if ((pkt->pkt_state & GOT_ARQ) == GOT_ARQ && (*XS_STSP(pkt) == STATUS_CHECK)) {
		if ((size_t) sp->cmd_scblen >= sizeof (struct scsi_arq_status)) {
			struct scsi_arq_status *aqp = (struct scsi_arq_status *) pkt->pkt_scbp;
			(*((uint8_t *)&aqp->sts_rqpkt_status)) = 0;
			aqp->sts_rqpkt_statistics = 0;
			aqp->sts_rqpkt_reason = CMD_CMPLT;
			aqp->sts_rqpkt_state = STATE_GOT_BUS | STATE_GOT_TARGET | STATE_SENT_CMD | STATE_XFERRED_DATA | STATE_GOT_STATUS;
			aqp->sts_rqpkt_resid = 0;
			/*
			 * Sanity check the sense data- sd blows up if we
			 * hand it a non-extended sense data block with
			 * zero residual. We'll insist on Extended Sense
			 * class and 'valid' being set.
			 */
			if (aqp->sts_sensedata.es_class != CLASS_EXTENDED_SENSE) {
				aqp->sts_rqpkt_reason = CMD_TRAN_ERR;
				aqp->sts_rqpkt_state = 0;
				aqp->sts_rqpkt_resid = sizeof (struct scsi_extended_sense);
			}
		} else {
			pkt->pkt_reason = CMD_TRAN_ERR;
		}
	}

	isp_prt(isp, ISP_LOGDEBUG1, "isp_done(%p) reason %d", pkt, pkt->pkt_reason);

	if (XS_GET_RESID(pkt)) {
		isp_prt(isp, ISP_LOGDEBUG0, "residual %ld of %ld for %d.%d.%d", XS_GET_RESID(pkt), XS_XFRLEN(pkt), XS_CHANNEL(pkt), XS_TGT(pkt), XS_LUN(pkt));
	}
	if ((pkt->pkt_flags & FLAG_NOINTR) == 0) {
		ISP_UNLOCK(isp);
		(*pkt->pkt_comp)(pkt);
		ISP_LOCK(isp);
	}
}

void
isp_update_scsi_props(ispsoftc_t *isp, int bus, int tgt, int mhz, int wb, int tq)
{
	static char prop[32];
	ARGSUSED(bus);
	mhz *= (1000 * (wb + 1));
	ISP_SNPRINTF(prop, sizeof prop, "target%x-sync-speed", tgt);
	isp_update_this_prop(isp, prop, (caddr_t) &mhz, sizeof mhz, mhz != 0);
	ISP_SNPRINTF(prop, sizeof prop, "target%x-TQ", tgt);
	isp_update_this_prop(isp, prop, (caddr_t) &tq, sizeof tq, tq != 0);
	ISP_SNPRINTF(prop, sizeof prop, "target%x-wide", tgt);
	isp_update_this_prop(isp, prop, (caddr_t) &wb, sizeof wb, wb != 0);
}

void
isp_update_fc_props(ispsoftc_t *isp, int tgt, int valid)
{
	int tmp;

	static char prop[32];
	ISP_SNPRINTF(prop, sizeof prop, "target%x-sync-speed", tgt);
	if (FCPARAM(isp, 0)->isp_gbspeed == 4) {
		tmp = 400000;
	} else if (FCPARAM(isp, 0)->isp_gbspeed == 2) {
		tmp = 200000;
	} else {
		tmp = 100000;
	}
	isp_update_this_prop(isp, prop, (char *)&tmp, sizeof tmp, valid);
	ISP_SNPRINTF(prop, sizeof prop, "target%x-TQ", tgt);
	tmp = valid;
	isp_update_this_prop(isp, prop, (char *)&tmp, sizeof tmp, valid);
}

void
isp_update_this_prop(ispsoftc_t *isp, char *p, caddr_t valuep, int size, int flag)
{
	int length;

	if (ddi_getproplen(DDI_DEV_T_ANY, isp->isp_dip, DDI_PROP_DONTPASS, p, &length) == DDI_PROP_SUCCESS) {
		if (flag == 0) {
			(void) ddi_prop_remove(DDI_DEV_T_NONE, isp->isp_dip, p);
		} else if (size) {
			(void) ddi_prop_modify(DDI_DEV_T_NONE, isp->isp_dip, 0, p, valuep, size);
		}
	} else if (flag) {
		(void) ddi_prop_create(DDI_DEV_T_NONE, isp->isp_dip, 0, p, valuep, size);
	}
}

static void
isp_dchange(void *arg)
{
	ispsoftc_t *isp = arg;
	fcparam *fcp = FCPARAM(isp, 0);
	fcportdb_t *lp;
	char *nname;
	int vtgt, hdlidx, circ, nc, i;
	uchar_t wwn_array[8];
	uint64_t wwn;
	dev_info_t *cdip = NULL;

	ndi_devi_enter(isp->isp_dip, &circ);
	ISP_LOCK(isp);
	for (vtgt = -1, nc = i = 0; i < MAX_FC_TARG; i++) {
		if (isp->isp_osinfo.ggp[i].coming) {
			nc++;
			if (vtgt == -1) {
				vtgt = i;
			}
		}
	}
	if (vtgt == -1) {
		ISP_UNLOCK(isp);
		ndi_devi_exit(isp->isp_dip, circ);
		isp->isp_osinfo.dchange_pending = 0;
		return;
	}
	nc--;
	hdlidx = fcp->isp_dev_map[vtgt] - 1;
	if (hdlidx < 0 || hdlidx >= MAX_FC_TARG) {
		ISP_UNLOCK(isp);
		ndi_devi_exit(isp->isp_dip, circ);
		isp->isp_osinfo.dchange_pending = 0;
		return;
	}
	lp = &fcp->portdb[hdlidx];
	if (lp->state != FC_PORTDB_STATE_VALID) {
		ISP_UNLOCK(isp);
		ndi_devi_exit(isp->isp_dip, circ);
		isp->isp_osinfo.dchange_pending = 0;
		return;
	}
	isp->isp_osinfo.ggp[vtgt].coming = 0;
	ISP_UNLOCK(isp);

#ifdef	__sparc__
	nname = "ssd";
#else
	nname = "sd";
#endif
	if (ndi_devi_alloc(isp->isp_dip, nname, DEVI_SID_NODEID, &cdip) != NDI_SUCCESS) {
		ndi_devi_exit(isp->isp_dip, circ);
		isp->isp_osinfo.ggp[vtgt].coming = 1;
		nc++;	/* force retry */
		isp_prt(isp, ISP_LOGWARN, "failed to allocate child devinfo", vtgt);
		goto fail;
	}
	ndi_devi_exit(isp->isp_dip, circ);

	wwn = lp->node_wwn;
	for (i = 7; i >= 0; i--) {
		wwn_array[i] = wwn;
		wwn >>= 8;
	}
	if (ndi_prop_update_byte_array(DDI_DEV_T_NONE, cdip, "node-wwn", wwn_array, 8) != DDI_PROP_SUCCESS) {
		isp->isp_osinfo.ggp[vtgt].coming = 1;
		nc++;	/* force retry */
		isp_prt(isp, ISP_LOGWARN, "ndi prop update of node-wwn property failed");
		goto fail;
	}

	wwn = lp->port_wwn;
	for (i = 7; i >= 0; i--) {
		wwn_array[i] = wwn;
		wwn >>= 8;
	}
	if (ndi_prop_update_byte_array(DDI_DEV_T_NONE, cdip, "port-wwn", wwn_array, 8) != DDI_PROP_SUCCESS) {
		isp->isp_osinfo.ggp[vtgt].coming = 1;
		nc++;	/* force retry */
		isp_prt(isp, ISP_LOGWARN, "ndi prop update of port-wwn property failed");
		goto fail;
	}

	if (ndi_prop_update_int(DDI_DEV_T_NONE, cdip, "priority-reserve", 1) != DDI_PROP_SUCCESS) {
		isp->isp_osinfo.ggp[vtgt].coming = 1;
		nc++;	/* force retry */
		isp_prt(isp, ISP_LOGWARN, "ndi prop update of priority-reserve failed");
		goto fail;
	}

	if (ndi_prop_update_int(DDI_DEV_T_NONE, cdip, "target", vtgt) != DDI_PROP_SUCCESS) {
		isp->isp_osinfo.ggp[vtgt].coming = 1;
		nc++;	/* force retry */
		isp_prt(isp, ISP_LOGWARN, "ndi prop update of target failed");
		goto fail;
	}

	if (ndi_prop_update_int(DDI_DEV_T_NONE, cdip, "lun", 0) != DDI_PROP_SUCCESS) {
		isp->isp_osinfo.ggp[vtgt].coming = 1;
		nc++;	/* force retry */
		isp_prt(isp, ISP_LOGWARN, "ndi prop update of lun failed");
		goto fail;
	}

	if (ndi_devi_online(cdip, NDI_ONLINE_ATTACH) != NDI_SUCCESS) {
                isp_prt(isp, ISP_LOGWARN, "failed to bring %s@w%016llx,0 online", nname, (unsigned long long) lp->port_wwn);
		goto fail;
        }

	isp->isp_osinfo.dchange_pending = 0;
	if (nc) {
		isp->isp_osinfo.dchange_needed = 1;
	}
	return;

fail:
	if (cdip) {
		ndi_prop_remove_all(cdip);
                (void) ndi_devi_free(cdip);
	}
	isp->isp_osinfo.dchange_pending = 0;
	if (nc) {
		isp->isp_osinfo.dchange_needed = 1;
	}
}

static void
isp_rescan(void *arg)
{
	ispsoftc_t *isp = arg;
	ISP_LOCK(isp);
	isp->isp_osinfo.scan_pending = 0;
	(void) isp_fc_runstate(isp, 0, 250000);
	ISP_UNLOCK(isp);
}

void
isp_tkick(void *arg)
{
	ispsoftc_t *isp = arg;
	ISP_LOCK(isp);
	if (isp->isp_osinfo.scan_needed && isp->isp_osinfo.scan_pending == 0) {
		if (ddi_taskq_dispatch(isp->isp_osinfo.tq, isp_rescan, isp, DDI_NOSLEEP) == DDI_FAILURE) {
			isp_prt(isp, ISP_LOGWARN, "unable to dispatch rescan event");
		} else {
			isp->isp_osinfo.scan_pending = 1;
			isp->isp_osinfo.scan_needed = 0;
		}
	}
	if (isp->isp_osinfo.dchange_needed && isp->isp_osinfo.dchange_pending == 0) {
		if (ddi_taskq_dispatch(isp->isp_osinfo.tq, isp_dchange, isp, DDI_NOSLEEP) == DDI_FAILURE) {
			isp_prt(isp, ISP_LOGWARN, "unable to dispatch rescan event");
		} else {
			isp->isp_osinfo.dchange_pending = 1;
			isp->isp_osinfo.dchange_needed = 0;
		}
	}
	isp->isp_osinfo.task_kick = timeout(isp_tkick, isp, drv_usectohz(30000));
	ISP_UNLOCK(isp);
}
