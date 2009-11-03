/* $Id: isp_solaris.c,v 1.33 2009/02/14 00:09:02 mjacob Exp $ */
/*
 * Copyright (c) 1997-2008 by Matthew Jacob All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer. 2. Redistributions
 * in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * 
 * 
 * Alternatively, this software may be distributed under the terms of the the
 * GNU Public License ("GPL") with platforms where the prevalant license is
 * the GNU Public License:
 * 
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of The Version 2 GNU General Public License as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 675
 * Mass Ave, Cambridge, MA 02139, USA.
 * 
 * 
 * Matthew Jacob Feral Software 421 Laurel Avenue Menlo Park, CA 94025 USA
 * 
 * gplbsd at feral com
 */
/*
 * General Solaris support routines for Qlogic FC/SCSI/IP Host Adapter Driver
 */
#include "isp_solaris.h"

#define	isp_change_is_bad	0

static const char *roles[4] = {
    "(none)", "Target", "Initiator", "Target/Initiator"
};
static const char prom3[] =
    "Chan %d PortID 0x%06x Departed from Target %u because of %s";

void
isp_async(ispsoftc_t * isp, ispasync_t cmd,...)
{
	int tgt, bus;
	fcportdb_t *lp;
	static const char prom[] =
	    "Chan %d PortID 0x%06x handle 0x%x role %s %s WWPN 0x%08x%08x";
	static const char prom2[] =
	    "Chan %d PortID 0x%06x handle 0x%x role %s %s tgt %u WWPN 0x%08x%08x";
	va_list ap;


	switch (cmd) {
	case ISPASYNC_NEW_TGT_PARAMS:
	{
		sdparam        *sdp = isp->isp_param;
		char           *wt;
		int             mhz, tq, wide, flags, period;

		if (IS_FC(isp)) {
			break;
		}
		va_start(ap, cmd);
		bus = va_arg(ap, int);
		tgt = va_arg(ap, int);
		va_end(ap);
		sdp = SDPARAM(isp, bus);
		flags = sdp->isp_devparam[tgt].actv_flags;
		period = sdp->isp_devparam[tgt].actv_period;

		if ((flags & DPARM_SYNC) && period && (sdp->isp_devparam[tgt].actv_offset) != 0) {
			/*
			 * There's some ambiguity about our
			 * negotiated speed if we haven't detected
			 * LVD mode correctly (which seems to happen,
			 * unfortunately). If we're in LVD mode, then
			 * different rules apply about speed.
			 */
			if (sdp->isp_lvdmode || period < 0xc) {
				switch (period) {
				case 0x9:
					mhz = 80;
					break;
				case 0xa:
					mhz = 40;
					break;
				case 0xb:
					mhz = 33;
					break;
				case 0xc:
					mhz = 25;
					break;
				default:
					mhz = 1000 / (period * 4);
					break;
				}
			} else {
				mhz = 1000 / (period * 4);
			}
		} else {
			mhz = 0;
		}
		tq = wide = 0;
		switch (flags & (DPARM_WIDE | DPARM_TQING)) {
		case DPARM_WIDE:
			wide = 1;
			wt = ", Wide";
			break;
		case DPARM_TQING:
			tq = 1;
			wt = ", Tagged";
			break;
		case DPARM_WIDE | DPARM_TQING:
			tq = wide = 1;
			wt = ", Wide, Tagged";
			break;
		default:
			wt = " ";
			break;
		}
		if (mhz) {
			isp_prt(isp, ISP_LOGINFO, "Bus %d, Target %d, %dMHz, Offset %d%s",
			    bus, tgt, mhz, sdp->isp_devparam[tgt].actv_offset, wt);
		} else {
			isp_prt(isp, ISP_LOGINFO, "Bus %d Target %d Async Mode%s", bus, tgt, wt);
		}
		isp_update_scsi_props(isp, bus, tgt, mhz, wide, tq);
		break;
	}
	case ISPASYNC_BUS_RESET:
		va_start(ap, cmd);
		bus = va_arg(ap, int);
		va_end(ap);
		isp_prt(isp, ISP_LOGINFO, "SCSI bus %d reset detected", bus);
		ISP_UNLOCK(isp);
		ISP_RESET_NOTIFY(isp);
		ISP_LOCK(isp);
		break;
	case ISPASYNC_LOOP_DOWN:
		/*
		 * Hopefully we get here in time to minimize the number of
		 * commands we are firing off that are sure to die.
		 */
		isp->isp_blocked = 1;
		isp_prt(isp, ISP_LOGINFO, "Loop DOWN");
		break;
	case ISPASYNC_LOOP_UP:
		isp_prt(isp, ISP_LOGINFO, "Loop UP");
		break;
	case ISPASYNC_DEV_ARRIVED:
		va_start(ap, cmd);
		bus = va_arg(ap, int);
		lp = va_arg(ap, fcportdb_t *);
		va_end(ap);
		lp->reserved = 0;
		if (lp->roles & (SVC3_TGT_ROLE >> SVC3_ROLE_SHIFT)) {
			int dbidx = lp - FCPARAM(isp, bus)->portdb;
			int i;

			for (i = MAX_FC_TARG-1; i >= 0; i--) {
				if (i >= FL_ID && i <= SNS_ID) {
					continue;
				}
				if (FCPARAM(isp, bus)->isp_dev_map[i] == 0) {
					break;
				}
			}
			if (i < MAX_FC_TARG) {
				FCPARAM(isp, bus)->isp_dev_map[i] = dbidx + 1;
				lp->dev_map_idx = i + 1;
			} else {
				isp_prt(isp, ISP_LOGWARN, "out of target ids");
				isp_dump_portdb(isp, bus);
			}
		}
		if (lp->dev_map_idx && bus == 0) {
			tgt = lp->dev_map_idx - 1;
			isp_prt(isp, ISP_LOGCONFIG, prom2, bus,
			    lp->portid, lp->handle,
		            roles[lp->roles], "arrived at", tgt,
		    	    (uint32_t) (lp->port_wwn >> 32),
			    (uint32_t) lp->port_wwn);
			isp->isp_osinfo.ggp[tgt].coming = 1;
			isp->isp_osinfo.dchange_needed = 1;
		} else {
			isp_prt(isp, ISP_LOGCONFIG, prom, bus,
			    lp->portid, lp->handle,
		            roles[lp->roles], "arrived",
		    	    (uint32_t) (lp->port_wwn >> 32),
			    (uint32_t) lp->port_wwn);
		}
		break;
	case ISPASYNC_DEV_CHANGED:
		va_start(ap, cmd);
		bus = va_arg(ap, int);
		lp = va_arg(ap, fcportdb_t *);
		va_end(ap);
		lp->reserved = 0;
		if (isp_change_is_bad && bus == 0) {
			lp->state = FC_PORTDB_STATE_NIL;
			if (lp->dev_map_idx) {
				tgt = lp->dev_map_idx - 1;
				FCPARAM(isp, bus)->isp_dev_map[tgt] = 0;
				lp->dev_map_idx = 0;
				isp_prt(isp, ISP_LOGCONFIG, prom3, bus,
				    lp->portid, tgt, "change is bad");
				isp->isp_osinfo.ggp[tgt].going = 1;
				isp->isp_osinfo.dchange_needed = 1;
			} else {
				isp_prt(isp, ISP_LOGCONFIG, prom, bus,
				    lp->portid, lp->handle,
				    roles[lp->roles],
				    "changed and departed",
				    (uint32_t) (lp->port_wwn >> 32),
				    (uint32_t) lp->port_wwn);
			}
		} else {
			lp->portid = lp->new_portid;
			lp->roles = lp->new_roles;
			if (lp->dev_map_idx) {
				int t = lp->dev_map_idx - 1;
				FCPARAM(isp, bus)->isp_dev_map[t] =
				    (lp - FCPARAM(isp, bus)->portdb) + 1;
				tgt = lp->dev_map_idx - 1;
				isp_prt(isp, ISP_LOGCONFIG, prom2, bus,
				    lp->portid, lp->handle,
				    roles[lp->roles], "changed at", tgt,
				    (uint32_t) (lp->port_wwn >> 32),
				    (uint32_t) lp->port_wwn);
			} else {
				isp_prt(isp, ISP_LOGCONFIG, prom, bus,
				    lp->portid, lp->handle,
				    roles[lp->roles], "changed",
				    (uint32_t) (lp->port_wwn >> 32),
				    (uint32_t) lp->port_wwn);
			}
		}
		break;
	case ISPASYNC_DEV_STAYED:
		va_start(ap, cmd);
		bus = va_arg(ap, int);
		lp = va_arg(ap, fcportdb_t *);
		va_end(ap);
		if (lp->dev_map_idx && bus == 0) {
			tgt = lp->dev_map_idx - 1;
			isp_prt(isp, ISP_LOGCONFIG, prom2, bus,
			    lp->portid, lp->handle,
		    	    roles[lp->roles], "stayed at", tgt,
		    	    (uint32_t) (lp->port_wwn >> 32),
			    (uint32_t) lp->port_wwn);
		} else {
			isp_prt(isp, ISP_LOGCONFIG, prom, bus,
			    lp->portid, lp->handle,
		    	    roles[lp->roles], "stayed",
		    	    (uint32_t) (lp->port_wwn >> 32),
			    (uint32_t) lp->port_wwn);
		}
		break;
	case ISPASYNC_DEV_GONE:
		va_start(ap, cmd);
		bus = va_arg(ap, int);
		lp = va_arg(ap, fcportdb_t *);
		va_end(ap);
		/*
		 * If this has a virtual target and we haven't marked it
		 * that we're going to have isp_gdt tell the OS it's gone,
		 * set the isp_gdt timer running on it.
		 *
		 * If it isn't marked that isp_gdt is going to get rid of it,
		 * announce that it's gone.
		 */
		if (lp->dev_map_idx && lp->reserved == 0 && bus == 0) {
			lp->reserved = 1;
			lp->state = FC_PORTDB_STATE_ZOMBIE;
			lp->new_reserved = ddi_get_lbolt();
#if	0
			if (ISP_FC_PC(isp, bus)->gdt_running == 0) {
				isp_prt(isp, ISP_LOGSANCFG|ISP_LOGDEBUG0,
				    "Chan %d starting Gone Device Timer", bus);
				ISP_FC_PC(isp, bus)->gdt_running = 1;
				callout_reset(&ISP_FC_PC(isp, bus)->gdt, hz,
				    isp_gdt, ISP_FC_PC(isp, bus));
			}
#endif
			tgt = lp->dev_map_idx - 1;
			isp_prt(isp, ISP_LOGCONFIG, prom2, bus,
			    lp->portid, lp->handle,
		            roles[lp->roles], "gone zombie at", tgt,
		    	    (uint32_t) (lp->port_wwn >> 32),
			    (uint32_t) lp->port_wwn);
			isp->isp_osinfo.ggp[tgt].going = 1;
			isp->isp_osinfo.dchange_needed = 1;
		} else if (lp->reserved == 0) {
			isp_prt(isp, ISP_LOGCONFIG, prom, bus,
			    lp->portid, lp->handle,
			    roles[lp->roles], "departed",
			    (uint32_t) (lp->port_wwn >> 32),
			    (uint32_t) lp->port_wwn);
		}
		break;
	case ISPASYNC_CHANGE_NOTIFY:
	{
		char *msg;
		int subcmd;

		va_start(ap, cmd);
		bus = va_arg(ap, int);
		subcmd = va_arg(ap, int);
		va_end(ap);
		if (subcmd == ISPASYNC_CHANGE_PDB) {
			msg = "Port Database Changed";
		} else if (subcmd == ISPASYNC_CHANGE_SNS) {
			msg = "Name Server Database Changed";
		} else {
			msg = "Other Change Notify";
		}
		isp_prt(isp, ISP_LOGINFO, msg);
		isp->isp_osinfo.scan_needed = 1;
		break;
	}
	case ISPASYNC_FW_CRASH:
	{
		uint16_t mbox1, mbox6;
		mbox1 = ISP_READ(isp, OUTMAILBOX1);
		if (IS_DUALBUS(isp)) {
			mbox6 = ISP_READ(isp, OUTMAILBOX6);
		} else {
			mbox6 = 0;
		}
		isp_prt(isp, ISP_LOGERR, "Internal F/W Error on bus %d @ RISC Address 0x%x", mbox6, mbox1);
		isp_reinit(isp);
		isp_async(isp, ISPASYNC_FW_RESTARTED, NULL);
		break;
	}
	case ISPASYNC_FW_RESTARTED:
	default:
		break;
	}
}

void
isp_wdog(void *arg)
{
	XS_T           *xs = arg;
	ispsoftc_t     *isp = XS_ISP(xs);
	uint32_t        handle;

	ISP_LOCK(isp);
	/*
	 * We've decided this command is dead. Make sure we're not trying to
	 * kill a command that's already dead by getting it's handle and and
	 * seeing whether it's still alive.
	 */
	handle = isp_find_handle(isp, xs);
	if (handle) {
		uint32_t        isr;
		uint16_t        sema, mbox;

		if (XS_CMD_DONE_P(xs)) {
			isp_prt(isp, ISP_LOGDEBUG0, "watchdog found done cmd (handle 0x%x)", handle);
			ISP_UNLOCK(isp);
			return;
		}
		if (XS_CMD_WDOG_P(xs)) {
			isp_prt(isp, ISP_LOGDEBUG0, "recursive watchdog (handle 0x%x)", handle);
			ISP_UNLOCK(isp);
			return;
		}
		XS_CMD_S_WDOG(xs);

		if (ISP_READ_ISR(isp, &isr, &sema, &mbox)) {
			isp_intr(isp, isr, sema, mbox);
			if (XS_CMD_DONE_P(xs)) {
				isp_prt(isp, ISP_LOGWARN, "watchdog cleanup on handle 0x%x", handle);
				XS_CMD_C_WDOG(xs);
				isp_done(xs);
			}
		} else if (XS_CMD_GRACE_P(xs)) {
			isp_prt(isp, ISP_LOGERR, "command timeout on handle 0x%x (cmd 0x%x target %d lun %d)", handle, XS_CDBP(xs)[0], XS_TGT(xs), XS_LUN(xs));
			/*
			 * Make sure the command is *really* dead before we
			 * release the handle (and DMA resources) for reuse.
			 */
			(void) isp_control(isp, ISPCTL_ABORT_CMD, arg);

			/*
			 * After this point, the comamnd is really dead.
			 */
			if (XS_XFRLEN(xs)) {
				ISP_DMAFREE(isp, xs, handle);
			}
			isp_destroy_handle(isp, handle);
			XS_SETERR(xs, HBA_CMDTIMEOUT);
			XS_CMD_S_CLEAR(xs);
			isp_done(xs);
		} else {
			uint32_t        nxti, optr;
			isp_marker_t    local, *mp = &local, *qe;

			isp_prt(isp, ISP_LOGWARN, "possible command timeout on handle 0x%x (cmd 0x%x target %d lun %d)", handle, XS_CDBP(xs)[0], XS_TGT(xs), XS_LUN(xs));

			XS_CMD_C_WDOG(xs);
			GOOSE_TIMER(PKT2CMD(xs));
			if (isp_getrqentry(isp, &nxti, &optr, (void **) &qe)) {
				ISP_UNLOCK(isp);
				return;
			}
			XS_CMD_S_GRACE(xs);
			ISP_MEMZERO((void *) mp, sizeof(*mp));
			mp->mrk_header.rqs_entry_count = 1;
			mp->mrk_header.rqs_entry_type = RQSTYPE_MARKER;
			mp->mrk_modifier = SYNC_ALL;
			mp->mrk_target = XS_CHANNEL(xs) << 7;
			isp_put_marker(isp, mp, qe);
			ISP_ADD_REQUEST(isp, nxti);
		}
	} else if (isp->isp_dblev) {
		isp_prt(isp, ISP_LOGDEBUG0, "watchdog with no command");
	}
	ISP_UNLOCK(isp);
}

extern char    *vsprintf_len(int, char *, const char *, va_list);

char *
isp_snprintf(char *buf, size_t len, const char *fmt,...)
{
	char           *rv;
	va_list         ap;
	if (buf == NULL || len == 0) {
		return (NULL);
	}
	va_start(ap, fmt);
	rv = vsprintf_len((int) len, buf, fmt, ap);
	va_end(ap);
	return (rv);
}

int
isp_mbox_acquire(ispsoftc_t * isp)
{
	if (sema_tryp(&isp->isp_mbox_sema) == 0) {
isp_prt(isp, ISP_LOGALL, "FAILED TO ACQUIRE REGS");
		return (1);
	} else {
		return (0);
	}
}


void
isp_mbox_wait_complete(ispsoftc_t *isp, mbreg_t *mbp)
{
	unsigned int usecs = mbp->timeout;
	unsigned int max, olim, ilim;
	int polled;

	if (usecs == 0) {
		usecs = MBCMD_DEFAULT_TIMEOUT;
	}
	max = isp->isp_mbxwrk0 + 1;
	isp->isp_osinfo.isp_mbox_cmd_done = 0;
	polled = (isp->isp_osinfo.isp_ints_ok == 0 || isp->isp_osinfo.isp_onintstack);

	if (polled == 0) {
		for (olim = 0; olim < max; olim++) {
			unsigned int hzd = ddi_get_lbolt() + drv_usectohz(usecs);
			isp->isp_osinfo.isp_mbox_waiting = 1;
			if (cv_timedwait(&isp->isp_mbox_cv, &isp->isp_lock, hzd) < 0) {
				if (olim == max) {
					isp->isp_osinfo.isp_mbox_waiting = 0;
					break;
				}
			}
			isp->isp_osinfo.isp_mbox_waiting = 0;
			if (isp->isp_osinfo.isp_mbox_cmd_done) {
				break;
			}
		}
	} else {
		for (olim = 0; olim < max; olim++) {
			for (ilim = 0; ilim < usecs; ilim += 100) {
				uint32_t isr;
				uint16_t sema, mbox;
				if (isp->isp_osinfo.isp_mbox_cmd_done) {
					break;
				}
				if (ISP_READ_ISR(isp, &isr, &sema, &mbox)) {
					isp_intr(isp, isr, sema, mbox);
					if (isp->isp_osinfo.isp_mbox_cmd_done) {
						break;
					}
				}
				ISP_DELAY(100);
			}
			if (isp->isp_osinfo.isp_mbox_cmd_done) {
				break;
			}
		}
	}
	if (isp->isp_osinfo.isp_mbox_cmd_done == 0) {
		isp_prt(isp, ISP_LOGWARN, "%s Mailbox Command (0x%x) Timeout (%uus)",
		    polled? "Polled" : "Interrupting", isp->isp_lastmbxcmd, usecs);
		mbp->param[0] = MBOX_TIMEOUT;
		isp->isp_osinfo.isp_mbox_cmd_done = 1;
	}
}

static const char vname[] = MODNAME;
void
isp_prt(ispsoftc_t * isp, int level, const char *fmt,...)
{
	va_list	ap;
	char	local[256], *schar;

	if (level != ISP_LOGALL && (level & isp->isp_dblev) == 0) {
		if ((level & ISP_LOG_BLOWTHROUGH) == 0) {
			return;
		}
	}
	switch (level) {
	case ISP_LOG_BLOWTHROUGH | CE_PANIC:
		level = CE_PANIC;
		schar = "%s,%d: %s";
		break;
	case ISP_LOGINFO:
	case ISP_LOGCONFIG:
		level = CE_CONT;
		schar = "?%s,%d: %s\n";
		break;
	case ISP_LOG_BLOWTHROUGH | CE_NOTE:
	case ISP_LOGWARN:
		level = CE_NOTE;
		schar = "?%s,%d: %s";
		break;
	case ISP_LOG_BLOWTHROUGH | CE_WARN:
	case ISP_LOGERR:
		level = CE_WARN;
		schar = "%s,%d: %s";
		break;
	case ISP_LOG_BLOWTHROUGH | CE_CONT:
	case ISP_LOGALL:
	case ISP_LOGDEBUG0:
	case ISP_LOGDEBUG1:
	case ISP_LOGDEBUG2:
	case ISP_LOGDEBUG3:
	case ISP_LOGTDEBUG0:
	case ISP_LOGTDEBUG1:
	case ISP_LOGTDEBUG2:
	default:
		level = CE_CONT;
		schar = "%s,%d: %s\n";
		break;
	}
	va_start(ap, fmt);
	(void) vsprintf_len(sizeof(local), local, fmt, ap);
	va_end(ap);
	cmn_err(level, schar, vname, isp->isp_unit, local);
}
