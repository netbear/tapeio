/* $Id: isp_cb_ops.c,v 1.30 2009/02/14 00:09:02 mjacob Exp $ */
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
 * Solaris open/ioctl/ioctl routines for Qlogic FC/SCSI/IP Host Adapter Driver
 */
#include "isp_solaris.h"
#include "isp_ioctl.h"

#include <sys/file.h>
#include <sys/fc4/fcio.h>

static int isp_open(dev_t *, int, int, cred_t *);
static int isp_close(dev_t, int, int, cred_t *);
static int isp_ioctl(dev_t, int, intptr_t, int, cred_t *, int *);

struct cb_ops isp_cb_ops = {
	isp_open,			/* open */
	isp_close,			/* close */
	nodev,				/* strategy */
	nodev,				/* print */
	nodev,				/* dump */
	nodev,				/* read */
	nodev,				/* write */
	isp_ioctl,			/* ioctl */
	nodev,				/* devmap */
	nodev,				/* mmap */
	nodev,				/* segmap */
	nochpoll,			/* poll */
	ddi_prop_op,			/* cb_prop_op */
	NULL,				/* streamtab  */
	D_MP | D_NEW | D_HOTPLUG,	/* Driver compatibility flag */
	CB_REV,				/* revision */
	nodev,				/* aread */
	nodev				/* awrite */
};

static int
isp_open(dev_t *dev_p, int flags, int otyp, cred_t *cred_p)
{
	ispsoftc_t *isp;
	UNUSED_PARAMETER(cred_p);
	if (otyp != OTYP_CHR) {
		return(EINVAL);
	}
	isp = ddi_get_soft_state(isp_softc_state, getminor(*dev_p));
	if (isp == (ispsoftc_t *) NULL) {
		return(ENXIO);
	}
	ISP_LOCK(isp);
	if ((flags & FEXCL) && isp->isp_open) {
		ISP_UNLOCK(isp);
		return(EBUSY);
	}
	isp->isp_open = 1;
	ISP_UNLOCK(isp);
	return(0);
}

static int
isp_close(dev_t dev, int flag, int otyp, cred_t *cred_p)
{
	ispsoftc_t *isp;
	UNUSED_PARAMETER(cred_p);
	UNUSED_PARAMETER(flag);
	if (otyp != OTYP_CHR)
		return(EINVAL);
	isp = ddi_get_soft_state(isp_softc_state, getminor(dev));
	if (isp == (ispsoftc_t *)NULL) {
		return(ENXIO);
	}
	ISP_LOCK(isp);
	isp->isp_open = 0;
	ISP_UNLOCK(isp);
	return(0);
}

#define	GET_ROLE(isp)	IS_FC(isp)? FCPARAM(isp, 0)->role : SDPARAM(isp, 0)->role
#define	SET_ROLE(isp, new_role)	\
	if (IS_FC(isp)) FCPARAM(isp, 0)->role = new_role; else SDPARAM(isp, 0)->role = new_role

static int
isp_ioctl(dev_t dev, int cmd, intptr_t arg, int mode, cred_t *cred_p, int *rp)
{
	int i, rv, inarg, outarg, chan;
	ispsoftc_t *isp;
	struct devctl_iocdata *dcp;
	dev_info_t *dip, *cdip;
	uint_t bus_state;
	fcparam *fcp;

	UNUSED_PARAMETER(cred_p);
	UNUSED_PARAMETER(rp);

	isp = ddi_get_soft_state(isp_softc_state, getminor(dev));
	if (isp == (ispsoftc_t *)NULL) {
		return(ENXIO);
	}

	switch (cmd) {
	case DEVCTL_DEVICE_GETSTATE:
	case DEVCTL_DEVICE_ONLINE:
	case DEVCTL_DEVICE_OFFLINE:
	case DEVCTL_BUS_GETSTATE:
		return (ndi_devctl_ioctl(isp->isp_dip, cmd, arg, mode, 0));

	case DEVCTL_DEVICE_RESET:
	case DEVCTL_BUS_QUIESCE:
	case DEVCTL_BUS_UNQUIESCE:
	case DEVCTL_BUS_RESET:
	case DEVCTL_BUS_RESETALL:
		if (ndi_dc_allochdl((void *)arg, &dcp) != NDI_SUCCESS) {
			return(EFAULT);
		}
		dip = isp->isp_dip;
		break;
	default:
		dip = NULL;
		dcp = NULL;
		break;
	}

	if (IS_SCSI(isp)) {
		switch (cmd) {
		case ISP_SDBLEV:
		case ISP_RESCAN:
		case ISP_RESETHBA:
		case ISP_GETROLE:
		case ISP_SETROLE:
		case ISP_GET_STATS:
		case ISP_CLR_STATS:
			break;
		default:
			if (dcp == NULL) {
				return(EINVAL);
			}
			break;
		}
		fcp = NULL;
	} else {
		fcp = isp->isp_param;
	}
	rv = 0;

	switch (cmd) {
	case ISP_GET_STATS:
	{
		isp_stats_t stats;

		ISP_MEMZERO(&stats, sizeof stats);
		stats.isp_stat_version = ISP_STATS_VERSION;
		stats.isp_type = isp->isp_type;
		stats.isp_revision = isp->isp_revision;
		ISP_LOCK(isp);
		stats.isp_stats[ISP_INTCNT] = isp->isp_intcnt;
		stats.isp_stats[ISP_INTBOGUS] = isp->isp_intbogus;
		stats.isp_stats[ISP_INTMBOXC] = isp->isp_intmboxc;
		stats.isp_stats[ISP_INGOASYNC] = isp->isp_intoasync;
		stats.isp_stats[ISP_RSLTCCMPLT] = isp->isp_rsltccmplt;
		stats.isp_stats[ISP_FPHCCMCPLT] = isp->isp_fphccmplt;
		stats.isp_stats[ISP_RSCCHIWAT] = isp->isp_rscchiwater;
		stats.isp_stats[ISP_FPCCHIWAT] = isp->isp_fpcchiwater;
		ISP_UNLOCK(isp);
		if (ddi_copyout(&stats, (void *)arg, sizeof (stats), mode)) {
			rv = EFAULT;
		}
		break;
	}
	case ISP_CLR_STATS:
		ISP_LOCK(isp);
		isp->isp_intcnt = 0;
		isp->isp_intbogus = 0;
		isp->isp_intmboxc = 0;
		isp->isp_intoasync = 0;
		isp->isp_rsltccmplt = 0;
		isp->isp_fphccmplt = 0;
		isp->isp_rscchiwater = 0;
		isp->isp_fpcchiwater = 0;
		ISP_UNLOCK(isp);
		break;

	case ISP_SDBLEV:
		if (ddi_copyin((void *)arg, &inarg, sizeof (inarg), mode)) {
			rv = EFAULT;
			break;
		}
		outarg = isp->isp_dblev;
		isp->isp_dblev = inarg;
		if (ddi_copyout(&outarg, (void *)arg, sizeof (outarg), mode)) {
			rv = EFAULT;
			break;
		}
		break;

	case ISP_RESCAN:
		ISP_LOCK(isp);
		(void) isp_fc_runstate(isp, 0, 25000);
		ISP_UNLOCK(isp);
		break;
	case ISP_GETROLE:
		if (ddi_copyin((void *)arg, &inarg, sizeof (inarg), mode))  {
			rv = EFAULT;
			break;
		}
		chan = inarg >> 16;
		if (chan >= isp->isp_nchan) {
			rv = ENXIO;
			break;
		}
		if (IS_FC(isp)) {
			outarg = FCPARAM(isp, chan)->role;
		} else {
			outarg = SDPARAM(isp, chan)->role;
		}
		if (ddi_copyout(&outarg, (void *)arg, sizeof (outarg), mode)) {
			rv = EFAULT;
			break;
		}
		break;

	case ISP_SETROLE:
		if (ddi_copyin((void *)arg, &inarg, sizeof (inarg), mode)) {
			rv = EFAULT;
			break;
		}
		chan = inarg >> 16;
		if (chan >= isp->isp_nchan) {
			rv = ENXIO;
			break;
		}
		inarg &= 0xffff;
		if (inarg & ~(ISP_ROLE_INITIATOR|ISP_ROLE_TARGET)) {
			rv = EINVAL;
			break;
		}
		/*
		 * Check to see if we're already in that role.
		 */
		if (IS_FC(isp)) {
			if (FCPARAM(isp, chan)->role == inarg) {
				break;
			}
			outarg = FCPARAM(isp, chan)->role;
			if (ddi_copyout(&outarg, (void *)arg, sizeof (outarg),
			    mode)) {
				rv = EFAULT;
				break;
			}
			FCPARAM(isp, chan)->role = inarg;
			SET_DEFAULT_ROLE(isp, chan, FCPARAM(isp, chan)->role);
		} else {
			if (SDPARAM(isp, chan)->role == inarg) {
				break;
			}
			outarg = SDPARAM(isp, chan)->role;
			if (ddi_copyout(&outarg, (void *)arg, sizeof (outarg),
			    mode)) {
				rv = EFAULT;
				break;
			}
			SDPARAM(isp, chan)->role = inarg;
			SET_DEFAULT_ROLE(isp, chan, SDPARAM(isp, chan)->role);
		}
		break;
	case ISP_RESETHBA:
	case DEVCTL_BUS_RESET:
	case DEVCTL_BUS_RESETALL:
		ISP_LOCK(isp);
		if (cmd == ISP_SETROLE) {
			outarg = GET_ROLE(isp);
			SET_ROLE(isp, inarg);
		}
		if (isp_drain_reset(isp, "isp_ioctl")) {
			ISP_UNLOCK(isp);
			rv = EIO;
			break;
		}
		SCHED_KICK(isp);
		ISP_UNLOCK(isp);
		if (cmd == ISP_SETROLE && ddi_copyout(&outarg, (void *)arg, sizeof (outarg), mode)) {
			rv = EFAULT;
			break;
		}
		break;

	case ISP_FC_LIP:
		ISP_LOCK(isp);
		if (isp_control(isp, ISPCTL_SEND_LIP, 0)) {
			rv = EIO;
		}
		ISP_UNLOCK(isp);
		break;

	case ISP_FC_GETDINFO:
	{
		struct isp_fc_device local, *ifc = &local;
		fcportdb_t *lp;

		if (ddi_copyin((void *)arg, ifc, sizeof (*ifc), mode)) {
			rv = EFAULT;
			break;
		}
		if (ifc->loopid < 0 || ifc->loopid >= MAX_FC_TARG) {
			rv = EINVAL;
			break;
		}
		ISP_LOCK(isp);
		lp = &FCPARAM(isp, 0)->portdb[ifc->loopid];
		if (lp->state == FC_PORTDB_STATE_VALID) {
			ifc->role = lp->roles;
			ifc->loopid = lp->handle;
			ifc->portid = lp->portid;
			ifc->node_wwn = lp->node_wwn;
			ifc->port_wwn = lp->port_wwn;
			rv = 0;
		} else {
			rv = ENODEV;
		}
		ISP_UNLOCK(isp);
		if (rv == 0) {
			if (ddi_copyout((void *)ifc, (void *)arg, sizeof (*ifc), mode)) {
				rv = EFAULT;
			}
		}
		break;
	}
	case ISP_FC_GETHINFO:
	{
		struct isp_hba_device local, *hba = &local;
		ISP_MEMZERO(hba, sizeof (*hba));
		ISP_LOCK(isp);
		hba->fc_fw_major = ISP_FW_MAJORX(isp->isp_fwrev);
		hba->fc_fw_minor = ISP_FW_MINORX(isp->isp_fwrev);
		hba->fc_fw_micro = ISP_FW_MICROX(isp->isp_fwrev);
		hba->fc_speed = FCPARAM(isp, 0)->isp_gbspeed;
		hba->fc_topology = FCPARAM(isp, 0)->isp_topo + 1;
		hba->fc_loopid = FCPARAM(isp, 0)->isp_loopid;
		hba->nvram_node_wwn = FCPARAM(isp, 0)->isp_wwnn_nvram;
		hba->nvram_port_wwn = FCPARAM(isp, 0)->isp_wwpn_nvram;
		hba->active_node_wwn = ACTIVE_NODEWWN(isp, 0);
		hba->active_port_wwn = ACTIVE_PORTWWN(isp, 0);
		ISP_UNLOCK(isp);
		if (ddi_copyout(hba, (void *)arg, sizeof (*hba), mode)) {
			rv = EFAULT;
			break;
		}
		break;
	}
	case DEVCTL_DEVICE_RESET:
		if (ndi_dc_getname(dcp) == NULL || ndi_dc_getaddr(dcp) == NULL) {
			rv = EINVAL;
			break;
		}

		/* XXXX */
		break;


	case DEVCTL_BUS_QUIESCE:
		if (ndi_get_bus_state(dip, &bus_state) == NDI_SUCCESS) {
			if (bus_state == BUS_QUIESCED) {
				break;
			}
		}

		ISP_LOCK(isp);
		/*
		 * Drain active commands.
		 */
		if (isp_drain(isp, "DEVCTL_BUS_QUIESCE")) {
			ISP_UNLOCK(isp);
			rv = EIO;
			break;
		}
		isp->isp_blocked = 1;
		ISP_UNLOCK(isp);
		(void) ndi_set_bus_state(dip, BUS_QUIESCED);
		break;

	case DEVCTL_BUS_UNQUIESCE:
		if (ndi_get_bus_state(dip, &bus_state) == NDI_SUCCESS) {
			if (bus_state == BUS_ACTIVE) {
				break;
			}
		}
		ISP_LOCK(isp);
		isp->isp_blocked = 0;
		SCHED_KICK(isp);
		ISP_UNLOCK(isp);
		(void) ndi_set_bus_state(dip, BUS_ACTIVE);
		break;

	default:
		rv = ENOTTY;
		break;
	}
	if (dcp) {
		ndi_dc_freehdl(dcp);
	}
	return(rv);
}
