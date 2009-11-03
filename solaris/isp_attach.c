/* $Id: isp_attach.c,v 1.53 2008/06/16 23:54:25 mjacob Exp $ */
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
 * Configure/Attach/Detach routines for Qlogic FC/SCSI/IP Host Adapter Driver
 */

#include "isp_solaris.h"
#include "isp_ioctl.h"

/*
 * Local defines...
 */
struct isp_storage {
	ispsoftc_t 	a;
	struct ispmdvec	b;
};
#define	ISP_ALLOC_SIZE	(sizeof (struct isp_storage))

/*
 * External References..
 */
extern struct cb_ops isp_cb_ops;

/*
 * Global driver data
 */
void *isp_softc_state = NULL;

int isp_maxlun = 1;

/*
 * Local prototypes
 */
static int isp_info(dev_info_t *, ddi_info_cmd_t, void *, void **);
static int isp_attach(dev_info_t *, ddi_attach_cmd_t);
static uint_t isp_intr_wrap(caddr_t);
static int isp_detach(dev_info_t *, ddi_detach_cmd_t);
static void isp_unattach(ispsoftc_t *);

/*
 * Local static data
 */
static struct dev_ops isp_ops = {
	DEVO_REV,		/* devo_rev, */
	0,			/* refcnt  */
	isp_info,		/* info */
	nulldev,		/* identify */
	nulldev,		/* probe */
	isp_attach,		/* attach */
	isp_detach,		/* detach */
	nodev,			/* reset */
	&isp_cb_ops,		/* driver operations */
	NULL,			/* bus operations */
	ddi_power		/* power management */
};

#ifndef	HBA_NAME_STRING
#define	HBA_NAME_STRING "Feral Fibre/SCSI Qlogic Driver"
#endif
static struct modldrv modldrv = {
	&mod_driverops,
	HBA_NAME_STRING,
	&isp_ops,	/* driver ops */
};

static struct modlinkage modlinkage = {
	MODREV_1, &modldrv, NULL
};
static int isp_logdebug = 0;
static int isp_roles = ISP_DEFAULT_ROLES;
static int isp_ignore_nvram = 0;

/*
 * Attach/Detach functions
 */

int
_init(void)
{
	int ret;

	ret = ddi_soft_state_init(&isp_softc_state, ISP_ALLOC_SIZE, 1);
	if (ret != 0) {
		cmn_err(CE_WARN, "?Cannot Init Soft State for Qlogic ISP driver");
		return (ret);
	}

	if ((ret = scsi_hba_init(&modlinkage)) != 0) {
		ddi_soft_state_fini(&isp_softc_state);
		return (ret);
	}

	ret = mod_install(&modlinkage);
	if (ret != 0) {
		scsi_hba_fini(&modlinkage);
		ddi_soft_state_fini(&isp_softc_state);
	}
	return (ret);
}

/*
 * nexus drivers are currently not unloaded so this routine is really redundant
 */
int
_fini(void)
{
	int ret;
	if ((ret = mod_remove(&modlinkage)) != 0) {
		return (ret);
	}
	scsi_hba_fini(&modlinkage);
	ddi_soft_state_fini(&isp_softc_state);
	return (ret);
}

int
_info(struct modinfo *modinfop)
{
	return (mod_info(&modlinkage, modinfop));
}


/*ARGSUSED*/
static int
isp_info(dev_info_t *dip, ddi_info_cmd_t infocmd, void *arg, void **result)
{
	int instance = getminor((dev_t)arg);
	ispsoftc_t *isp;

	switch (infocmd) {
	case DDI_INFO_DEVT2DEVINFO:
		isp = ddi_get_soft_state(&isp_softc_state, instance);
		if (isp != NULL)
			*result = isp->isp_dip;
		else {
			*result = NULL;
			return (DDI_FAILURE);
		}
		break;

	case DDI_INFO_DEVT2INSTANCE:
		*result = (void *)(uintptr_t)instance;
		break;
	default:
		return (DDI_FAILURE);
	}
	return (DDI_SUCCESS);
}

/*
 * Attach isp host adapter.  Allocate data structures and link
 * to isp_head list.  Initialize the isp and we're
 * on the air.
 */

#define	_BRG	_BLK_REG_SHFT

int
isp_attach(dev_info_t *dip, ddi_attach_cmd_t cmd)
{
	static const char *nomem = "memory allocation failure during attach";
	ispsoftc_t *isp;
	scsi_hba_tran_t *tran = NULL;
	ddi_device_acc_attr_t dev_attr;
	int inst, wwnlen, idpromlen, tmp;
	uint32_t dtype;
	char propbuf[32];
	uint8_t lsz, rev;
	off_t regsize;
	
	switch (cmd) {
	case DDI_ATTACH:
		dev_attr.devacc_attr_version = DDI_DEVICE_ATTR_V0;
		dev_attr.devacc_attr_dataorder = DDI_STRICTORDER_ACC;
		break;

	case DDI_PM_RESUME:
	case DDI_RESUME:
		tran = (scsi_hba_tran_t *)ddi_get_driver_private(dip);
		if (!tran) {
			return (DDI_FAILURE);
		}
		isp = TRAN2ISP(tran);
		if (!isp) {
			return (DDI_FAILURE);
		}

		/*
		 * reset isp and bus
		 */
		ISP_LOCK(isp);
		isp_reset(isp);
		if (isp->isp_state != ISP_RESETSTATE) {
			ISP_UNLOCK(isp);
			return (DDI_FAILURE);
		}
		isp_init(isp);
		if (isp->isp_state != ISP_INITSTATE) {
			ISP_UNLOCK(isp);
			return (DDI_FAILURE);
		}
		isp->isp_state = ISP_RUNSTATE;
		ISP_UNLOCK(isp);
		ISP_RESET_NOTIFY(isp);
		isp->isp_osinfo.isp_suspended = 0;
		if (isp->isp_osinfo.tq) {
			isp->isp_osinfo.task_kick = timeout(isp_tkick, isp, drv_usectohz(30000));
			ddi_taskq_resume(isp->isp_osinfo.tq);
		}
		SCHED_KICK(isp);
		return (DDI_SUCCESS);

	default:
		return (DDI_FAILURE);
	}

	if (ddi_slaveonly(dip) == DDI_SUCCESS) {
		return (DDI_FAILURE);
	}
	if (ddi_intr_hilevel(dip, 0)) {
		return (DDI_FAILURE);
	}

	/*
	 * Allocate softc and initialize some fields.
	 */
	inst = ddi_get_instance(dip);
	if (ddi_soft_state_zalloc(isp_softc_state, inst) != DDI_SUCCESS) {
		cmn_err(CE_WARN, MODNAME ",%d: Failed to alloc soft state", inst);
		return (DDI_FAILURE);
	}

	isp = (ispsoftc_t *)ddi_get_soft_state(isp_softc_state, inst);
	if (isp == (ispsoftc_t *)NULL) {
		cmn_err(CE_WARN, MODNAME "%d: bad soft state", inst);
		ddi_soft_state_free(isp_softc_state, inst);
		return (DDI_FAILURE);
	}
	(void) sprintf(propbuf, "isp%d_cache", inst);
	isp->isp_osinfo.isp_mcache =
	    kmem_cache_create(propbuf, sizeof (isp_cmd_t), 8, isp_kmem_cache_constructor, isp_kmem_cache_destructor, NULL, (void *)isp, NULL, 0);
	if (isp->isp_osinfo.isp_mcache == NULL) {
		ddi_soft_state_free(isp_softc_state, inst);
		cmn_err(CE_WARN, MODNAME "%d: unable to create kmem cache", inst);
		return (DDI_FAILURE);
	}
	
	isp->isp_dip = dip;
	isp->isp_name = ddi_get_name(dip);
	isp->isp_unit = inst;

	tmp = ddi_prop_get_int(DDI_DEV_T_ANY, dip, 0, "isp_maxlun", isp_maxlun);
	if (tmp < 1 || tmp > 256) {
		cmn_err(CE_WARN, "bad isp_maxlun parameter (%d)", tmp);
	} else {
		isp_maxlun = tmp;
	}
	isp_logdebug = ddi_prop_get_int(DDI_DEV_T_ANY, dip, 0, "isp_logdebug", isp_logdebug);
	if (isp_logdebug) {
		isp->isp_dblev = isp_logdebug;
	} else {
		isp->isp_dblev = ISP_LOGCONFIG|ISP_LOGINFO|ISP_LOGWARN|ISP_LOGERR;
	}
	isp_ignore_nvram = ddi_prop_get_int(DDI_DEV_T_ANY, dip, 0, "isp_ignore_nvram", isp_ignore_nvram);

	isp->isp_dma_attr.dma_attr_version = DMA_ATTR_V0;
	isp->isp_dma_attr.dma_attr_addr_hi = (uint64_t) 0xffffffff;
	isp->isp_dma_attr.dma_attr_count_max = (uint64_t) 0xffffffff;
	isp->isp_dma_attr.dma_attr_align = 1;
	isp->isp_dma_attr.dma_attr_burstsizes = 0xff;
	isp->isp_dma_attr.dma_attr_minxfer = 1;
	isp->isp_dma_attr.dma_attr_maxxfer = (uint64_t) 0xffffffff;
	isp->isp_dma_attr.dma_attr_seg = (uint64_t) DMA_ATTR_SEG;
	isp->isp_dma_attr.dma_attr_granular = (uint64_t) DMA_ATTR_GRAN;
	isp->isp_mdvec = &((struct isp_storage *)isp)->b;
	isp->isp_mdvec->dv_rd_isr = isp_rd_isr;
	isp->isp_mdvec->dv_rd_reg = isp_rd_reg;
	isp->isp_mdvec->dv_wr_reg = isp_wr_reg;
	isp->isp_mdvec->dv_mbxdma = isp_mbxdma_setup;
	isp->isp_mdvec->dv_dmaset = isp_dma_setup;
	isp->isp_mdvec->dv_dregs = isp_dump_regs;
	isp->isp_mdvec->dv_reset0 = isp_reset0;
	isp->isp_mdvec->dv_reset1 = isp_reset1;
	lsz = 0;

	/*
	 * Check for SBus devices by name.
	 * PCI devices we check by Vendor
	 * and Device ID.
	 */
	if (strcmp(isp->isp_name, "isp") == 0 || strcmp(isp->isp_name, "QLGC,isp") == 0 || strcmp(isp->isp_name, "SUNW,isp") == 0) {
		uint32_t cfreq;
		isp->isp_bustype = ISP_BT_SBUS;
		dtype = SBUS_QLOGIC_ISP;
		rev = 0;
		regsize = SBUS_REGSIZE;
		dev_attr.devacc_attr_endian_flags = DDI_NEVERSWAP_ACC;
		cfreq = ddi_prop_get_int(DDI_DEV_T_ANY, dip, 0, "clock-frequency", -1);
		if (cfreq == (uint32_t) -1) {
			ddi_soft_state_free(isp_softc_state, inst);
			return (DDI_FAILURE);
		}
		cfreq = (cfreq + 500000)/1000000;
		isp->isp_mdvec->dv_clock = cfreq;
	} else {
		uint16_t vid;
		isp->isp_bustype = ISP_BT_PCI;
		if (pci_config_setup(dip, &isp->isp_phandle)) {
			ddi_soft_state_free(isp_softc_state, inst);
			return (DDI_FAILURE);
		}
		vid = pci_config_get16(isp->isp_phandle, PCI_CONF_VENID);
		if (vid != PCI_VENDOR_QLOGIC) {
			ddi_soft_state_free(isp_softc_state, inst);
			return (DDI_FAILURE);
		}
		dtype = pci_config_get16(isp->isp_phandle, PCI_CONF_DEVID);
		dtype <<= 16;
		dtype |= vid;
		rev = pci_config_get8(isp->isp_phandle, PCI_CONF_REVID);
		regsize = PCI_REGSIZE;
		dev_attr.devacc_attr_endian_flags = DDI_STRUCTURE_LE_ACC;
		lsz = PCI_DFLT_LNSZ;
		/*
		 * Clock is either 60 or 100. Let isp_reset decide.
		 */
	}
	isp_get_firmware(ISPFW_VERSION, 0, dtype, (const uint16_t **)&isp->isp_mdvec->dv_ispfw);
	isp->isp_regoff[BIU_BLOCK >> _BRG] = BIU_REGS_OFF;
	isp->isp_regoff[MBOX_BLOCK >> _BRG] = PCI_MBOX_REGS_OFF;
	isp->isp_regoff[SXP_BLOCK >> _BRG] = PCI_SXP_REGS_OFF;
	isp->isp_regoff[RISC_BLOCK >> _BRG] = PCI_RISC_REGS_OFF;
	isp->isp_regoff[DMA_BLOCK >> _BRG] = DMA_REGS_OFF;

	isp->isp_nchan = 1;
	switch (dtype) {
	case SBUS_QLOGIC_ISP:
		isp->isp_regoff[MBOX_BLOCK >> _BRG] = SBUS_MBOX_REGS_OFF;
		isp->isp_regoff[SXP_BLOCK >> _BRG] = SBUS_SXP_REGS_OFF;
		isp->isp_regoff[RISC_BLOCK >> _BRG] = SBUS_RISC_REGS_OFF;
		/* FALLTHROUGH */
	case PCI_QLOGIC_ISP:
		isp->isp_type = ISP_HA_SCSI_UNKNOWN;
		isp->isp_dma_attr.dma_attr_count_max = (uint64_t) 0x00ffffff;
		isp->isp_osinfo.pamt = sizeof (sdparam);
		break;
	case PCI_QLOGIC_ISP1080:
		isp->isp_type = ISP_HA_SCSI_1080;
		isp->isp_regoff[DMA_BLOCK >> _BRG] = ISP1080_DMA_REGS_OFF;
		isp->isp_mdvec->dv_rd_reg = isp_rd_reg_1080;
		isp->isp_mdvec->dv_wr_reg = isp_wr_reg_1080;
		isp->isp_osinfo.pamt = sizeof (sdparam);
		break;
	case PCI_QLOGIC_ISP1240:
		isp->isp_type = ISP_HA_SCSI_1240;
		isp->isp_regoff[DMA_BLOCK >> _BRG] = ISP1080_DMA_REGS_OFF;
		isp->isp_mdvec->dv_rd_reg = isp_rd_reg_1080;
		isp->isp_mdvec->dv_wr_reg = isp_wr_reg_1080;
		isp->isp_nchan++;
		isp->isp_osinfo.pamt = 2 * sizeof (sdparam);
		break;
	case PCI_QLOGIC_ISP1280:
		isp->isp_type = ISP_HA_SCSI_1280;
		isp->isp_regoff[DMA_BLOCK >> _BRG] = ISP1080_DMA_REGS_OFF;
		isp->isp_mdvec->dv_rd_reg = isp_rd_reg_1080;
		isp->isp_mdvec->dv_wr_reg = isp_wr_reg_1080;
		isp->isp_nchan++;
		isp->isp_osinfo.pamt = 2 * sizeof (sdparam);
		break;
	case PCI_QLOGIC_ISP10160:
		isp->isp_type = ISP_HA_SCSI_10160;
		isp->isp_regoff[DMA_BLOCK >> _BRG] = ISP1080_DMA_REGS_OFF;
		isp->isp_mdvec->dv_rd_reg = isp_rd_reg_1080;
		isp->isp_mdvec->dv_wr_reg = isp_wr_reg_1080;
		isp->isp_osinfo.pamt = sizeof (sdparam);
		break;
	case PCI_QLOGIC_ISP12160:
		isp->isp_type = ISP_HA_SCSI_12160;
		isp->isp_regoff[DMA_BLOCK >> _BRG] = ISP1080_DMA_REGS_OFF;
		isp->isp_mdvec->dv_rd_reg = isp_rd_reg_1080;
		isp->isp_mdvec->dv_wr_reg = isp_wr_reg_1080;
		isp->isp_nchan++;
		isp->isp_osinfo.pamt = 2 * sizeof (sdparam);
		break;
	case PCI_QLOGIC_ISP2100:
		isp->isp_type = ISP_HA_FC_2100;
		isp->isp_regoff[MBOX_BLOCK >> _BRG] = PCI_MBOX_REGS2100_OFF;
		if (rev < 3) {
			/*
			 * XXX: Need to get the actual revision
			 * XXX: number of the 2100 FB. At any rate,
			 * XXX: lower cache line size for early revision
			 * XXX; boards.
			 */
			lsz = 1;
		}
		isp->isp_osinfo.pamt = sizeof (fcparam);
		break;
	case PCI_QLOGIC_ISP2200:
		isp->isp_type = ISP_HA_FC_2200;
		isp->isp_regoff[MBOX_BLOCK >> _BRG] = PCI_MBOX_REGS2100_OFF;
		isp->isp_osinfo.pamt = sizeof (fcparam);
		break;
	case PCI_QLOGIC_ISP2300:
	case PCI_QLOGIC_ISP2312:
	case PCI_QLOGIC_ISP6312:
	case PCI_QLOGIC_ISP2322:
	case PCI_QLOGIC_ISP6322:
		if (dtype == PCI_QLOGIC_ISP2300) {
			isp->isp_type = ISP_HA_FC_2300;
		} else {
			pci_regspec_t *rp;
			int len, val;

			val = ddi_getlongprop(DDI_DEV_T_ANY, dip, DDI_PROP_CANSLEEP, "reg", (caddr_t)&rp, &len);
			if (val != DDI_SUCCESS) {
				isp_prt(isp, ISP_LOGWARN, "cannot determine which port I am");
			} else {
				isp->isp_port = PCI_REG_FUNC_G(rp->pci_phys_hi);
				kmem_free(rp, len);
			}
			isp->isp_type = ISP_HA_FC_2312;
		}
		isp->isp_regoff[MBOX_BLOCK >> _BRG] = PCI_MBOX_REGS2300_OFF;
		isp->isp_mdvec->dv_rd_isr = isp_rd_isr_2300;
		isp->isp_osinfo.pamt = sizeof (fcparam);
		break;
	case PCI_QLOGIC_ISP2422:
	case PCI_QLOGIC_ISP2432:

	default:
		isp_prt(isp, ISP_LOGWARN, "Unknown Qlogic Device 0x%x", dtype & 0xffff);
		ddi_soft_state_free(isp_softc_state, inst);
		return (DDI_FAILURE);
	}

	isp->isp_param = kmem_zalloc(isp->isp_osinfo.pamt, KM_SLEEP);
	if (isp->isp_param == NULL) {
		isp_prt(isp, ISP_LOGERR, nomem);
		ddi_soft_state_free(isp_softc_state, inst);
		return (DDI_FAILURE);
	}
	isp->isp_revision = rev;

	/*
	 * Do any config space cleanup that needs to happen (PCI only).
	 */
	if (isp->isp_bustype == ISP_BT_PCI) {
		uint32_t word;

		/*
		 * Make sure the Cache Line Size register is set sensibly.
		 */
		pci_config_put8(isp->isp_phandle, PCI_CONF_CACHE_LINESZ, lsz); 

		/*
		 * Make sure the Latency Timer is sane. This is crucial.
		 */
		lsz = pci_config_get8(isp->isp_phandle, PCI_CONF_LATENCY_TIMER);
		if (lsz < PCI_DFLT_LTNCY) {
			pci_config_put8(isp->isp_phandle, PCI_CONF_LATENCY_TIMER, PCI_DFLT_LTNCY);
		}

		/*
		 * Make sure we've set the command register.
		 */
		word =  pci_config_get16(isp->isp_phandle, PCI_CONF_COMM);
		word |= PCI_COMM_IO | PCI_COMM_MAE| PCI_COMM_ME | PCI_COMM_MEMWR_INVAL | PCI_COMM_PARITY_DETECT | PCI_COMM_SERR_ENABLE;
		if (IS_2300(isp)) {	/* Per QLogic errata */
			word &= ~PCI_COMM_MEMWR_INVAL;
		}
		pci_config_put16(isp->isp_phandle, PCI_CONF_COMM, word);
	}

	/*
	 * Get iblock coookie and setup interrupt.
	 */
	if (ddi_get_iblock_cookie(dip, 0, &isp->isp_iblock)) {
		isp_prt(isp, ISP_LOGERR, "cannot get iblock cookie");
		ddi_soft_state_free(isp_softc_state, inst);
		return (DDI_FAILURE);
	}
	/*
	 * Initialize the softc lock.
	 */
	mutex_init(ISP_LOCKP(isp), NULL, MUTEX_DRIVER, isp->isp_iblock);
	ISP_LOCK(isp);

	if (ddi_add_intr(dip, 0, &isp->isp_iblock, NULL, isp_intr_wrap, (caddr_t)isp)) {
		ISP_UNLOCK(isp);
		mutex_destroy(ISP_LOCKP(isp));
		isp_prt(isp, ISP_LOGERR, "Cannot add interrupt");
		ddi_soft_state_free(isp_softc_state, inst);
		return (DDI_FAILURE);
	}

	/*
	 * Most of the DMA setup is done later (as a callout from isp_reset),
	 * but we allocate the main dma handle here so we can set burstsizes
	 * correctly.
	 */
	isp->isp_dma_attr.dma_attr_sgllen = ISP_SGLLEN(isp);;
	if (isp->isp_dma_attr.dma_attr_sgllen > 1) {
		isp->isp_dma_attr.dma_attr_maxxfer = (isp->isp_dma_attr.dma_attr_sgllen - 1) * ddi_ptob(dip, 1);
	}

	if (ddi_dma_alloc_handle(dip, &isp->isp_dma_attr, DDI_DMA_SLEEP, NULL, &isp->isp_dma_handle) != DDI_SUCCESS) {
		ISP_DISABLE_INTS(isp);
		ISP_UNLOCK(isp);
		mutex_destroy(ISP_LOCKP(isp));
		isp_prt(isp, ISP_LOGERR, "cannot alloc dma handle");
		ddi_remove_intr(dip, 0, isp->isp_iblock);
		ddi_soft_state_free(isp_softc_state, inst);
		return (DDI_FAILURE);
	}

	/*
	 * Map in device registers- for PCI, the device registers are the
	 * *second* group of registers. After this point, we're committed to
	 * be here or do the full decommission.
	 */
	if (ddi_regs_map_setup(dip, (isp->isp_bustype == ISP_BT_PCI)? 1 : 0, &isp->isp_regs, 0, regsize, &dev_attr, &isp->isp_reg_acc_handle)) {
		isp_prt(isp, ISP_LOGERR, "Unable to map registers");
		ISP_DISABLE_INTS(isp);
		ISP_UNLOCK(isp);
		mutex_destroy(ISP_LOCKP(isp));
		ddi_dma_free_handle(&isp->isp_dma_handle);
		isp->isp_dma_handle = NULL;
		ddi_remove_intr(dip, 0, isp->isp_iblock);
		ddi_soft_state_free(isp_softc_state, inst);
		return (DDI_FAILURE);
	}
	isp->isp_dev_attr = dev_attr;

	/*
	 * Initialize the rest of the locks.
	 */
	sema_init(ISP_MBOX_SEMA(isp), 1, NULL, SEMA_DRIVER, NULL);
	cv_init(ISP_MBOX_CV(isp), NULL, CV_DRIVER, NULL);
	cv_init(ISP_DRAIN_CV(isp), NULL, CV_DRIVER, NULL);


	/*
	 * Find the system imposed burstsize limit and reduce ours if necessary.
	 * If no burst size found, select a reasonable default.
	 */
	isp->isp_dma_attr.dma_attr_burstsizes &= ddi_dma_burstsizes(isp->isp_dma_handle);
	if (isp->isp_bustype == ISP_BT_PCI) {
		uint_t btmp = isp->isp_dma_attr.dma_attr_burstsizes;
		isp->isp_mdvec->dv_conf1 = 0;
		if (btmp & (1 << 7)) {
			isp->isp_mdvec->dv_conf1 = BIU_PCI_CONF1_FIFO_128;
		} else if (btmp & (1 << 6)) {
			isp->isp_mdvec->dv_conf1 = BIU_PCI_CONF1_FIFO_64;
		} else if (btmp & (1 << 5)) {
			isp->isp_mdvec->dv_conf1 = BIU_PCI_CONF1_FIFO_32;
		} else if (btmp & (1 << 4)) {
			isp->isp_mdvec->dv_conf1 = BIU_PCI_CONF1_FIFO_16;
		}
	} else {
		uint_t btmp = isp->isp_dma_attr.dma_attr_burstsizes;
		isp->isp_mdvec->dv_conf1 = 0;

		/*
		 * We don't have 128 byte bursts for SBus.
		 */
		btmp &= ~(1 << 7);

		/*
		 * There have been problems with 64 byte bursts on an SS10.
		 */
		btmp &= ~(1 << 6);

		if (btmp & (1 << 5)) {
			isp->isp_mdvec->dv_conf1 = BIU_SBUS_CONF1_FIFO_32;
		} else if (btmp & (1 << 4)) {
			isp->isp_mdvec->dv_conf1 = BIU_SBUS_CONF1_FIFO_16;
		} else if (btmp & (1 << 3)) {
			isp->isp_mdvec->dv_conf1 = BIU_SBUS_CONF1_BURST8 | BIU_SBUS_CONF1_FIFO_8;
		}
	}

	if (isp->isp_mdvec->dv_conf1) {
		isp->isp_mdvec->dv_conf1 |= BIU_BURST_ENABLE;
		isp_prt(isp, ISP_LOGDEBUG2, "DMA burstmask 0x%x conf1 0x%x", isp->isp_dma_attr.dma_attr_burstsizes, isp->isp_mdvec->dv_conf1);
	}

	/*
	 * Set some default WWN properties.
	 */
	wwnlen = 16;
	idpromlen = 32;
	if (ddi_getlongprop_buf(DDI_DEV_T_ANY, dip, DDI_PROP_CANSLEEP, "port-wwn", propbuf, &wwnlen) == DDI_PROP_SUCCESS) {
		unsigned char *idl = (unsigned char *) &propbuf[0];
		isp->isp_osinfo.default_portwwn =
		    (((uint64_t) idl[0]) << 56) |
		    (((uint64_t) idl[1]) << 48) |
		    (((uint64_t) idl[2]) << 40) |
		    (((uint64_t) idl[3]) << 32) |
		    (((uint64_t) idl[4]) << 24) |
		    (((uint64_t) idl[5]) << 16) |
		    (((uint64_t) idl[6]) <<  8) |
		    (((uint64_t) idl[7]) <<  0);
	}

	if (ddi_getlongprop_buf(DDI_DEV_T_ANY, dip, DDI_PROP_CANSLEEP, "node-wwn", propbuf, &wwnlen) == DDI_PROP_SUCCESS) {
		unsigned char *idl = (unsigned char *) &propbuf[0];
		isp->isp_osinfo.default_nodewwn =
		    (((uint64_t) idl[0]) << 56) |
		    (((uint64_t) idl[1]) << 48) |
		    (((uint64_t) idl[2]) << 40) |
		    (((uint64_t) idl[3]) << 32) |
		    (((uint64_t) idl[4]) << 24) |
		    (((uint64_t) idl[5]) << 16) |
		    (((uint64_t) idl[6]) <<  8) |
		    (((uint64_t) idl[7]) <<  0);
	}

	if (IS_FC(isp)) {
		char buf[32];
		int loopid = ddi_getprop(DDI_DEV_T_ANY, dip, 0, "loop-id", -1);

		if (loopid >= 0 && loopid <= 126) {
			isp->isp_confopts |= ISP_CFG_OWNLOOPID;
			isp->isp_osinfo.default_id = loopid;
		}
		(void) snprintf(buf, sizeof (buf), "%s%d_tq", MODNAME, isp->isp_unit);
		isp->isp_osinfo.tq = ddi_taskq_create(dip,  buf, 4, TASKQ_DEFAULTPRI, 0);
		if (isp->isp_osinfo.tq == NULL) {
			ISP_UNLOCK(isp);
			isp_prt(isp, ISP_LOGERR, "unable to create taskq");
			goto failure;
		}
	} else {
		int iid;
		iid = ddi_getprop(DDI_DEV_T_ANY, dip, 0, "initiator-id", -1);
		if (iid == -1) {
			iid = ddi_getprop(DDI_DEV_T_ANY, dip, 0, "scsi-initiator-id", -1);
		}
		if (iid < 0 || iid > 15) {
			iid = 7;
		} else {
			isp->isp_confopts |= ISP_CFG_OWNLOOPID;
		}
		isp->isp_osinfo.default_id = iid;
	}
	isp->isp_osinfo.ggp = kmem_zalloc(sizeof (struct gg) * ISP_MAX_TARGETS(isp), KM_SLEEP);
	if (isp->isp_osinfo.ggp == NULL) {
		ISP_UNLOCK(isp);
		isp_prt(isp, ISP_LOGERR, "unable to alloct geom arrays");
		goto failure;
	}

	/*
	 * Because we don't/can't really set NVRAM options
	 * on SBus, don't read values from NVRAM.
	 */
	if (isp->isp_bustype == ISP_BT_SBUS || isp_ignore_nvram) {
		isp->isp_confopts |= ISP_CFG_NONVRAM;
	}

	/*
	 * Set up default role
	 */
	isp->isp_osinfo.default_role = ISP_DEFAULT_ROLES;

	/*
	 * Now it's time to see whether things can finish being setup or not.
	 */
	isp_reset(isp);
	if (isp->isp_state != ISP_RESETSTATE) {
		ISP_UNLOCK(isp);
		goto failure;
	}
	isp_init(isp);
	if (isp->isp_state != ISP_INITSTATE) {
		ISP_UNLOCK(isp);
		goto failure;
	}
	isp->isp_state = ISP_RUNSTATE;

	/*
	 * Create props. We can do this as soon as we've called isp_init
	 * which, no matter what our role, reads NVRAM and sets what
	 * WWN we'll be using.
	 */
	if (IS_FC(isp)) {
		/*
		 * Just use our 'nvram' port wwn. If NVRAM was corrupted, this will be DEFAULT_PORTWWN.
                 */
		isp_update_this_prop(isp, HBA_PORTID_PROPNAME, (caddr_t) &FCPARAM(isp, 0)->isp_wwpn_nvram,
		    sizeof (FCPARAM(isp, 0)->isp_wwpn_nvram), 1);
	} else {
		int chan;
		for (chan = 0; chan < isp->isp_nchan; chan++) {
			(void) isp_control(isp, ISPCTL_RESET_BUS, chan);
                }
	}

	if (isp_scsa_init(isp) != DDI_SUCCESS) {
		ISP_UNLOCK(isp);
		goto failure;
	}

#ifdef ISP_TARGET_MODE
	/*
	 * Setup target mode.
	 */
	isp_attach_target(isp);
#endif
	ISP_UNLOCK(isp);
	ddi_report_dev(dip);

	/*
	 * Create power management property. Don't fail if we can't.
	 */
	if (pm_create_components(dip, 1) == DDI_SUCCESS) {
		pm_set_normal_power(dip, 0, 1);
	} else {
		isp_prt(isp, ISP_LOGWARN, "creating pm component failed");
	}

	/*
	 * It's also not a fatal error if the minor node creation fails.
	 */
	if (ddi_create_minor_node(dip, "devctl", S_IFCHR, inst, DDI_NT_NEXUS, 0) != DDI_SUCCESS) {
		isp_prt(isp, ISP_LOGWARN, "ddi_create_minor_node failed");
	}
	return (DDI_SUCCESS);

failure:
	isp_unattach(isp);
	return (DDI_FAILURE);
}

/*
 * Interrupt Wrapper
 */

static uint_t
isp_intr_wrap(caddr_t arg)
{
	ispsoftc_t *isp = (ispsoftc_t *) arg;
	uint32_t isr;
	uint16_t sema, mbox;
	int rv = DDI_INTR_UNCLAIMED;

	ISP_LOCK(isp);
	isp->isp_osinfo.isp_onintstack = 1;
	isp->isp_intcnt++;
	if (ISP_READ_ISR(isp, &isr, &sema, &mbox)) {
		isp_intr(isp, isr, sema, mbox);
		rv = DDI_INTR_CLAIMED;
	} else {
		rv = DDI_INTR_CLAIMED;	/* lie */
		isp->isp_intbogus++;
	}
	isp->isp_osinfo.isp_onintstack = 0;
	ISP_UNLOCK(isp);
	return (rv);
}

int
isp_detach(dev_info_t *dip, ddi_detach_cmd_t cmd)
{
	ispsoftc_t *isp;
	scsi_hba_tran_t *tran;

	switch (cmd) {
	case DDI_DETACH:
		isp = (ispsoftc_t *)
		    ddi_get_soft_state(isp_softc_state, ddi_get_instance(dip));
		if (!isp) {
			return (DDI_FAILURE);
		}
		isp_unattach(isp);
		return (DDI_SUCCESS);

	case DDI_SUSPEND:
	case DDI_PM_SUSPEND:
		tran = (scsi_hba_tran_t *)ddi_get_driver_private(dip);
		if (!tran) {
			return (DDI_FAILURE);
		}
		isp = TRAN2ISP(tran);
		if (!isp) {
			return (DDI_FAILURE);
		}
		ISP_LOCK(isp);
		if (isp->isp_osinfo.tq) {
			ddi_taskq_suspend(isp->isp_osinfo.tq);
		}
		if (isp->isp_osinfo.task_kick) {
			untimeout(isp->isp_osinfo.task_kick);
			isp->isp_osinfo.task_kick = 0;
		}
		if (isp->isp_osinfo.kickt) {
			untimeout(isp->isp_osinfo.kickt);
			isp->isp_osinfo.kickt = 0;
		}
		isp->isp_osinfo.isp_suspended = 1;
		ISP_UNLOCK(isp);
		return (DDI_SUCCESS);

	default:
		return (DDI_FAILURE);
	}
}

static void
isp_unattach(ispsoftc_t *isp)
{

	if (isp->isp_regs) {
		ISP_DISABLE_INTS(isp);
	}

	if (isp->isp_osinfo.task_kick) {
		untimeout(isp->isp_osinfo.task_kick);
		isp->isp_osinfo.task_kick = 0;
	}

	if (isp->isp_osinfo.kickt) {
		untimeout(isp->isp_osinfo.kickt);
		isp->isp_osinfo.kickt = 0;
	}

	if (isp->isp_osinfo.tq) {
		ddi_taskq_destroy(isp->isp_osinfo.tq);
		isp->isp_osinfo.tq = NULL;
	}
#ifdef ISP_TARGET_MODE
	/*
	 * Decommission target mode.
	 */
	isp_detach_target(isp);
#endif
	if (isp->isp_tran) {
		(void) scsi_hba_detach(isp->isp_dip);
		scsi_hba_tran_free(isp->isp_tran);
		isp->isp_tran = NULL;
	}

        if (isp->isp_xflist) {
		kmem_free(isp->isp_xflist, sizeof (XS_T **) * isp->isp_maxcmds);
		isp->isp_xflist = NULL;
	}

	if (isp->isp_rquest) {
		ddi_dma_unbind_handle(isp->isp_dma_chandle);
		ddi_dma_free_handle(&isp->isp_dma_chandle);
		isp->isp_dma_chandle = NULL;
		ddi_dma_mem_free(&isp->isp_cmd_acc_handle);
		isp->isp_rquest = NULL;
	}

	if (isp->isp_dma_handle) {
		ddi_dma_free_handle(&isp->isp_dma_handle);
		isp->isp_dma_handle = NULL;
	}

	if (isp->isp_osinfo.isp_mcache) {
		kmem_cache_destroy(isp->isp_osinfo.isp_mcache);
		isp->isp_osinfo.isp_mcache = 0;
	}

	ddi_remove_intr(isp->isp_dip, 0, isp->isp_iblock);
	sema_destroy(ISP_MBOX_SEMA(isp));
	cv_destroy(ISP_MBOX_CV(isp));
	mutex_destroy(ISP_LOCKP(isp));
	ddi_regs_map_free(&isp->isp_reg_acc_handle);
	if (isp->isp_bustype == ISP_BT_PCI) {
		pci_config_teardown(&isp->isp_phandle);
	}
	ddi_remove_minor_node(isp->isp_dip, "devctl");
	if (isp->isp_osinfo.ggp) {
		kmem_free(isp->isp_osinfo.ggp, sizeof (struct gg) * ISP_MAX_TARGETS(isp));
		isp->isp_osinfo.ggp = NULL;
	}
	if (isp->isp_param && isp->isp_osinfo.pamt) {
		kmem_free(isp->isp_param, isp->isp_osinfo.pamt);
		isp->isp_param = NULL;
	}
	ddi_soft_state_free(isp_softc_state, isp->isp_unit);
}
