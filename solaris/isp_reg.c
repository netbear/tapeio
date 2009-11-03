/* $Id: isp_reg.c,v 1.24 2008/04/15 22:41:09 mjacob Exp $ */
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
 * I/O Register Access support for Qlogic FC/SCSI/IP Host Adapter Driver
 */

#include "isp_solaris.h"

#define	IspVirt2Off(a, x)	((a)->isp_regoff[((x) & _BLK_REG_MASK) >> _BLK_REG_SHFT] + ((x) & 0xff))

static int isp_rd_debounced(ispsoftc_t *, int, uint16_t *);

static int
isp_rd_debounced(ispsoftc_t *isp, int regoff, uint16_t *rp)
{
	uint16_t val0, val1;
	int i = 0;
	do {
		val0 = isp_rd_reg(isp, regoff);
		val1 = isp_rd_reg(isp, regoff);
	} while (val0 != val1 && ++i < 1000);
	if (val0 != val1) {
		return (1);
	}
	*rp = val0;
	return (0);
}

int
isp_rd_isr(ispsoftc_t *isp, uint32_t *isrp, uint16_t *semap, uint16_t *mbox0p)
{
	uint16_t isr, sema;

	if (IS_2100(isp)) {
		if (isp_rd_debounced(isp, BIU_ISR, &isr)) {
			return (0);
		}
		if (isp_rd_debounced(isp, BIU_SEMA, &sema)) {
			return (0);
		}
	} else {
		isr = isp_rd_reg(isp, BIU_ISR);
		sema = isp_rd_reg(isp, BIU_SEMA);
	}
	isp_prt(isp, ISP_LOGDEBUG3, "ISR 0x%x SEMA 0x%x", isr, sema);
	isr &= INT_PENDING_MASK(isp);
	sema &= BIU_SEMA_LOCK;
	if (isr == 0 && sema == 0) {
		return (0);
	}
	*isrp = isr;
	if ((*semap = sema) != 0) {
		if (IS_2100(isp)) {
			if (isp_rd_debounced(isp, OUTMAILBOX0, mbox0p)) {
				return (0);
			}
		} else {
			*mbox0p = isp_rd_reg(isp, OUTMAILBOX0);
		}
	}
	return (1);
}

int
isp_rd_isr_2300(ispsoftc_t *isp, uint32_t *isrp, uint16_t *semap, uint16_t *mbox0p)
{
	uint32_t r2hisr;

	if ((isp_rd_reg(isp, BIU_ISR) & BIU2100_ISR_RISC_INT) == 0) {
		*isrp = 0;
		return (0);
	}
	r2hisr = ddi_get32(isp->isp_reg_acc_handle, (uint32_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_R2HSTSLO)]));
	isp_prt(isp, ISP_LOGDEBUG3, "RISC2HOST ISR 0x%x", r2hisr);
	if ((r2hisr & BIU_R2HST_INTR) == 0) {
		*isrp = 0;
		return (0);
	}
	switch (r2hisr & BIU_R2HST_ISTAT_MASK) {
	case ISPR2HST_ROM_MBX_OK:
	case ISPR2HST_ROM_MBX_FAIL:
	case ISPR2HST_MBX_OK:
	case ISPR2HST_MBX_FAIL:
	case ISPR2HST_ASYNC_EVENT:
		*isrp = r2hisr & 0xffff;
		*mbox0p = (r2hisr >> 16);
		*semap = 1;
		return (1);
	case ISPR2HST_RIO_16:
		*isrp = r2hisr & 0xffff;
		*mbox0p = ASYNC_RIO1;
		*semap = 1;
		return (1);
	case ISPR2HST_FPOST:
		*isrp = r2hisr & 0xffff;
		*mbox0p = ASYNC_CMD_CMPLT;
		*semap = 1;
		return (1);
	case ISPR2HST_FPOST_CTIO:
		*isrp = r2hisr & 0xffff;
		*mbox0p = ASYNC_CTIO_DONE;
		*semap = 1;
		return (1);
	case ISPR2HST_RSPQ_UPDATE:
		*isrp = r2hisr & 0xffff;
		*mbox0p = 0;
		*semap = 0;
		return (1);
	default:
		return (0);
	}
}

uint32_t
isp_rd_reg(ispsoftc_t *isp, int regoff)
{
	uint32_t rv;
	int oldconf = 0;

	if ((regoff & _BLK_REG_MASK) == SXP_BLOCK) {
		/*
		 * We will assume that someone has paused the RISC processor.
		 */
		oldconf = ddi_get16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]));
		ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]), oldconf | BIU_PCI_CONF1_SXP);
	}
	rv = ddi_get16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, regoff)]));
	isp_prt(isp, ISP_LOGDEBUG3, "read offset 0x%x returned %x", IspVirt2Off(isp, regoff), rv);
	if ((regoff & _BLK_REG_MASK) == SXP_BLOCK) {
		ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]), oldconf);
	}
	return(rv);
}

void
isp_wr_reg(ispsoftc_t *isp, int regoff, uint32_t val)
{
	int oldconf = 0;

	if ((regoff & _BLK_REG_MASK) == SXP_BLOCK) {
		/*
		 * We will assume that someone has paused the RISC processor.
		 */
		oldconf = ddi_get16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]));
		ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]), oldconf | BIU_PCI_CONF1_SXP);
	}
	isp_prt(isp, ISP_LOGDEBUG3, "write offset 0x%x with %x", IspVirt2Off(isp, regoff), val);
	ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, regoff)]), val);
	if ((regoff & _BLK_REG_MASK) == SXP_BLOCK) {
		ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]), oldconf);
	}
}

uint32_t
isp_rd_reg_1080(ispsoftc_t *isp, int regoff)
{
	uint16_t rv, oc = 0;

	if ((regoff & _BLK_REG_MASK) == SXP_BLOCK) {
		uint16_t tc;
		/*
		 * We will assume that someone has paused the RISC processor.
		 */
		oc = ddi_get16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]));
		tc = oc & ~BIU_PCI1080_CONF1_DMA;
		if (IS_1280(isp)) {
			if (regoff & SXP_BANK1_SELECT)
				tc |= BIU_PCI1080_CONF1_SXP0;
			else
				tc |= BIU_PCI1080_CONF1_SXP1;
		} else {
			tc |= BIU_PCI1080_CONF1_SXP0;
		}
		ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]), tc);
	} else if ((regoff & _BLK_REG_MASK) == DMA_BLOCK) {
		oc = ddi_get16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]));
		ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]), oc | BIU_PCI1080_CONF1_DMA);
	}
	rv = ddi_get16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, regoff)]));
	/*
	 * Okay, because BIU_CONF1 is always nonzero
	 */
	if (oc) {
		ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]), oc);
	}
	return(rv);
}

void
isp_wr_reg_1080(ispsoftc_t *isp, int regoff, uint32_t val)
{
	uint16_t oc = 0;
	if ((regoff & _BLK_REG_MASK) == SXP_BLOCK) {
		uint16_t tc;
		/*
		 * We will assume that someone has paused the RISC processor.
		 */
		oc = ddi_get16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]));
		tc = oc & ~BIU_PCI1080_CONF1_DMA;
		if (IS_1280(isp)) {
			if (regoff & SXP_BANK1_SELECT)
				tc |= BIU_PCI1080_CONF1_SXP0;
			else
				tc |= BIU_PCI1080_CONF1_SXP1;
		} else {
			tc |= BIU_PCI1080_CONF1_SXP0;
		}
		ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]), tc);
	} else if ((regoff & _BLK_REG_MASK) == DMA_BLOCK) {
		oc = ddi_get16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]));
		ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]), oc | BIU_PCI1080_CONF1_DMA);
	}
	ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, regoff)]), val);
	/*
	 * Okay, because BIU_CONF1 is always nonzero
	 */
	if (oc) {
		ddi_put16(isp->isp_reg_acc_handle, (uint16_t *) (&isp->isp_regs[IspVirt2Off(isp, BIU_CONF1)]), oc);
	}
}

void
isp_reset0(ispsoftc_t *isp)
{
	ISP_DISABLE_INTS(isp);
}

void
isp_reset1(ispsoftc_t *isp)
{
	if (isp->isp_bustype == ISP_BT_PCI) {
		isp_wr_reg(isp, HCCR, PCI_HCCR_CMD_BIOS);
	}
	ISP_ENABLE_INTS(isp);
	isp->isp_osinfo.isp_ints_ok = 1;
}

static void
isp_dump1XXXregs(ispsoftc_t *isp)
{
	isp_prt(isp, ISP_LOGERR, "CONF1=%x ICR=%x ISR=%x SEMA=%x RISC_HCCR=%x",
	    ISP_READ(isp, BIU_CONF1), ISP_READ(isp, BIU_ICR),
	    ISP_READ(isp, BIU_ISR), ISP_READ(isp, BIU_SEMA),
	    ISP_READ(isp, HCCR));
	ISP_WRITE(isp, HCCR, HCCR_CMD_PAUSE);
	isp_prt(isp, ISP_LOGERR, "CDMA_CONF=%x CDMA_STS=%x CDMA_FIFOSTAT=%x",
	    ISP_READ(isp, CDMA_CONF), ISP_READ(isp, CDMA_STATUS),
	    ISP_READ(isp, CDMA_FIFO_STS));
	isp_prt(isp, ISP_LOGERR, "DDMA_CONF=%x DDMA_STS=%x DDMA_FIFOSTAT=%x",
	    ISP_READ(isp, DDMA_CONF), ISP_READ(isp, DDMA_STATUS),
	    ISP_READ(isp, DDMA_FIFO_STS));
	isp_prt(isp, ISP_LOGERR, "SXP_INT=%x SXP_GROSS=%x SXP(SCSI_CTRL)=%x",
	    ISP_READ(isp, SXP_INTERRUPT), ISP_READ(isp, SXP_GROSS_ERR),
       	    ISP_READ(isp, SXP_PINS_CTRL));
	ISP_WRITE(isp, HCCR, HCCR_CMD_RELEASE);
	isp_prt(isp, ISP_LOGERR, "MBOX REGS: %x %x %x %x %x",
	    ISP_READ(isp, OUTMAILBOX0), ISP_READ(isp, OUTMAILBOX1),
	    ISP_READ(isp, OUTMAILBOX2), ISP_READ(isp, OUTMAILBOX3),
	    ISP_READ(isp, OUTMAILBOX4));
}

void
isp_dump_regs(ispsoftc_t *isp, const char *msg)
{
	if (msg) {
		isp_prt(isp, ISP_LOGERR, msg);
	}
	if (isp->isp_bustype == ISP_BT_PCI) {
                isp_prt(isp, ISP_LOGERR, "PCI Command=0x%04x PCI Status=0x%04x",
                    pci_config_get16(isp->isp_phandle, PCI_CONF_COMM),
                    pci_config_get16(isp->isp_phandle, PCI_CONF_STAT));
        }
	if (IS_SCSI(isp)) {
		isp_dump1XXXregs(isp);
	}
}
