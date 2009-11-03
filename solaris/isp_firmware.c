/* $Id: isp_firmware.c,v 1.13 2008/02/11 23:59:06 mjacob Exp $ */
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
 * ISP Firmware Helper Functrion
 */

#include "isp_solaris.h"
#include "asm_1000.h"
#include "asm_1040.h"
#include "asm_1080.h"
#include "asm_12160.h"
#include "asm_2100.h"
#include "asm_2200.h"
#include "asm_2300.h"

#ifdef	DYNAMIC_FW_SELECTION
#define	FWNAME(x, tm)	(tm) ? x ## _it : x
#else
#define	FWNAME(x, tm)	x
#endif

void
isp_get_firmware(int version, int tm, uint32_t devid, const uint16_t **ptrp)
{
	const uint16_t *rp = NULL;

#if	!defined(DYNAMIC_FW_SELECTION) && !defined(ISP_TARGET_MODE)
	if (tm)
		return;
#endif
#if	!defined(DYNAMIC_FW_SELECTION) && defined(ISP_TARGET_MODE)
	ARGSUSED(tm);
#endif
	if (version == ISPFW_VERSION) {
		switch (devid) {
		case SBUS_QLOGIC_ISP:
			rp = FWNAME(isp_1000_risc_code, tm);
			break;
		case PCI_QLOGIC_ISP:
			rp = FWNAME(isp_1040_risc_code, tm);
			break;
		case PCI_QLOGIC_ISP1080:
		case PCI_QLOGIC_ISP1240:
		case PCI_QLOGIC_ISP1280:
			rp = FWNAME(isp_1080_risc_code, tm);
			break;
		case PCI_QLOGIC_ISP10160:
		case PCI_QLOGIC_ISP12160:
			rp = isp_12160_risc_code;
			break;
		case PCI_QLOGIC_ISP2100:
			rp = isp_2100_risc_code;
			break;
		case PCI_QLOGIC_ISP2200:
			rp = isp_2200_risc_code;
			break;
		case PCI_QLOGIC_ISP2300:
		case PCI_QLOGIC_ISP2312:
		case PCI_QLOGIC_ISP6312:
			rp = isp_2300_risc_code;
			break;
		case PCI_QLOGIC_ISP2322:
			/*
			 * Don't even attempt do this yet.
			 */
			break;
		default:
			break;
		}
	}
	if (rp) {
		*ptrp = rp;
	}
}
