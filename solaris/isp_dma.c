/* $Id: isp_dma.c,v 1.16 2009/02/14 00:09:02 mjacob Exp $ */
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
 * DMA support routines for Qlogic FC/SCSI/IP Host Adapter Driver
 */
#include "isp_solaris.h"

int
isp_mbxdma_setup(ispsoftc_t *isp)
{
	uint count;
	size_t len, rlen;
	int rv;
	ddi_dma_attr_t tattr;
	ddi_device_acc_attr_t dattr;

	if (isp->isp_rquest) {
		return(0);
	}

	len = sizeof (XS_T **) * isp->isp_maxcmds;
	isp->isp_xflist = (XS_T **) kmem_zalloc(len, KM_SLEEP);
	if (isp->isp_xflist == NULL) {
		isp_prt(isp, ISP_LOGERR, "cannot alloc xflist array");
		return(1);
	}

	/*
	 * Force alignment of the request/response
	 * queues to a QENTRY_LEN boundary. But
	 * do this only for sparc- this breaks
	 * attaching under solaris/intel.
	 */
	tattr = isp->isp_dma_attr;
#ifdef	__i386__
	tattr.dma_attr_align = 1;
#else
	tattr.dma_attr_align = QENTRY_LEN;
#endif
	tattr.dma_attr_sgllen = 1;
	if (ddi_dma_alloc_handle(isp->isp_dip, &tattr, DDI_DMA_SLEEP, NULL,
	    &isp->isp_dma_chandle) != DDI_SUCCESS) {
		isp_prt(isp, ISP_LOGERR, "cannot alloc command dma handle");
		return(-1);
	}

	len = ISP_QUEUE_SIZE(RQUEST_QUEUE_LEN(isp));
	len += ISP_QUEUE_SIZE(RESULT_QUEUE_LEN(isp));
	if (IS_FC(isp))
		len += ISP_FC_SCRLEN;

	dattr = isp->isp_dev_attr;
	dattr.devacc_attr_endian_flags = DDI_NEVERSWAP_ACC;

	if (ddi_dma_mem_alloc(isp->isp_dma_chandle, len, &dattr,
	    DDI_DMA_CONSISTENT, DDI_DMA_SLEEP, NULL,
	    (caddr_t *)&isp->isp_rquest, &rlen,
	    &isp->isp_cmd_acc_handle) != DDI_SUCCESS) {
		isp_prt(isp, ISP_LOGERR, "cannot alloc cmd area");
		ddi_dma_free_handle(&isp->isp_dma_chandle);
		isp->isp_dma_chandle = NULL;
		return(-1);
	}
	ISP_MEMZERO(isp->isp_rquest, len);
	rv = ddi_dma_addr_bind_handle(isp->isp_dma_chandle, NULL,
	    isp->isp_rquest, rlen, DDI_DMA_RDWR|DDI_DMA_CONSISTENT,
	    DDI_DMA_SLEEP, NULL, &isp->isp_cdmacookie, &count);
	if (rv != DDI_DMA_MAPPED) {
		isp_prt(isp, ISP_LOGERR, "cannot bind cmd area");
		ddi_dma_free_handle(&isp->isp_dma_chandle);
		isp->isp_dma_chandle = NULL;
		return(-1);
	}
	if (count != 1) {
		isp_prt(isp, ISP_LOGERR, "multiple cookies for command area");
		ddi_dma_free_handle(&isp->isp_dma_chandle);
		isp->isp_dma_chandle = NULL;
		ddi_dma_mem_free(&isp->isp_cmd_acc_handle);
		isp->isp_rquest = NULL;
		return(-1);
	}

	isp->isp_rquest_dma = isp->isp_cdmacookie.dmac_address;
	isp->isp_result =
	    &((char *)isp->isp_rquest)[ISP_QUEUE_SIZE(RQUEST_QUEUE_LEN(isp))];

	isp->isp_result_dma = isp->isp_rquest_dma +
	    ISP_QUEUE_SIZE(RQUEST_QUEUE_LEN(isp));

	if (IS_FC(isp)) {
		fcparam *fcp = isp->isp_param;
		fcp->isp_scratch = &((char *)isp->isp_result)
		    [ISP_QUEUE_SIZE(RESULT_QUEUE_LEN(isp))];
		fcp->isp_scdma = isp->isp_result_dma;
		fcp->isp_scdma += ISP_QUEUE_SIZE(RESULT_QUEUE_LEN(isp));
	}
	return(0);
}

int
isp_dma_setup(ispsoftc_t *isp, struct scsi_pkt *pkt, ispreq_t *rq, uint32_t *nxtip, uint32_t optr)
{
	ddi_dma_cookie_t wcookie;
	isp_cmd_t *sp = PKT2CMD(pkt);
	uint32_t starti = isp->isp_reqidx, nxti = *nxtip;
	ispreq_t *qep;
	int segcnt, seg, ovseg, seglim, drq;

	qep = (ispreq_t *) ISP_QUEUE_ENTRY(isp->isp_rquest, starti);
	if (sp->cmd_dmahandle == 0) {
		goto mbxsync;
	}

	if (sp->cmd_flags & CFLAG_DMASEND) {
		drq = REQFLAG_DATA_OUT;
	} else {
		drq = REQFLAG_DATA_IN;
	}

	pkt->pkt_resid = sp->cmd_dmacount;

	if (IS_FC(isp)) {
		seglim = ISP_RQDSEG_T2;
		((ispreqt2_t *)rq)->req_totalcnt = sp->cmd_dmacount;
		((ispreqt2_t *)rq)->req_flags |= drq;
	} else {
		rq->req_flags |= drq;
		if (XS_CDBLEN(pkt) > 12) {
			seglim = 0;
		} else {
			seglim = ISP_RQDSEG;
		}
	}
	wcookie = sp->cmd_dmacookie;

	/*
	 * Fill in the data segments that fit into the request queue entry.
	 */
	segcnt = sp->cmd_ccnt;

	for (seg = 0, rq->req_seg_count = 0;
	    seg < segcnt && rq->req_seg_count < seglim;
	    seg++, rq->req_seg_count++) {
		if (IS_FC(isp)) {
			ispreqt2_t *rq2 = (ispreqt2_t *)rq;
			rq2->req_dataseg[rq2->req_seg_count].ds_base =
			    wcookie.dmac_address;
			rq2->req_dataseg[rq2->req_seg_count].ds_count =
			    wcookie.dmac_size;
		} else {
			rq->req_dataseg[rq->req_seg_count].ds_base =
			    wcookie.dmac_address;
			rq->req_dataseg[rq->req_seg_count].ds_count =
			    wcookie.dmac_size;
		}
		isp_prt(isp, ISP_LOGDEBUG2,
		    "isp_dma_setup(%p)(qe%d,idx%d)=(0x%x,%lu)", pkt,
		    rq->req_header.rqs_entry_count, rq->req_seg_count,
		    wcookie.dmac_address, (long)wcookie.dmac_size);
		if (seg < segcnt - 1) {
			ddi_dma_nextcookie(sp->cmd_dmahandle, &wcookie);
		}
	}

	if (seg == segcnt)
		goto mbxsync;

	/*
	 * Now do any continuation segments.
	 */

	do {
		uint16_t onxti;
		ispcontreq_t *crq, *cqe, local;
		crq = &local;

		/*
		 * We cannot use isp_gertrqentry because that allocates
		 * based off of the current in pointer which doesn't change
		 * until we add all of these entries.
		 */
		cqe = (ispcontreq_t *) ISP_QUEUE_ENTRY(isp->isp_rquest, nxti);
		onxti = nxti;
		nxti = ISP_NXT_QENTRY(onxti, RQUEST_QUEUE_LEN(isp));
		if (nxti == optr) {
			isp_prt(isp, ISP_LOGDEBUG0, "Request Queue Overflow");
			return(CMD_EAGAIN);
		}
		rq->req_header.rqs_entry_count++;
		ISP_MEMZERO((void *)crq, sizeof (*crq));
		crq->req_header.rqs_entry_count = 1;
		crq->req_header.rqs_entry_type = RQSTYPE_DATASEG;

		for (ovseg = 0; seg < segcnt && ovseg < ISP_CDSEG;
		    rq->req_seg_count++, seg++, ovseg++) {
			crq->req_dataseg[ovseg].ds_base =
			    wcookie.dmac_address;
			crq->req_dataseg[ovseg].ds_count =
			    wcookie.dmac_size;
			isp_prt(isp, ISP_LOGDEBUG2,
			    "isp_dma_setup(%p)(qe%d,idx%d)=(0x%x,%lu)", pkt,
			    rq->req_header.rqs_entry_count, ovseg,
			    wcookie.dmac_address, (long) wcookie.dmac_size);
			if (seg < segcnt - 1) {
				ddi_dma_nextcookie(sp->cmd_dmahandle, &wcookie);
			}
		}
		isp_put_cont_req(isp, crq, cqe);
		MEMORYBARRIER(isp, SYNC_REQUEST, onxti, QENTRY_LEN);
	} while (seg < segcnt);

mbxsync:
	if (isp->isp_dblev & ISP_LOGDEBUG3) {
		isp_print_bytes(isp, "req before swizzle", QENTRY_LEN, rq);
	}
	switch (rq->req_header.rqs_entry_type) {
	case RQSTYPE_REQUEST:
		isp_put_request(isp, rq, qep);
		break;
	case RQSTYPE_CMDONLY:
		isp_put_extended_request(isp, (ispextreq_t *)rq,
		    (ispextreq_t *)qep);
		break;
	case RQSTYPE_T2RQS:
		isp_put_request_t2(isp, (ispreqt2_t *) rq, (ispreqt2_t *) qep);
		break;
	}
	if (isp->isp_dblev & ISP_LOGDEBUG2) {
		isp_print_bytes(isp, "req after swizzle", QENTRY_LEN, qep);
	}
	if (sp->cmd_flags & CFLAG_CMDIOPB) {
		ddi_dma_sync(sp->cmd_dmahandle, 0, 0, DDI_DMA_SYNC_FORDEV);
	}
	*nxtip = nxti;
	return(CMD_QUEUED);
}
