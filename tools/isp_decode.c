/* $Id: isp_decode.c,v 1.12 2009/05/01 22:34:13 mjacob Exp $ */
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
#include <sys/types.h>
#include <unistd.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#define	ISP_TARGET_MODE	1
#define	ISP_TOOLS	1

#include "isp_stds.h"
#include "ispmbox.h"
#include "isp_target.h"

static void decode_t2rqs(uint8_t *);
static void decode_response(uint8_t *);
static void decode_ctio2(uint8_t *);
static void decode_ctio7(uint8_t *);
static void decode_rqst7(uint8_t *);
static void decode_a64_cont(uint8_t *);

int
main(void)
{
	isphdr_t *hdr;
	uint8_t buf[QENTRY_LEN];
	int i;

again:
	for (i = 0; i < QENTRY_LEN; i++) {
		int x;
		int r = scanf("%02x", &x);
		if (r == EOF) {
			break;
		}
		if (r != 1) {
			fprintf(stderr, "bad conversion at offset %d\n", i);
			break;
		}
		if (x > 0xff) {
			fprintf(stderr, "bogosity- x = 0x%x\n", x);
			break;
		}
		buf[i] = x;
	}
	if (i == 0) {
		return (0);
	} else if (i != QENTRY_LEN) {
		return (1);
	}
	hdr = (isphdr_t *) buf;
	switch (hdr->rqs_entry_type) {
	case RQSTYPE_RESPONSE:
		decode_response(buf);
		break;
	case RQSTYPE_T2RQS:
		decode_t2rqs(buf);
		break;
	case RQSTYPE_CTIO2:
	case RQSTYPE_CTIO3:
		decode_ctio2(buf);
		break;
	case RQSTYPE_CTIO7:
		decode_ctio7(buf);
		break;
	case RQSTYPE_T7RQS:
		decode_rqst7(buf);
		break;
	case RQSTYPE_A64_CONT:
		decode_a64_cont(buf);
		break;
	default:
		fprintf(stderr, "type 0x%x unknown\n", hdr->rqs_entry_type);
		break;
	}
	goto again;
}

#define	DOOB(x)		printf("%30s:", # x );
#define	DOOF(ptr, x)	printf("%30s: 0x%x\n", # x , ptr -> x )
#define	DOOD(ptr, x)	printf("%30s: 0x%x (%u)\n", # x , ptr -> x, ptr -> x)

static void
decode_t2rqs(uint8_t *buf)
{
	int i;
	ispreqt2_t *t2;

	t2 = (ispreqt2_t *)buf;
	DOOF(t2, req_header.rqs_entry_type);
	DOOF(t2, req_header.rqs_entry_count);
	DOOF(t2, req_header.rqs_seqno);
	DOOF(t2, req_header.rqs_flags);
        DOOF(t2, req_scclun);
        DOOF(t2, req_flags);
        DOOF(t2, req_time);
        DOOF(t2, req_seg_count);
	DOOB(("req_cdb"));
	for (i = 0; i < sizeof (t2->req_cdb); i++) {
        	printf(" %02x", t2->req_cdb[i]);
	}
	putchar('\n');
        DOOD(t2, req_totalcnt);
	putchar('\n');
	for (i = 0; i < ISP_RQDSEG_T2; i++) {
		printf("\t\t DS%d: 0x%08x 0x%x (%u)\n", i, t2->req_dataseg[i].ds_base, t2->req_dataseg[i].ds_count, t2->req_dataseg[i].ds_count);
	}
	putchar('\n');
}

static void
decode_response(uint8_t *buf)
{
	ispstatusreq_t *sp;

	sp = (ispstatusreq_t *)buf;

	DOOF(sp, req_header.rqs_entry_type);
	DOOF(sp, req_header.rqs_entry_count);
	DOOF(sp, req_header.rqs_seqno);
	DOOF(sp, req_header.rqs_flags);
	DOOF(sp, req_handle);
	DOOF(sp, req_scsi_status);
	DOOF(sp, req_state_flags);
	DOOF(sp, req_status_flags);
	DOOF(sp, req_time);
	DOOD(sp, req_sense_len);
	putchar('\n');
}

static void
decode_ctio2(uint8_t *buf)
{
	ct2_entry_t *ct;
	int i;

	ct = (ct2_entry_t *)buf;

	DOOF(ct, ct_header.rqs_entry_type);
	DOOF(ct, ct_header.rqs_entry_count);
	DOOF(ct, ct_header.rqs_seqno);
	DOOF(ct, ct_header.rqs_flags);
	DOOF(ct, ct_syshandle);
	DOOF(ct, ct_lun);
	DOOF(ct, ct_iid);
	DOOF(ct, ct_rxid);
	DOOF(ct, ct_flags);
	DOOF(ct, ct_status);
	DOOD(ct, ct_timeout);
	DOOF(ct, ct_seg_count);
	DOOF(ct, ct_reloff);
	DOOD(ct, ct_resid);
	if ((ct->ct_flags & CT2_FLAG_MMASK) == CT2_FLAG_MODE0) {
		DOOF(ct, rsp.m0.ct_scsi_status);
		DOOD(ct, rsp.m0.ct_xfrlen);
		if (ct->ct_header.rqs_entry_type == RQSTYPE_CTIO2) {
			DOOF(ct, rsp.m0.u.ct_dataseg[0].ds_base);
			DOOD(ct, rsp.m0.u.ct_dataseg[0].ds_count);
			DOOF(ct, rsp.m0.u.ct_dataseg[1].ds_base);
			DOOD(ct, rsp.m0.u.ct_dataseg[1].ds_count);
			DOOF(ct, rsp.m0.u.ct_dataseg[2].ds_base);
			DOOD(ct, rsp.m0.u.ct_dataseg[2].ds_count);
		} else if (ct->ct_header.rqs_entry_type == RQSTYPE_CTIO3) {
			DOOF(ct, rsp.m0.u.ct_dataseg64[0].ds_base);
			DOOF(ct, rsp.m0.u.ct_dataseg64[0].ds_basehi);
			DOOD(ct, rsp.m0.u.ct_dataseg64[0].ds_count);
			DOOF(ct, rsp.m0.u.ct_dataseg64[1].ds_base);
			DOOF(ct, rsp.m0.u.ct_dataseg64[1].ds_basehi);
			DOOD(ct, rsp.m0.u.ct_dataseg64[1].ds_count);
		}
	} else if ((ct->ct_flags & CT2_FLAG_MMASK) == CT2_FLAG_MODE1) {
		DOOF(ct, rsp.m1.ct_scsi_status);
		DOOF(ct, rsp.m1.ct_senselen);
		DOOD(ct, rsp.m1.ct_resplen);
		printf("%30s:", "ct_resp");
		for (i = 0; i < MAXRESPLEN; i++) {
			printf(" %02x", ct->rsp.m1.ct_resp[i] & 0xff);
			if (((i+1) % 8) == 0 && (i+1) != MAXRESPLEN) {
				putchar('\n');
				DOOB((ct_resp cont));
			}
		}
		putchar('\n');
	}
	putchar('\n');
}

static void
decode_ctio7(uint8_t *buf)
{
	ct7_entry_t *ct;
	int mode;
	int i;

	ct = (ct7_entry_t *)buf;
	mode = ct->ct_flags & CT7_FLAG_MMASK;

	DOOF(ct, ct_header.rqs_entry_type);
	DOOF(ct, ct_header.rqs_entry_count);
	DOOF(ct, ct_header.rqs_seqno);
	DOOF(ct, ct_header.rqs_flags);
	DOOF(ct, ct_syshandle);
	DOOF(ct, ct_nphdl);
	DOOF(ct, ct_timeout);
	DOOF(ct, ct_seg_count);
	DOOF(ct, ct_vpidx);
	DOOF(ct, ct_xflags);
	DOOF(ct, ct_iid_lo);
	DOOF(ct, ct_iid_hi);
	DOOF(ct, ct_rxid);
	if (mode == 1) {
		DOOF(ct, ct_senselen);
	}
	DOOF(ct, ct_flags);
	DOOD(ct, ct_resid);
	DOOF(ct, ct_oxid);
	DOOF(ct, ct_scsi_status);
	if ((ct->ct_flags & CT7_FLAG_MMASK) == CT7_FLAG_MODE0) {
		DOOD(ct, rsp.m0.reloff);
		DOOD(ct, rsp.m0.ct_xfrlen);
		DOOF(ct, rsp.m0.ds.ds_base);
		DOOF(ct, rsp.m0.ds.ds_basehi);
		DOOD(ct, rsp.m0.ds.ds_count);
	} else if ((ct->ct_flags & CT7_FLAG_MMASK) == CT7_FLAG_MODE1) {
		DOOD(ct, rsp.m1.ct_resplen);
		printf("%30s:", "ct_resp");
		for (i = 0; i < MAXRESPLEN_24XX; i++) {
			printf(" %02x", ct->rsp.m1.ct_resp[i] & 0xff);
			if (((i+1) % 8) == 0 && (i+1) != MAXRESPLEN_24XX) {
				putchar('\n');
				DOOB((ct_resp cont));
			}
		}
		putchar('\n');
	}
	putchar('\n');
}

static void
decode_rqst7(uint8_t *buf)
{
	ispreqt7_t *t7;
	int i;

	t7 = (ispreqt7_t *)buf;

	DOOF(t7, req_header.rqs_entry_type);
	DOOF(t7, req_header.rqs_entry_count);
	DOOF(t7, req_header.rqs_seqno);
	DOOF(t7, req_header.rqs_flags);
	DOOF(t7, req_handle);
	DOOF(t7, req_nphdl);
	DOOF(t7, req_time);
	DOOF(t7, req_seg_count);
	DOOF(t7, req_reserved);
	DOOB(req_lun);
	for (i = 0; i < 8; i++) {
		printf(" 0x%02x", t7->req_cdb[i]);
	}
	putchar('\n');
	DOOF(t7, req_alen_datadir);
	DOOF(t7, req_task_management);
	DOOF(t7, req_task_attribute);
	DOOF(t7, req_crn);
	DOOB(cdb);
	for (i = 0; i < 16; i++) {
		printf(" 0x%02x", t7->req_cdb[i]);
		if (((i+1) % 8) == 0 && (i+1) != 16) {
			putchar('\n');
			DOOB((cdb));
		}
	}
	putchar('\n');
	DOOF(t7, req_dl);
	DOOF(t7, req_tidlo);
	DOOF(t7, req_tidhi);
	DOOF(t7, req_vpidx);
	DOOF(t7, req_dataseg.ds_base);
	DOOF(t7, req_dataseg.ds_basehi);
	DOOD(t7, req_dataseg.ds_count);
	putchar('\n');
}

static void
decode_a64_cont(uint8_t *buf)
{
	ispcontreq64_t *a64 = (ispcontreq64_t *) buf;
	int i;

	DOOF(a64, req_header.rqs_entry_type);
	DOOF(a64, req_header.rqs_entry_count);
	DOOF(a64, req_header.rqs_seqno);
	DOOF(a64, req_header.rqs_flags);
	for (i = 0; i < ISP_CDSEG64; i++) {
		DOOF(a64, req_dataseg[i].ds_base);
		DOOF(a64, req_dataseg[i].ds_basehi);
		DOOD(a64, req_dataseg[i].ds_count);
	}
	putchar('\n');
}
