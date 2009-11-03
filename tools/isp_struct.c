/* $Id: isp_struct.c,v 1.11 2009/01/24 17:55:55 mjacob Exp $ */
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

#define	NP(x)	((x *)0)
#define	DOPR(z)		printf("sizeof %s = %ld (0x%lx)\n", # z , (long) sizeof (z), (unsigned long) sizeof (z) )
#define	DOPRT(a, z)	printf("\tsizeof %s = %ld (0x%lx)\n", # z , (long) sizeof (NP(a)-> z), (long) sizeof (NP(a)-> z) )
#define	DOOF(z, a)	\
 printf("offset of %24s in %s = %ld (0x%lx)\n",  # a , #z , \
	 (long) offsetof(z,a), (long) offsetof(z, a))
#define	DOOFT(z, a)	\
 printf("\toffset of %24s in %s = %ld (0x%lx)\n",  # a , #z , \
	 (long) offsetof(z,a), (long) offsetof(z, a))


int
main(void)
{

	/* ispmbox.h structures */
	DOPR(ispds_t);
	DOOF(ispds_t, ds_base);
	DOOF(ispds_t, ds_count);
	putchar('\n');

	DOPR(ispds64_t);
	DOOF(ispds64_t, ds_base);
	DOOF(ispds64_t, ds_basehi);
	DOOF(ispds64_t, ds_count);
	putchar('\n');

	DOPR(ispdslist_t);
	DOOF(ispdslist_t, ds_type);
	DOOF(ispdslist_t, ds_segment);
	DOOF(ispdslist_t, ds_base);
	putchar('\n');


	DOPR(isphdr_t);
	DOOF(isphdr_t, rqs_entry_type);
	DOOF(isphdr_t, rqs_entry_count);
	DOOF(isphdr_t, rqs_seqno);
	DOOF(isphdr_t, rqs_flags);
	putchar('\n');

	DOPR(ispreq_t);
	DOOF(ispreq_t, req_header);
	DOOF(ispreq_t, req_handle);
	DOOF(ispreq_t, req_lun_trn);
	DOOF(ispreq_t, req_target);
	DOOF(ispreq_t, req_cdblen);
	DOOF(ispreq_t, req_flags);
	DOOF(ispreq_t, req_reserved);
	DOOF(ispreq_t, req_time);
	DOOF(ispreq_t, req_seg_count);
	DOOF(ispreq_t, req_cdb);
	DOOF(ispreq_t, req_dataseg);
	putchar('\n');

	DOPR(isp_marker_t);
	DOOF(isp_marker_t, mrk_header);
	DOOF(isp_marker_t, mrk_handle);
	DOOF(isp_marker_t, mrk_reserved0);
	DOOF(isp_marker_t, mrk_target);
	DOOF(isp_marker_t, mrk_modifier);
	DOOF(isp_marker_t, mrk_flags);
	DOOF(isp_marker_t, mrk_lun);
	DOOF(isp_marker_t, mrk_reserved1);
	putchar('\n');

	DOPR(isp_marker_24xx_t);
	DOOF(isp_marker_24xx_t, mrk_header);
	DOOF(isp_marker_24xx_t, mrk_handle);
	DOOF(isp_marker_24xx_t, mrk_nphdl);
	DOOF(isp_marker_24xx_t, mrk_modifier);
	DOOF(isp_marker_24xx_t, mrk_reserved0);
	DOOF(isp_marker_24xx_t, mrk_reserved1);
	DOOF(isp_marker_24xx_t, mrk_vphdl);
	DOOF(isp_marker_24xx_t, mrk_reserved2);
	DOOF(isp_marker_24xx_t, mrk_lun);
	DOOF(isp_marker_24xx_t, mrk_reserved3);
	putchar('\n');

	DOPR(ispreqt2_t);
	DOOF(ispreqt2_t, req_header);
	DOOF(ispreqt2_t, req_handle);
	DOOF(ispreqt2_t, req_lun_trn);
	DOOF(ispreqt2_t, req_target);
	DOOF(ispreqt2_t, req_scclun);
	DOOF(ispreqt2_t, req_flags);
	DOOF(ispreqt2_t, req_reserved);
	DOOF(ispreqt2_t, req_time);
	DOOF(ispreqt2_t, req_seg_count);
	DOOF(ispreqt2_t, req_cdb);
	DOOF(ispreqt2_t, req_totalcnt);
	DOOF(ispreqt2_t, req_dataseg);
	putchar('\n');

	DOPR(ispreqt2e_t);
	DOOF(ispreqt2e_t, req_header);
	DOOF(ispreqt2e_t, req_handle);
	DOOF(ispreqt2e_t, req_target);
	DOOF(ispreqt2e_t, req_scclun);
	DOOF(ispreqt2e_t, req_flags);
	DOOF(ispreqt2e_t, req_reserved);
	DOOF(ispreqt2e_t, req_time);
	DOOF(ispreqt2e_t, req_seg_count);
	DOOF(ispreqt2e_t, req_cdb);
	DOOF(ispreqt2e_t, req_totalcnt);
	DOOF(ispreqt2e_t, req_dataseg);
	putchar('\n');

	DOPR(ispreqt3_t);
	DOOF(ispreqt3_t, req_header);
	DOOF(ispreqt3_t, req_handle);
	DOOF(ispreqt3_t, req_lun_trn);
	DOOF(ispreqt3_t, req_target);
	DOOF(ispreqt3_t, req_scclun);
	DOOF(ispreqt3_t, req_flags);
	DOOF(ispreqt3_t, req_reserved);
	DOOF(ispreqt3_t, req_time);
	DOOF(ispreqt3_t, req_seg_count);
	DOOF(ispreqt3_t, req_cdb);
	DOOF(ispreqt3_t, req_totalcnt);
	DOOF(ispreqt3_t, req_dataseg);
	putchar('\n');

	DOPR(ispreqt3e_t);
	DOOF(ispreqt3e_t, req_header);
	DOOF(ispreqt3e_t, req_handle);
	DOOF(ispreqt3e_t, req_target);
	DOOF(ispreqt3e_t, req_scclun);
	DOOF(ispreqt3e_t, req_flags);
	DOOF(ispreqt3e_t, req_reserved);
	DOOF(ispreqt3e_t, req_time);
	DOOF(ispreqt3e_t, req_seg_count);
	DOOF(ispreqt3e_t, req_cdb);
	DOOF(ispreqt3e_t, req_totalcnt);
	DOOF(ispreqt3e_t, req_dataseg);
	putchar('\n');

	DOPR(ispextreq_t);
	DOOF(ispextreq_t, req_header);
	DOOF(ispextreq_t, req_handle);
	DOOF(ispextreq_t, req_lun_trn);
	DOOF(ispextreq_t, req_target);
	DOOF(ispextreq_t, req_cdblen);
	DOOF(ispextreq_t, req_flags);
	DOOF(ispextreq_t, req_reserved);
	DOOF(ispextreq_t, req_time);
	DOOF(ispextreq_t, req_seg_count);
	DOOF(ispextreq_t, req_cdb);
	putchar('\n');

	DOPR(fcp_cmnd_ds_t);
	DOOF(fcp_cmnd_ds_t, fcd_length);
	DOOF(fcp_cmnd_ds_t, fcd_a1500);
	DOOF(fcp_cmnd_ds_t, fcd_a3116);
	DOOF(fcp_cmnd_ds_t, fcd_a4732);
	DOOF(fcp_cmnd_ds_t, fcd_a6348);
	putchar('\n');

	DOPR(ispreqt6_t);
	DOOF(ispreqt6_t, req_header);
	DOOF(ispreqt6_t, req_handle);
	DOOF(ispreqt6_t, req_nphdl);
	DOOF(ispreqt6_t, req_time);
	DOOF(ispreqt6_t, req_seg_count);
	DOOF(ispreqt6_t, req_fc_rsp_dsd_length);
	DOOF(ispreqt6_t, req_lun);
	DOOF(ispreqt6_t, req_flags);
	DOOF(ispreqt6_t, req_fc_cmnd_dsd_length);
	DOOF(ispreqt6_t, req_fc_cmnd_dsd_a1500);
	DOOF(ispreqt6_t, req_fc_cmnd_dsd_a3116);
	DOOF(ispreqt6_t, req_fc_cmnd_dsd_a4732);
	DOOF(ispreqt6_t, req_fc_cmnd_dsd_a6348);
	DOOF(ispreqt6_t, req_fc_rsp_dsd_a1500);
	DOOF(ispreqt6_t, req_fc_rsp_dsd_a3116);
	DOOF(ispreqt6_t, req_fc_rsp_dsd_a4732);
	DOOF(ispreqt6_t, req_fc_rsp_dsd_a6348);
	DOOF(ispreqt6_t, req_totalcnt);
	DOOF(ispreqt6_t, req_tidlo);
	DOOF(ispreqt6_t, req_tidhi);
	DOOF(ispreqt6_t, req_vpidx);
	DOOF(ispreqt6_t, req_dataseg);
	putchar('\n');

	DOPR(ispreqt7_t);
	DOOF(ispreqt7_t, req_header);
	DOOF(ispreqt7_t, req_handle);
	DOOF(ispreqt7_t, req_nphdl);
	DOOF(ispreqt7_t, req_time);
	DOOF(ispreqt7_t, req_seg_count);
	DOOF(ispreqt7_t, req_reserved);
	DOOF(ispreqt7_t, req_lun);
	DOOF(ispreqt7_t, req_alen_datadir);
	DOOF(ispreqt7_t, req_task_management);
	DOOF(ispreqt7_t, req_task_attribute);
	DOOF(ispreqt7_t, req_crn);
	DOOF(ispreqt7_t, req_cdb);
	DOOF(ispreqt7_t, req_dl);
	DOOF(ispreqt7_t, req_tidlo);
	DOOF(ispreqt7_t, req_tidhi);
	DOOF(ispreqt7_t, req_vpidx);
	DOOF(ispreqt7_t, req_dataseg);
	putchar('\n');

	DOPR(ispcontreq_t);
	DOOF(ispcontreq_t, req_header);
	DOOF(ispcontreq_t, req_reserved);
	DOOF(ispcontreq_t, req_dataseg);
	putchar('\n');

	DOPR(ispcontreq64_t);
	DOOF(ispcontreq64_t, req_header);
	DOOF(ispcontreq64_t, req_dataseg);
	putchar('\n');

	DOPR(ispstatusreq_t);
	DOOF(ispstatusreq_t, req_header);
	DOOF(ispstatusreq_t, req_handle);
	DOOF(ispstatusreq_t, req_scsi_status);
	DOOF(ispstatusreq_t, req_completion_status);
	DOOF(ispstatusreq_t, req_state_flags);
	DOOF(ispstatusreq_t, req_status_flags);
	DOOF(ispstatusreq_t, req_time);
	DOOF(ispstatusreq_t, req_sense_len);
	DOOF(ispstatusreq_t, req_resid);
	DOOF(ispstatusreq_t, req_response);
	DOOF(ispstatusreq_t, req_sense_data);
	putchar('\n');

	DOPR(ispstatus_cont_t);
	DOOF(ispstatus_cont_t, req_header);
	DOOF(ispstatus_cont_t, req_sense_data);
	putchar('\n');

	DOPR(isp24xx_statusreq_t);
	DOOF(isp24xx_statusreq_t, req_header);
	DOOF(isp24xx_statusreq_t, req_handle);
	DOOF(isp24xx_statusreq_t, req_completion_status);
	DOOF(isp24xx_statusreq_t, req_oxid);
	DOOF(isp24xx_statusreq_t, req_resid);
	DOOF(isp24xx_statusreq_t, req_reserved0);
	DOOF(isp24xx_statusreq_t, req_state_flags);
	DOOF(isp24xx_statusreq_t, req_reserved1);
	DOOF(isp24xx_statusreq_t, req_scsi_status);
	DOOF(isp24xx_statusreq_t, req_fcp_residual);
	DOOF(isp24xx_statusreq_t, req_sense_len);
	DOOF(isp24xx_statusreq_t, req_response_len);
	DOOF(isp24xx_statusreq_t, req_rsp_sense);
	putchar('\n');


	DOPR(isp_ct_pt_t);
	DOOF(isp_ct_pt_t, ctp_header);
	DOOF(isp_ct_pt_t, ctp_handle);
	DOOF(isp_ct_pt_t, ctp_status);
	DOOF(isp_ct_pt_t, ctp_nphdl);
	DOOF(isp_ct_pt_t, ctp_cmd_cnt);
	DOOF(isp_ct_pt_t, ctp_vpidx);
	DOOF(isp_ct_pt_t, ctp_time);
	DOOF(isp_ct_pt_t, ctp_reserved0);
	DOOF(isp_ct_pt_t, ctp_rsp_cnt);
	DOOF(isp_ct_pt_t, ctp_reserved1);
	DOOF(isp_ct_pt_t, ctp_rsp_bcnt);
	DOOF(isp_ct_pt_t, ctp_cmd_bcnt);
	DOOF(isp_ct_pt_t, ctp_dataseg);
	putchar('\n');


	DOPR(isp_ms_t);
	DOOF(isp_ms_t, ms_header);
	DOOF(isp_ms_t, ms_handle);
	DOOF(isp_ms_t, ms_nphdl);
	DOOF(isp_ms_t, ms_status);
	DOOF(isp_ms_t, ms_flags);
	DOOF(isp_ms_t, ms_reserved1);
	DOOF(isp_ms_t, ms_time);
	DOOF(isp_ms_t, ms_cmd_cnt);
	DOOF(isp_ms_t, ms_tot_cnt);
	DOOF(isp_ms_t, ms_type);
	DOOF(isp_ms_t, ms_r_ctl);
	DOOF(isp_ms_t, ms_rxid);
	DOOF(isp_ms_t, ms_reserved2);
	DOOF(isp_ms_t, ms_handle2);
	DOOF(isp_ms_t, ms_rsp_bcnt);
	DOOF(isp_ms_t, ms_cmd_bcnt);
	DOOF(isp_ms_t, ms_dataseg);
	putchar('\n');

	DOPR(isp_rio1_t);
	DOOF(isp_rio1_t, req_header);
	DOOF(isp_rio1_t, req_handles);
	putchar('\n');

	DOPR(isp_rio2_t);
	DOOF(isp_rio2_t, req_header);
	DOOF(isp_rio2_t, req_handles);
	putchar('\n');

	DOPR(isp_icb_t);
	DOOF(isp_icb_t, icb_version);
	DOOF(isp_icb_t, icb_reserved0);
	DOOF(isp_icb_t, icb_fwoptions);
	DOOF(isp_icb_t, icb_maxfrmlen);
	DOOF(isp_icb_t, icb_maxalloc);
	DOOF(isp_icb_t, icb_execthrottle);
	DOOF(isp_icb_t, icb_retry_count);
	DOOF(isp_icb_t, icb_retry_delay);
	DOOF(isp_icb_t, icb_portname);
	DOOF(isp_icb_t, icb_hardaddr);
	DOOF(isp_icb_t, icb_iqdevtype);
	DOOF(isp_icb_t, icb_logintime);
	DOOF(isp_icb_t, icb_nodename);
	DOOF(isp_icb_t, icb_rqstout);
	DOOF(isp_icb_t, icb_rspnsin);
	DOOF(isp_icb_t, icb_rqstqlen);
	DOOF(isp_icb_t, icb_rsltqlen);
	DOOF(isp_icb_t, icb_rqstaddr);
	DOOF(isp_icb_t, icb_respaddr);
	DOOF(isp_icb_t, icb_lunenables);
	DOOF(isp_icb_t, icb_ccnt);
	DOOF(isp_icb_t, icb_icnt);
	DOOF(isp_icb_t, icb_lunetimeout);
	DOOF(isp_icb_t, icb_reserved1);
	DOOF(isp_icb_t, icb_xfwoptions);
	DOOF(isp_icb_t, icb_racctimer);
	DOOF(isp_icb_t, icb_idelaytimer);
	DOOF(isp_icb_t, icb_zfwoptions);
	DOOF(isp_icb_t, icb_reserved2);
	putchar('\n');

	DOPR(isp_icb_2400_t);
	DOOF(isp_icb_2400_t, icb_version);
	DOOF(isp_icb_2400_t, icb_reserved0);
	DOOF(isp_icb_2400_t, icb_maxfrmlen);
	DOOF(isp_icb_2400_t, icb_execthrottle);
	DOOF(isp_icb_2400_t, icb_xchgcnt);
	DOOF(isp_icb_2400_t, icb_hardaddr);
	DOOF(isp_icb_2400_t, icb_portname);
	DOOF(isp_icb_2400_t, icb_nodename);
	DOOF(isp_icb_2400_t, icb_rspnsin);
	DOOF(isp_icb_2400_t, icb_rqstout);
	DOOF(isp_icb_2400_t, icb_retry_count);
	DOOF(isp_icb_2400_t, icb_priout);
	DOOF(isp_icb_2400_t, icb_rsltqlen);
	DOOF(isp_icb_2400_t, icb_rqstqlen);
	DOOF(isp_icb_2400_t, icb_ldn_nols);
	DOOF(isp_icb_2400_t, icb_prqstqlen);
	DOOF(isp_icb_2400_t, icb_rqstaddr);
	DOOF(isp_icb_2400_t, icb_respaddr);
	DOOF(isp_icb_2400_t, icb_priaddr);
	DOOF(isp_icb_2400_t, icb_reserved1);
	DOOF(isp_icb_2400_t, icb_atio_in);
	DOOF(isp_icb_2400_t, icb_atioqlen);
	DOOF(isp_icb_2400_t, icb_atioqaddr);
	DOOF(isp_icb_2400_t, icb_idelaytimer);
	DOOF(isp_icb_2400_t, icb_logintime);
	DOOF(isp_icb_2400_t, icb_fwoptions1);
	DOOF(isp_icb_2400_t, icb_fwoptions2);
	DOOF(isp_icb_2400_t, icb_fwoptions3);
	DOOF(isp_icb_2400_t, icb_reserved2);
	putchar('\n');

	DOPR(isp_pdb_21xx_t);
	DOOF(isp_pdb_21xx_t, pdb_options);
	DOOF(isp_pdb_21xx_t, pdb_mstate);
	DOOF(isp_pdb_21xx_t, pdb_sstate);
	DOOF(isp_pdb_21xx_t, pdb_hardaddr_bits);
	DOOF(isp_pdb_21xx_t, pdb_portid_bits);
	DOOF(isp_pdb_21xx_t, pdb_nodename);
	DOOF(isp_pdb_21xx_t, pdb_portname);
	DOOF(isp_pdb_21xx_t, pdb_execthrottle);
	DOOF(isp_pdb_21xx_t, pdb_exec_count);
	DOOF(isp_pdb_21xx_t, pdb_retry_count);
	DOOF(isp_pdb_21xx_t, pdb_retry_delay);
	DOOF(isp_pdb_21xx_t, pdb_resalloc);
	DOOF(isp_pdb_21xx_t, pdb_curalloc);
	DOOF(isp_pdb_21xx_t, pdb_qhead);
	DOOF(isp_pdb_21xx_t, pdb_qtail);
	DOOF(isp_pdb_21xx_t, pdb_tl_next);
	DOOF(isp_pdb_21xx_t, pdb_tl_last);
	DOOF(isp_pdb_21xx_t, pdb_features);
	DOOF(isp_pdb_21xx_t, pdb_pconcurrnt);
	DOOF(isp_pdb_21xx_t, pdb_roi);
	DOOF(isp_pdb_21xx_t, pdb_target);
	DOOF(isp_pdb_21xx_t, pdb_initiator);
	DOOF(isp_pdb_21xx_t, pdb_rdsiz);
	DOOF(isp_pdb_21xx_t, pdb_ncseq);
	DOOF(isp_pdb_21xx_t, pdb_noseq);
	DOOF(isp_pdb_21xx_t, pdb_labrtflg);
	DOOF(isp_pdb_21xx_t, pdb_lstopflg);
	DOOF(isp_pdb_21xx_t, pdb_sqhead);
	DOOF(isp_pdb_21xx_t, pdb_sqtail);
	DOOF(isp_pdb_21xx_t, pdb_ptimer);
	DOOF(isp_pdb_21xx_t, pdb_nxt_seqid);
	DOOF(isp_pdb_21xx_t, pdb_fcount);
	DOOF(isp_pdb_21xx_t, pdb_prli_len);
	DOOF(isp_pdb_21xx_t, pdb_prli_svc0);
	DOOF(isp_pdb_21xx_t, pdb_prli_svc3);
	DOOF(isp_pdb_21xx_t, pdb_loopid);
	DOOF(isp_pdb_21xx_t, pdb_il_ptr);
	DOOF(isp_pdb_21xx_t, pdb_sl_ptr);
	putchar('\n');

	DOPR(isp_pdb_24xx_t);
	DOOF(isp_pdb_24xx_t, pdb_flags);
	DOOF(isp_pdb_24xx_t, pdb_curstate);
	DOOF(isp_pdb_24xx_t, pdb_laststate);
	DOOF(isp_pdb_24xx_t, pdb_hardaddr_bits);
	DOOF(isp_pdb_24xx_t, pdb_portid_bits);
	DOOF(isp_pdb_24xx_t, pdb_retry_timer);
	DOOF(isp_pdb_24xx_t, pdb_handle);
	DOOF(isp_pdb_24xx_t, pdb_rcv_dsize);
	DOOF(isp_pdb_24xx_t, pdb_reserved0);
	DOOF(isp_pdb_24xx_t, pdb_prli_svc0);
	DOOF(isp_pdb_24xx_t, pdb_prli_svc3);
	DOOF(isp_pdb_24xx_t, pdb_portname);
	DOOF(isp_pdb_24xx_t, pdb_nodename);
	DOOF(isp_pdb_24xx_t, pdb_reserved1);
	putchar('\n');

	DOPR(isp_plogx_t);
	DOOF(isp_plogx_t, plogx_header);
	DOOF(isp_plogx_t, plogx_handle);
	DOOF(isp_plogx_t, plogx_status);
	DOOF(isp_plogx_t, plogx_nphdl);
	DOOF(isp_plogx_t, plogx_flags);
	DOOF(isp_plogx_t, plogx_vphdl);
	DOOF(isp_plogx_t, plogx_portlo);
	DOOF(isp_plogx_t, plogx_rspsz_porthi);
	DOOF(isp_plogx_t, plogx_ioparm);
	putchar('\n');

	DOPR(sns_screq_t);
	DOPR(sns_ga_nxt_req_t);
	DOPR(sns_gxn_id_req_t);
	DOPR(sns_gid_ft_req_t);
	DOPR(sns_rft_id_req_t);
	DOPR(sns_scrsp_t);
	DOPR(sns_ga_nxt_rsp_t);
	DOPR(sns_gxn_id_rsp_t);
	DOPR(sns_gff_id_rsp_t);
	DOPR(sns_gid_ft_rsp_t);
	putchar('\n');

	DOPR(els_t);
	DOOF(els_t, els_hdr);
	DOOF(els_t, els_handle);
	DOOF(els_t, els_status);
	DOOF(els_t, els_nphdl);
	DOOF(els_t, els_xmit_dsd_count);
	DOOF(els_t, els_vphdl);
	DOOF(els_t, els_sof);
	DOOF(els_t, els_rxid);
	DOOF(els_t, els_recv_dsd_count);
	DOOF(els_t, els_opcode);
	DOOF(els_t, els_reserved1);
	DOOF(els_t, els_did_lo);
	DOOF(els_t, els_did_mid);
	DOOF(els_t, els_did_hi);
	DOOF(els_t, els_reserved2);
	DOOF(els_t, els_reserved3);
	DOOF(els_t, els_ctl_flags);
	putchar('\n');
	DOOFT(els_t, els_bytecnt);
	DOOFT(els_t, els_subcode1);
	DOOFT(els_t, els_subcode2);
	DOOFT(els_t, els_reserved4);
	putchar('\n');
	DOOFT(els_t, els_recv_bytecnt);
	DOOFT(els_t, els_xmit_bytecnt);
	DOOFT(els_t, els_xmit_dsd_length);
	DOOFT(els_t, els_xmit_dsd_a1500);
	DOOFT(els_t, els_xmit_dsd_a3116);
	DOOFT(els_t, els_xmit_dsd_a4732);
	DOOFT(els_t, els_xmit_dsd_a6348);
	DOOFT(els_t, els_recv_dsd_length);
	DOOFT(els_t, els_recv_dsd_a1500);
	DOOFT(els_t, els_recv_dsd_a3116);
	DOOFT(els_t, els_recv_dsd_a4732);
	DOOFT(els_t, els_recv_dsd_a6348);
	putchar('\n');
	

	/* isp_stds.h */

	DOPR(fc_hdr_t);
	DOOF(fc_hdr_t, r_ctl);
	DOOF(fc_hdr_t, d_id);
	DOOF(fc_hdr_t, cs_ctl);
	DOOF(fc_hdr_t, s_id);
	DOOF(fc_hdr_t, type);
	DOOF(fc_hdr_t, f_ctl);
	DOOF(fc_hdr_t, seq_id);
	DOOF(fc_hdr_t, df_ctl);
	DOOF(fc_hdr_t, seq_cnt);
	DOOF(fc_hdr_t, ox_id);
	DOOF(fc_hdr_t, rx_id);
	DOOF(fc_hdr_t, parameter);
	putchar('\n');

	DOPR(fcp_cmnd_iu_t);
	DOOF(fcp_cmnd_iu_t, fcp_cmnd_lun);
	DOOF(fcp_cmnd_iu_t, fcp_cmnd_crn);
	DOOF(fcp_cmnd_iu_t, fcp_cmnd_task_attribute);
	DOOF(fcp_cmnd_iu_t, fcp_cmnd_task_management);
	DOOF(fcp_cmnd_iu_t, fcp_cmnd_alen_datadir);
	DOOF(fcp_cmnd_iu_t, cdb_dl.sf.fcp_cmnd_cdb);
	DOOF(fcp_cmnd_iu_t, cdb_dl.sf.fcp_cmnd_dl);
	putchar('\n');

	DOPR(ct_hdr_t);
	DOOF(ct_hdr_t, ct_revision);
	DOOF(ct_hdr_t, ct_in_id);
	DOOF(ct_hdr_t, ct_fcs_type);
	DOOF(ct_hdr_t, ct_fcs_subtype);
	DOOF(ct_hdr_t, ct_options);
	DOOF(ct_hdr_t, ct_reserved0);
	DOOF(ct_hdr_t, ct_cmd_resp);
	DOOF(ct_hdr_t, ct_bcnt_resid);
	DOOF(ct_hdr_t, ct_reserved1);
	DOOF(ct_hdr_t, ct_reason);
	DOOF(ct_hdr_t, ct_explanation);
	DOOF(ct_hdr_t, ct_vunique);
	putchar('\n');

	DOPR(rft_id_t);
	DOOF(rft_id_t, rftid_hdr);
	DOOF(rft_id_t, rftid_reserved);
	DOOF(rft_id_t, rftid_portid);
	DOOF(rft_id_t, rftid_fc4types);

	/* isp_target.h */

	DOPR(lun_entry_t);
	DOPR(in_fcentry_t);
	DOPR(in_fcentry_e_t);
	DOPR(in_fcentry_24xx_t);
	DOPR(na_entry_t);
	DOPR(na_fcentry_t);
	DOPR(na_fcentry_e_t);
	DOPR(na_fcentry_24xx_t);
	DOPR(at_entry_t);
	DOPR(at2_entry_t);
	DOPR(at2e_entry_t);
	DOPR(at7_entry_t);

	DOPR(ct_entry_t);

	DOPR(ct2_entry_t);
	DOOF(ct2_entry_t, ct_header);
	DOOF(ct2_entry_t, ct_syshandle);
	DOOF(ct2_entry_t, ct_lun);
	DOOF(ct2_entry_t, ct_iid);
	DOOF(ct2_entry_t, ct_rxid);
	DOOF(ct2_entry_t, ct_flags);
	DOOF(ct2_entry_t, ct_status);
	DOOF(ct2_entry_t, ct_timeout);
	DOOF(ct2_entry_t, ct_seg_count);
	DOOF(ct2_entry_t, ct_reloff);
	DOOF(ct2_entry_t, ct_resid);
	DOOF(ct2_entry_t, rsp);

	DOPR(ct2e_entry_t);

	DOPR(ct7_entry_t);
	DOOF(ct7_entry_t, ct_header);
	DOOF(ct7_entry_t, ct_syshandle);
	DOOF(ct7_entry_t, ct_nphdl);
	DOOF(ct7_entry_t, ct_timeout);
	DOOF(ct7_entry_t, ct_seg_count);
	DOOF(ct7_entry_t, ct_vpidx);
	DOOF(ct7_entry_t, ct_xflags);
	DOOF(ct7_entry_t, ct_iid_lo);
	DOOF(ct7_entry_t, ct_iid_hi);
	DOOF(ct7_entry_t, ct_reserved);
	DOOF(ct7_entry_t, ct_rxid);
	DOOF(ct7_entry_t, ct_senselen);
	DOOF(ct7_entry_t, ct_flags);
	DOOF(ct7_entry_t, ct_resid);
	DOOF(ct7_entry_t, ct_oxid);
	DOOF(ct7_entry_t, ct_scsi_status);
	DOOF(ct7_entry_t, rsp.m0.reloff);
	DOOF(ct7_entry_t, rsp.m0.reserved0);
	DOOF(ct7_entry_t, rsp.m0.ct_xfrlen);
	DOOF(ct7_entry_t, rsp.m0.reserved1);
	DOOF(ct7_entry_t, rsp.m0.ds);
	DOOF(ct7_entry_t, rsp.m1.ct_resplen);
	DOOF(ct7_entry_t, rsp.m1.reserved);
	DOOF(ct7_entry_t, rsp.m1.ct_resp);
	DOOF(ct7_entry_t, rsp.m2.reserved0);
	DOOF(ct7_entry_t, rsp.m2.ct_datalen);
	DOOF(ct7_entry_t, rsp.m2.reserved1);
	DOOF(ct7_entry_t, rsp.m2.ct_fcp_rsp_iudata);
	putchar('\n');

	DOPR(abts_t);
	DOOF(abts_t, abts_header);
	DOOF(abts_t, abts_reserved0);
	DOOF(abts_t, abts_nphdl);
	DOOF(abts_t, abts_reserved1);
	DOOF(abts_t, abts_sof);
	DOOF(abts_t, abts_rxid_abts);
	DOOF(abts_t, abts_did_lo);
	DOOF(abts_t, abts_did_hi);
	DOOF(abts_t, abts_r_ctl);
	DOOF(abts_t, abts_sid_lo);
	DOOF(abts_t, abts_sid_hi);
	DOOF(abts_t, abts_cs_ctl);
	DOOF(abts_t, abts_fs_ctl);
	DOOF(abts_t, abts_f_ctl);
	DOOF(abts_t, abts_type);
	DOOF(abts_t, abts_seq_cnt);
	DOOF(abts_t, abts_df_ctl);
	DOOF(abts_t, abts_seq_id);
	DOOF(abts_t, abts_rx_id);
	DOOF(abts_t, abts_ox_id);
	DOOF(abts_t, abts_param);
	DOOF(abts_t, abts_reserved2);
	DOOF(abts_t, abts_rxid_task);
	putchar('\n');

	DOPR(abts_rsp_t);
	DOOF(abts_rsp_t, abts_rsp_header);
	DOOF(abts_rsp_t, abts_rsp_handle);
	DOOF(abts_rsp_t, abts_rsp_status);
	DOOF(abts_rsp_t, abts_rsp_nphdl);
	DOOF(abts_rsp_t, abts_rsp_ctl_flags);
	DOOF(abts_rsp_t, abts_rsp_sof);
	DOOF(abts_rsp_t, abts_rsp_rxid_abts);
	DOOF(abts_rsp_t, abts_rsp_did_lo);
	DOOF(abts_rsp_t, abts_rsp_did_hi);
	DOOF(abts_rsp_t, abts_rsp_r_ctl);
	DOOF(abts_rsp_t, abts_rsp_sid_lo);
	DOOF(abts_rsp_t, abts_rsp_sid_hi);
	DOOF(abts_rsp_t, abts_rsp_cs_ctl);
	DOOF(abts_rsp_t, abts_rsp_f_ctl_lo);
	DOOF(abts_rsp_t, abts_rsp_f_ctl_hi);
	DOOF(abts_rsp_t, abts_rsp_type);
	DOOF(abts_rsp_t, abts_rsp_seq_cnt);
	DOOF(abts_rsp_t, abts_rsp_df_ctl);
	DOOF(abts_rsp_t, abts_rsp_seq_id);
	DOOF(abts_rsp_t, abts_rsp_rx_id);
	DOOF(abts_rsp_t, abts_rsp_ox_id);
	DOOF(abts_rsp_t, abts_rsp_param);
	DOOF(abts_rsp_t, abts_rsp_payload);
	DOPRT(abts_rsp_t, abts_rsp_payload.ba_acc);
	DOPRT(abts_rsp_t, abts_rsp_payload.ba_rjt);
	DOPRT(abts_rsp_t, abts_rsp_payload.reserved);
	DOOF(abts_rsp_t, abts_rsp_rxid_task);
	putchar('\n');
	return (0);
}
