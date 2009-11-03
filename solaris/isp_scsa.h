/* $Id: isp_scsa.h,v 1.13 2008/04/15 22:41:09 mjacob Exp $ */
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
#ifndef _ISP_SCSA_H
#define	_ISP_SCSA_H

#ifdef	__cplusplus
extern "C" {
#endif

#include <sys/scsi/scsi_types.h>
#ifndef	sparc
#define	PKT_PRIV_LEN		48
#else
#define	PKT_PRIV_LEN		(2 * sizeof (void *))
#endif

#define	PKT2CMD(pkt)		((isp_cmd_t *) pkt)
#define	CMD2PKT(sp)		((struct scsi_pkt *) sp)

/*
 * Wrapper around scsi_pkt.
 */
typedef struct isp_cmd {
	struct scsi_pkt		cmd_pkt;	/* actual SCSI Packet */
	struct scsi_arq_status	cmd_arqstatus;	/* ARQ status */
	struct isp_cmd *	cmd_next;	/* for waitqs and doneqs only */
	timeout_id_t		cmd_timer;	/* timeout handle for cmd */
	ddi_dma_handle_t	cmd_dmahandle;	/* dma handle */
	ddi_dma_cookie_t	cmd_dmacookie;	/* dma cookie(s) */
	ssize_t			cmd_dmacount;	/* dma count */
	uint8_t 		cmd_cdb[16];	/* 'generic' Sun cdb */
	uint8_t			cmd_pkt_private[PKT_PRIV_LEN];
	uint16_t		cmd_ccnt;	/* count of dma cookies */
	uint16_t		cmd_flags;	/* private flags */
	uint_t			cmd_cdblen;	/* length of cdb */
	uint_t			cmd_scblen;	/* length of scb */
	uint_t			cmd_privlen;	/* length of tgt private */
} isp_cmd_t;


/*
 * These are the defined flags for this structure.
 */
#define	CFLAG_RUNNING		0x0001	/* command actually running */
#define	CFLAG_ABORTING		0x0002	/* packet is being aborted */
#define	CFLAG_DMASEND		0x0004	/* data is going 'out' */
#define	CFLAG_CMDIOPB		0x0008	/* this is an 'iopb' packet */
#define	CFLAG_CDBEXTERN		0x0010	/* cdb kmem_alloc'd */
#define	CFLAG_SCBEXTERN		0x0020	/* scb kmem_alloc'd */
#define	CFLAG_PRIVEXTERN	0x0040	/* target private was kmem_alloc'd */
#define	CFLAG_DMA_MAPPED	0x0080	/* dma handle is mappeed */

/*
 * Timer stuff..
 */
#define	START_TIMER(sp)	\
	if (CMD2PKT(sp)->pkt_time) { \
		(sp)->cmd_timer = timeout(isp_wdog, sp, \
		    drv_usectohz(1000000) * (CMD2PKT(sp)->pkt_time + 3)); \
	} else {					\
		(sp)->cmd_timer = timeout(isp_wdog, sp, \
		    drv_usectohz(1000000) * 2); \
	}

#define	STOP_TIMER(sp)	\
	if ((sp)->cmd_timer) { \
		untimeout((sp)->cmd_timer); \
		(sp)->cmd_timer = 0; \
	}

#define	RESTART_TIMER(sp)	\
	STOP_TIMER(sp); \
	START_TIMER(sp);

#define	GOOSE_TIMER(sp)		\
	STOP_TIMER(sp); \
	(sp)->cmd_timer = timeout(isp_wdog, sp, drv_usectohz(1000000))

#define	FALSE		0
#define	TRUE		1
#define	UNDEFINED	-1

#ifdef	__cplusplus
}
#endif
#endif
