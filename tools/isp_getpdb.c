/* $Id: isp_getpdb.c,v 1.17 2009/02/01 23:49:57 mjacob Exp $ */
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
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <memory.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/ioctl.h>
#include "isp_ioctl.h"

static const char fmt[] =
    " [%4u]: %s@0x%02x (0x%06x) WWNN 0x%08x%08x WWPN 0x%08x%08x\n";

static const char *
fcp_role(int role)
{
	const char *roles[] = {
		" UNK",
		" TGT",
		" INI",
		"TINI",
	};
	return (roles[role&0x3]);
}

static void
process(char *filename)
{
	struct isp_hba_device hba;
	struct isp_fc_device fcp;
	int fd, chan;
	uint16_t idx;
	uint8_t nchan;

	fd = open(filename, O_RDONLY);
	if (fd < 0) {
		perror(filename);
		return;
	}
	
	memset(&hba, 0, sizeof (hba));
	if (ioctl(fd, ISP_FC_GETHINFO, &hba) < 0) {
		fprintf(stderr, "%s: ISP_FC_GETHINFO failed(%s)\n", filename, strerror(errno));
		(void) close(fd);
		return;
	}

	nchan  = hba.fc_nchannels;
	for (chan = 0; chan < nchan; chan++) {
		memset(&hba, 0, sizeof (hba));
		hba.fc_channel = chan;
		if (ioctl(fd, ISP_FC_GETHINFO, &hba) < 0) {
			fprintf(stderr, "%s: ISP_FC_GETHINFO failed on chan %u (%s)\n", filename, chan, strerror(errno));
			break;
		}
		fprintf(stdout, "%s, Chan %u %d ports\n", filename, chan, hba.fc_nports);
		for (idx = 0; idx < hba.fc_nports; idx++) {
			memset(&fcp, 0, sizeof (fcp));
			fcp.loopid = idx;
			fcp.chan = chan;
			if (ioctl(fd, ISP_FC_GETDINFO, &fcp) < 0) {
				continue;
			}
			fprintf(stdout, fmt, idx, fcp_role(fcp.role), fcp.loopid, fcp.portid, (uint32_t)(fcp.node_wwn >> 32),
			    (uint32_t)fcp.node_wwn, (uint32_t)(fcp.port_wwn >> 32), (uint32_t)fcp.port_wwn);
		}
	}
	(void) close(fd);
}

int
main(int a, char **v)
{
	while (*++v) {
		process(*v);
	}
	return(0);
}
