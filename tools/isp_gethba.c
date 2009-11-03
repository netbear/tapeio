/* $Id: isp_gethba.c,v 1.15 2009/05/06 22:51:30 mjacob Exp $ */
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
    "%s, channel %d:\n"
    "\tFW Revision: %d.%d.%d\n"
    "\tSpeed %d Gbps, topology %s, loopid %d\n"
    "\tWWNN %#llx WWPN %#llx\n";

static const char fmt1[] =
    "%s, channel %d:\n"
    "\tFW Revision: %d.%d.%d\n"
    "\tSpeed %d Gbps, topology %s\n"
    "\tWWNN %#llx WWPN %#llx\n";

static const char fmt2[] =
    "%s, channel %d:\n"
    "\tFW Revision: %d.%d.%d\n"
    "\tSpeed n/a, topology n/a, loopid n/a\n"
    "\tWWNN n/a WWPN n/a\n";

static const char *topos[8] = {
    "unknown", "private loop", "public loop", "N-port",
    "F-port", "bad5", "bad6", "bad7"
};
		


static void
process(char *filename)
{
	struct isp_hba_device hba;
	int fd, chan;

	fd = open(filename, O_RDONLY);
	if (fd < 0) {
		perror(filename);
		return;
	}

	chan = 0;
	do {
		memset(&hba, 0, sizeof (hba));
		hba.fc_channel = chan;
		if (ioctl(fd, ISP_FC_GETHINFO, (caddr_t)&hba)) {
			perror("ISP_FC_GETHINFO");
			break;
		}
		if (hba.fc_speed) {
			if (hba.fc_topology == ISP_TOPO_NPORT || hba.fc_topology == ISP_TOPO_FPORT) {
				fprintf(stdout, fmt1, filename, chan++, hba.fc_fw_major, hba.fc_fw_minor, hba.fc_fw_micro,
				    hba.fc_speed * 1062, topos[hba.fc_topology], (unsigned long long)hba.active_node_wwn, (unsigned long long)hba.active_port_wwn);
			} else {
				fprintf(stdout, fmt, filename, chan++, hba.fc_fw_major, hba.fc_fw_minor, hba.fc_fw_micro,
				    hba.fc_speed * 1062, topos[hba.fc_topology], hba.fc_loopid, (unsigned long long)hba.active_node_wwn, (unsigned long long)hba.active_port_wwn);
			}
		} else {
			fprintf(stdout, fmt2, filename, chan++, hba.fc_fw_major, hba.fc_fw_minor, hba.fc_fw_micro);
		}
	} while (chan < hba.fc_nchannels);
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
