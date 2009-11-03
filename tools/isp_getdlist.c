/* $Id: isp_getdlist.c,v 1.7 2009/05/06 22:51:30 mjacob Exp $ */
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
#include <fcntl.h>
#include <errno.h>
#include <memory.h>
#include <sys/ioctl.h>
#include "isp_ioctl.h"

static void
process(char *filename)
{
	int i;
	union {
		char buf[8192];
		struct isp_hba_device dev;
		isp_dlist_t list;
	} dl;
	int fd, ch, nc;

	fd = open(filename, O_RDONLY);
	if (fd < 0) {
		perror(filename);
		return;
	}

	memset(&dl, 0, sizeof (dl));
	if (ioctl(fd, ISP_FC_GETHINFO, &dl.dev) < 0) {
		perror("ISP_FC_GETHINFO");
		(void) close(fd);
		return;
	}
	nc = dl.dev.fc_nchannels;
	for (ch = 0; ch < nc; ch++) {
		memset(&dl, 0, sizeof (dl));
		dl.list.count = (sizeof (dl) / sizeof (dl.list.wwns[0])) - 1;
		dl.list.channel = ch;
		if (ioctl(fd, ISP_FC_GETDLIST, &dl) < 0) {
			if (errno != ENODEV) {
				perror("ISP_FC_GETDLIST");
				(void) close(fd);
				return;
			}
			continue;
		}
		fprintf(stdout, "%s Chan %d: %d wwns\n", filename, ch, dl.list.count);
		for (i = 0; i < dl.list.count; i++) {
			fprintf(stdout, "\tWWNN=0x%016llx WWPN=0x%016llx\n",
			    (unsigned long long) dl.list.wwns[i].wwnn,
			    (unsigned long long) dl.list.wwns[i].wwpn);
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
