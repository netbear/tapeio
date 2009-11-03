/* $Id: isp_setrole.c,v 1.16 2009/05/06 22:51:30 mjacob Exp $ */
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
#include <string.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include "isp_ioctl.h"




int
main(int a, char **v)
{
	static char *roles[] = {
		"none", "target", "initiator", "both"
	};
	int nrole, role, fd, chan, sc, ec, avi;

	if (a != 3 && a != 4) {
usage:
		fprintf(stderr,
		    "usage: %s hba [chan] {target|initiator|both|none}\n", *v);
		return(1);
	}
	if (a == 3) {
		avi = 2;
	} else {
		avi = 3;
	}

	if (strncmp(v[avi], "target", 4) == 0) {
		role = ISP_ROLE_TARGET;
	} else if (strncmp(v[avi], "initiator", 4) == 0) {
		role = ISP_ROLE_INITIATOR;
	} else if (strncmp(v[avi], "none", 4) == 0) {
		role = ISP_ROLE_NONE;
	} else if (strncmp(v[avi], "both", 4) == 0) {
		role = ISP_ROLE_BOTH;
	} else {
		goto usage;
	}
	fd = open(v[1], O_RDWR);
	if (fd < 0) {
		perror(v[1]);
		return (1);
	}
	if (a == 4) {
		sc = atoi(v[2]);
		ec = sc + 1;
	} else {
		struct isp_hba_device dev;
		memset(&dev, 0, sizeof (dev));
		if (ioctl(fd, ISP_FC_GETHINFO, &dev) < 0) {
			perror("ISP_FC_GETHINFO");
			(void) close(fd);
			return (1);
		}
		sc = 0;
		ec = dev.fc_nchannels;
	}
	for (chan = sc; chan < ec; chan++) {
		nrole = role | (chan << 16);
		if (ioctl(fd, ISP_SETROLE, (caddr_t)&nrole) < 0) {
			perror("ISP_SETROLE");
			(void) close(fd);
			return (1);
		}
		fprintf(stdout, "%s chan %d: old role %s, new role %s\n",
		    v[1], chan, roles[nrole & 0xffff], roles[role]);
	}
	(void) close(fd);
	return(0);
}
