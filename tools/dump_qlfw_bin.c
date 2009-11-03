/*
 * Copyright (c) 1997-2008 by Matthew Jacob
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *
 * Alternatively, this software may be distributed under the terms of the
 * the GNU Public License ("GPL") with platforms where the prevalant license
 * is the GNU Public License:
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the Version 2 GNU General Public License as
 *  published by the Free Software Foundation
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 * Matthew Jacob
 * Feral Software
 * 421 Laurel Avenue
 * Menlo Park, CA 94025
 * USA
 *
 * gplbsd at feral com
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>

int
main(int a, char **v)
{
	int fd;
	char x;
	int wdsize;
	int i = 0;

	if (a != 3) {
usage:
		printf("Usage: dump_qlfw_bin {2|4} binary-file\n");
		return (1);
	}
	fd = open(v[2], O_RDONLY);
	if (fd < 0) {
		perror(v[2]);
		return (1);
	}
	wdsize = atoi(v[1]);
	if (wdsize != 2 && wdsize != 4) {
		goto usage;
	}
	while (read(fd, &x, 1) == 1) {
		if (i == 0) {
			printf("0x");
		}
		printf("%02x", x & 0xff);
		if (++i == wdsize) {
			i = 0;
			putchar('\n');
		}
	}
	return (0);
}
