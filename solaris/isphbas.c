/* $Id: isphbas.c,v 1.10 2008/02/11 23:59:07 mjacob Exp $ */
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
 * make isphbas LDLIBS=-ldevinfo -ldevid
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mkdev.h>
#include <dirent.h>
#include <libdevinfo.h>
#include <stdio.h>

#define	DI_INIT_FLAGS (DINFOSUBTREE | DINFOMINOR | DINFOPROP)

static int 
find_hbas(di_node_t node, void *arg)
{
	DIR *Dp;
	char *dname, *nname, *bname;
	uchar_t *pd;


	bname = di_binding_name(node);
	if (strncmp(bname, "SUNW,isp", 8) != 0 &&
	    strncmp(bname, "QLGC,isp", 8) != 0 &&
	    strncmp(bname, "pci1077,", 8) != 0) {
		return(DI_WALK_CONTINUE);
	}

	/*
	 * Snag all nodes with a binding name starting as
	 *	SUNW,isp
	 *	QLGC,isp
	 *	pci1077,
	 */
	nname = di_node_name(node);

	/*
	 * Throw out all nodes that have a non-zero state.
	 */
	if (di_state(node) != 0) {
		return(DI_WALK_CONTINUE);
	}

	/*
	 * Throw out all nodes that don't have a bus address. (cannothappen).
	 */
	if (di_bus_addr(node) == NULL) {
		return(DI_WALK_CONTINUE);
	}

	/*
	 * We should now be able to print out the path of the
	 * device. With a :devctl appended, in theory we
	 * should have a /devices name to use. In practice,
	 * we seem to have something like pci@SLOT interpolated
	 * as the parent. Lame.
	 */
	dname = di_devfs_path(node);

	printf("/devices%s:devctl\n", dname);
	di_devfs_path_free(dname);
	return(DI_WALK_PRUNECHILD);
}

int
main(int a, char **v)
{
	int c;
	di_node_t root_node;

	if ((root_node = di_init("/", DI_INIT_FLAGS)) == DI_NODE_NIL) {
		perror ("di_init");
		return(1);
	}
	di_walk_node(root_node, DI_WALK_CLDFIRST, NULL, find_hbas);
	di_fini(root_node);
	return(0);
}
