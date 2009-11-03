/* $Id: scsi_target_ctl.c,v 1.24 2009/03/30 04:16:30 mjacob Exp $ */
/*
 *  Copyright (c) 1997-2009 by Matthew Jacob
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
 * SCSI Target Mode "stub" control program for Linux.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <syslog.h>
#include <sys/ioctl.h>
#ifndef USERSPACE
#define USERSPACE
#include "scsi_target.h"
#endif

#define dprintf if (debug) printf

const char usage[] = "usage: %s\n\
 scsi_target_ctl debug level\n\
 scsi_target_ctl enable hba-name-unit channel lun nbytes\n\
 scsi_target_ctl disable hba-name-unit channel lun\n";

static uint64_t szarg(char *);
static int debug;

int
main(int a, char **v)
{
    union {
        sc_enable_t _x;
        int         _y;
    } x;
    int fd, action, dd = 0;
    char *progname;

    if (v) {
        progname = v[0];
        if (progname == NULL) {
            return (1);
        }
    } else {
        return (1);
    }

    debug = getenv("DEBUG") != NULL;

    if (a < 3 || a > 7) {
 usage:
        fprintf(stderr, usage, progname);
        return (1);
    }

    memset(&x, 0, sizeof (x));
    if (strcmp(v[1], "enable") == 0) {
        if (a != 6) {
            goto usage;
        }
        action = SC_ENABLE_LUN;
        strncpy(x._x.hba_name_unit, v[2], sizeof (x._x.hba_name_unit));
        x._x.channel = atoi(v[3]);
        x._x.lun = atoi(v[4]);
        x._x.nbytes = szarg(v[5]);
    } else if (strcmp(v[1], "disable") == 0) {
        if (a != 5) {
            goto usage;
        }
        action = SC_DISABLE_LUN;
        strncpy(x._x.hba_name_unit, v[2], sizeof (x._x.hba_name_unit));
        x._x.channel = atoi(v[3]);
        x._x.lun = atoi(v[4]);
    } else if (strcmp(v[1], "debug") == 0) {
        if (a != 3) {
            goto usage;
        }
        action = SC_DEBUG;
        dd = x._y = atoi(v[2]);
    } else {
        goto usage;
    }


    if ((fd = open(SCSI_TARGET_DEV, O_RDWR)) < 0) {
        perror(SCSI_TARGET_DEV);
        return (1);    
    }

    if (ioctl(fd, action, &x) < 0) {
        perror(v[1]);
        return (2);
    }
    if (action == SC_DEBUG) {
        printf("old debug level: %d; new debug level %d\n", x._y, dd);
    }
    return (0);
}

static uint64_t
szarg(char *n)
{
    uint64_t result;
    char *q = n;

    while (isxdigit(*q)) {
        q++;
    }
    result = (uint64_t) strtoull(n, NULL, 0);
    if (*q == '\0') {
        return (result);
    }
    if (strcasecmp(q, "kib") == 0) {
        result <<= 10;
    } else if (strcasecmp(q, "mib") == 0) {
        result <<= 20;
    } else if (strcasecmp(q, "gib") == 0) {
        result <<= 30;
    } else if (strcasecmp(q, "tib") == 0) {
        result <<= 40;
    } else if (strcasecmp(q, "pib") == 0) {
        result <<= 50;
    } else if (strcasecmp(q, "k") == 0) {
        result <<= 10;
    } else if (strcasecmp(q, "m") == 0) {
        result <<= 20;
    } else if (strcasecmp(q, "g") == 0) {
        result <<= 30;
    } else if (strcasecmp(q, "t") == 0) {
        result <<= 40;
    } else if (strcasecmp(q, "p") == 0) {
        result <<= 50;
    } else if (strcasecmp(q, "kb") == 0) {
        result *= 1000;
    } else if (strcasecmp(q, "mb") == 0) {
        result *= 1000000;
    } else if (strcasecmp(q, "gb") == 0) {
        result *= 1000000000ULL;
    } else if (strcasecmp(q, "tb") == 0) {
        result *= 1000000000000ULL;
    } else if (strcasecmp(q, "pb") == 0) {
        result *= 1000000000000000ULL;
    }
    return (result);
}
/*
 * vim:ts=4:sw=4:expandtab
 */
