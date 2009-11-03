# $Id: Makefile,v 1.23 2008/06/16 23:54:25 mjacob Exp $
#
#  Copyright (c) 1997-2008 by Matthew Jacob
#  All rights reserved.
# 
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
# 
#   1. Redistributions of source code must retain the above copyright
#      notice, this list of conditions, and the following disclaimer,
#      without modification, immediately at the beginning of the file.
#   2. The name of the author may not be used to endorse or promote products
#      derived from this software without specific prior written permission.
# 
#   THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
#   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#   ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
#   ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#   OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
#   OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
#   SUCH DAMAGE.
# 
#  Alternatively, this software may be distributed under the terms of the
#  the GNU Public License ("GPL") with platforms where the prevalant license
#  is the GNU Public License:
# 
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of The Version 2 GNU General Public License as published
#   by the Free Software Foundation.
# 
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#  
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
# 
# 
#  Matthew Jacob
#  Feral Software
#  421 Laurel Avenue
#  Menlo Park, CA 94025
#  USA
# 
#  isplicense at feral com
#
# Qlogic ISP Host Adapter Build File
#

# Building prior to Solaris 10 is no longer supported.
# This supports only sparc64 and amd64.
#
# Platform information
#
ARCH:sh	=	if [ `uname -p` = sparc ]; then echo sparc64; elif [ `uname -p` = i386 ]; then echo amd64 ; fi
Rel:sh	= 	uname -r | sed 's/\./_/g'
RDEFS	=	-DREL${Rel}
BLDDIR	=	$(ARCH)
VPATH	=	.:../common

#
# Solaris defines
#
#
DFLAGS		=
KDEFS		= -D_KERNEL -DSYSACCT -DC2_AUDIT -DQUOTA -D_SYSCALL32 -D_SYSCALL32_IMPL -D_DDI_STRICT -Dsun -D__SVR4 -D_MACHDEP -D__svr4__
KDEFS		+= -I/usr/platform/`uname -m`/include

MF:sh		= kcflags

KCFLAGS_amd64	= ${MF} -Ui386 -U__i386 -Damd64 -Xa -xspace  -xmodel=kernel -g -xc99=no_lib

KCFLAGS_sparc64	= ${MF} -Wc,-xcode=abs32 -xarch=v9 -xO3 -Xa -xspace -W0,-Lt -v -dalign -D__sparcv9cpu -D__sparc__ -Dsparc -xregs=no%appl,no%float -D_ELF64

CDIR	=	../common
FDIR	=	../firmware
TDIR	=	../tools

#
# CFLAGS definition
#
MOD		=	misp
MODNAME		=	misp
OBJS		=	isp_attach.o isp_cb_ops.o isp_dma.o isp_reg.o \
			isp_scsa.o isp_solaris.o isp_firmware.o isp.o \
			isp_library.o
KHDR		=	isp_ioctl.h isp_scsa.h isp_solaris.h \
			${CDIR}/isp_library.h ${CDIR}/isp_target.h \
			${CDIR}/isp_tpublic.h ${CDIR}/ispmbox.h \
			${CDIR}/ispreg.h ${CDIR}/ispvar.h
FW		=	${FDIR}/asm_1000.h ${FDIR}/asm_1040.h \
			${FDIR}/asm_1080.h ${FDIR}/asm_12160.h \
			${FDIR}/asm_2100.h ${FDIR}/asm_2200.h \
			${FDIR}/asm_2300.h
INCS		=	-I. -I${CDIR} -I${FDIR} ${INC_${ARCH}}
CFLAGS		=	${KCFLAGS_$(ARCH)} ${INCS} ${KDEFS} ${RDEFS} $(DFLAGS)

KDEFS		+=	-DMODNAME=\"${MODNAME}\"
ARCH_OBJS	=	$(OBJS:%=$(BLDDIR)/%)
ARCH_MOD	=	$(MOD:%=$(BLDDIR)/$(MOD))
MOD_CONF	=	$(MOD:%=%.conf)

LDFLAGS		=	-N misc/scsi -r -dy
LDFLAGS_amd64	=	-64

all:	$(MOD)

$(MOD): $(BLDDIR) $(ARCH_MOD)

$(ARCH_MOD): $(ARCH_OBJS)
	${LD} ${LDFLAGS_${ARCH}} ${LDFLAGS} -o $(ARCH_MOD) $(ARCH_OBJS)

debug:= DFLAGS = -DDEBUG=1
debug:	$(MOD)

FLIST=	isp_attach.c isp_cb_ops.c isp_dma.c isp_reg.c isp_scsa.c isp_solaris.c

tags:	FRC
	ctags -w *.[ch] ../common/*.[ch]

$(BLDDIR)/%.o: %.c ${KHDR} ${FW}
	$(CC) $(CFLAGS) -c -o $@ $<

$(BLDDIR):
	@/usr/sbin/install -s -d -m 777 $@

#
# Clean Rules
#
clean:
	@$(RM) -rf sparc* i386* amd64* *.ln isp.c isp_library.c

reallyclean:
	$(RM) tags

FRC:
