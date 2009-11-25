# $Id: Makefile,v 1.20 2008/11/29 20:13:50 mjacob Exp $
#
# Copyright (c) 2006-2008 by Matthew Jacob
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of The Version 2 GNU General Public License as published
#  by the Free Software Foundation.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
# 
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#
# Matthew Jacob
# Feral Software
# 421 Laurel Avenue
# Menlo Park, CA 94025
# USA
#
# gpl at feral com
M			?=	$(CURDIR)
TDIR			=	$(M)/../../
COM_OBJ 		=	isp.o isp_library.o isp_target.o
LNX_OBJ 		=	isp_cb_ops.o isp_linux.o isp_pci.o
COM_DIR			=	${TDIR}/common
LNX_DIR			=	${TDIR}/linux
XXX_DIR			=	${TDIR}/tools
MOD_NAME		?=	isp

STG_SRC			=	scsi_target_verify.c scsi_target_ctl.c mid_target.c
XXX_SRC			:=	$(notdir $(wildcard ${XXX_DIR}/*.c))

EXTRA_CFLAGS		+=	-g -Wall -Werror -Idrivers/scsi -I${COM_DIR} -I${LNX_DIR} -I${TDIR}/firmware -DISP_NAME=\"${MOD_NAME}\"

ifdef ISP_TARGET_MODE

EXTRA_CFLAGS		+=	-DISP_TARGET_MODE=1 -DISP_DEFAULT_ROLES=ISP_ROLE_TARGET

obj-m			:=	${MOD_NAME}_mod.o isp_scsi_target.o mid_target.o
isp_scsi_target-objs	:=	scsi_target_verify.o

else

obj-m			:=	${MOD_NAME}_mod.o

always			:=	$(hostprogs-y)

endif

HOSTCFLAGS		+=	-Wall -Werror -D_LARGEFILE_SUPPORT=1 -D_LARGEFILE64_SUPPORT -D_FILE_OFFSET_BITS=64 -DISP_TOOLS=1 -I${LNX_DIR} -I$(COM_DIR)
hostprogs-y		:=	$(basename ${XXX_SRC}) scsi_target_ctl
always			:=	$(hostprogs-y)

clean-files		:=	*.o ${hostprogs-y} Module.symvers


${MOD_NAME}_mod-objs		:=	${COM_OBJ} ${LNX_OBJ}

#
clean_links:
	@${RM} $(COM_OBJ:.o=.c) $(LNX_OBJ:.o=.c) ${STG_SRC} $(XXX_SRC)

make_links: clean_links
	@for j in ${COM_OBJ:.o=.c}; do ln -s ${COM_DIR}/$$j;  done
	@for j in ${LNX_OBJ:.o=.c}; do ln -s ${LNX_DIR}/$$j;  done
	@for j in ${STG_SRC}; do ln -s ${LNX_DIR}/$$j;  done
	@for j in ${XXX_SRC}; do ln -s ${XXX_DIR}/$$j;  done

install_host_progs:
	mkdir -p /opt/scsi_tgt
	cp $(hostprogs-y) /opt/scsi_tgt
