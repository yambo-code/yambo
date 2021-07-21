#
#        Copyright (C) 2000-2021 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM, DS
#
# This file is distributed under the terms of the GNU
# General Public License. You can redistribute it and/or
# modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation;
# either version 2, or (at your option) any later version.
#
# This program is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
# MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
#
ifeq ($(wildcard config/mk/global/defs.mk),config/mk/global/defs.mk)
 include config/mk/global/defs.mk
else ifeq ($(MAKECMDGOALS), download)
else ifeq ($(MAKECMDGOALS), check)
else
 include config/mk/global/no_configure_help.mk
endif
#
ifndef VERBOSE
 MAKEFLAGS += --no-print-directory
endif
#
# Targets
include config/mk/global/targets.mk
#
# Libraries (ordered for compiling & linking)
include config/mk/global/libraries.mk

.PHONY: interfaces 

nothing: 
	@$(make_message)
changelog:
	./sbin/gitchangelog.py > ChangeLog
interfaces: ext-libs
	@for target in $(INTERFCS) ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
gpl: ext-libs
	@for target in $(GPL)      ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
core: ext-libs
	@for target in $(CORE)     ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
ph-project: ext-libs
	@for target in $(PH_PROJ)  ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
sc-project: ext-libs
	@for target in $(SC_PROJ)  ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
mag-project: ext-libs
	@for target in $(MAG_PROJ) ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
rt-project: ext-libs
	@for target in $(RT_PROJ)  ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
nl-project: ext-libs
	@for target in $(NL_PROJ)  ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
rtext-project: ext-libs
	@for target in $(RTE_PROJ) ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
kerr-project: ext-libs
	@for target in $(KERR_PROJ); do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
main: ext-libs 
	@for target in $(MAIN)     ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
all: ext-libs 
	@for target in $(ALL)      ; do $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
ext-libs:
	@for target in $(EXT_LIBS) ; do if ! test "$$target" = Ydriver; then $(MAKE) $$target; fi; done
int-libs: dependencies migration
	@for target in $(INT_LIBS) ; do $(MAKE) $$target; done
yambo-int-libs: 
	@for target in $(YAMBO_INT_LIBS) ; do $(MAKE) $$target; done
#
#=====================
# DOUBLE PRECISION?
#=====================
#
ifeq ($(wildcard config/stamps_and_lists/compiling_ypp_nl.stamp),config/stamps_and_lists/compiling_ypp_nl.stamp)
 DOUBLE_PRECMP=-D_DOUBLE
else ifeq ($(wildcard config/stamps_and_lists/compiling_yambo_nl.stamp),config/stamps_and_lists/compiling_yambo_nl.stamp)
 DOUBLE_PRECMP=-D_DOUBLE
endif
#
#==============
# COMPILATIONS
#==============
#
# External libraries
include config/mk/global/actions/download_external_libraries.mk
include config/mk/global/actions/compile_external_libraries.mk
#
# Internal libraries
include config/mk/global/actions/compile_internal_libraries.mk
#
# Yambo libs 
include config/mk/global/actions/compile_yambo_libraries.mk
#
# All libs 
libs: ext-libs int-libs
#
# Yambo 
include config/mk/global/actions/compile_yambo.mk
#
# Interfaces #
include config/mk/global/actions/compile_interfaces.mk
#
# YPP 
include config/mk/global/actions/compile_ypp.mk
#
# Cleans
include config/mk/global/actions/clean.mk
#
# Utils
include config/mk/global/actions/help.mk
#
#===========
# Functions
#===========
#
# Global Configuration Check (DOUBLE, FFTW ...)
include config/mk/global/functions/global_check.mk
#
# Libraries download/clone/checkout
include config/mk/global/functions/get_libraries.mk
#
# Internal code dependencies
include config/mk/global/actions/dependencies.mk
#
# New sources to be compiled
include config/mk/global/functions/todo.mk
#
# Remote compilation
include config/mk/global/actions/remote_compilation.mk
#
# Messages
include config/mk/global/functions/messaging.mk
#
# LIBs ...
# ... internal
include config/mk/global/functions/mk_lib.mk
# ... external
include config/mk/global/functions/mk_external_lib.mk
#
# Linker
include config/mk/global/functions/mk_exe.mk
#
# CLEANING
include config/mk/global/functions/cleaning.mk
