#
#        Copyright (C) 2000-2020 the YAMBO team
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
ifeq ($(wildcard config/mk/defs.mk),config/mk/defs.mk)
 include config/mk/defs.mk
else
 $(error Run ./configure first)
endif
#
# STAMPS and CFGFILES
include config/mk/variables.mk
#
# Targets
include config/mk/targets.mk
#
# Libraries (ordered for compiling & linking)
include config/mk/libraries.mk

.PHONY: interfaces

nothing: 
	@$(make_message)
changelog:
	./sbin/gitchangelog.py > ChangeLog
interfaces: ext-libs
	for target in $(INTERFCS) ; do rm -f "$(bindir)/$$target" ; $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
core: ext-libs
	for target in $(CORE)     ; do rm -f "$(bindir)/$$target" ; $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
ph-project: ext-libs
	for target in $(PH_PROJ)  ; do rm -f "$(bindir)/$$target" ; $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
sc-project: ext-libs
	for target in $(SC_PROJ)  ; do rm -f "$(bindir)/$$target" ; $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
mag-project: ext-libs
	for target in $(MAG_PROJ) ; do rm -f "$(bindir)/$$target" ; $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
rt-project: ext-libs
	for target in $(RT_PROJ)  ; do rm -f "$(bindir)/$$target" ; $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
nl-project: ext-libs
	for target in $(NL_PROJ)  ; do rm -f "$(bindir)/$$target" ; $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
rtext-project: ext-libs
	for target in $(RTE_PROJ) ; do rm -f "$(bindir)/$$target" ; $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
kerr-project: ext-libs
	for target in $(KERR_PROJ); do rm -f "$(bindir)/$$target" ; $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
all: ext-libs 
	for target in $(ALL)      ; do rm -f "$(bindir)/$$target" ; $(MAKE) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
#
#==============
# COMPILATIONS
#==============
#
# External libraries
include config/mk/actions/compile_external_libraries.mk
#
# Internal libraries
include config/mk/actions/compile_internal_libraries.mk
#
# Yambo libs 
include config/mk/actions/compile_yambo_libraries.mk
#
# All libs 
libs:	ext-libs int-libs yambo-libs
#
# Yambo 
include config/mk/actions/compile_yambo.mk
#
# Interfaces #
include config/mk/actions/compile_interfaces.mk
#
# YPP 
include config/mk/actions/compile_ypp.mk
#
# Cleans
include config/mk/actions/clean.mk
#
#===========
# Functions
#===========
#
# Libraries download/clone/checkout
include config/mk/functions/get_libraries.mk
#
# Messages
include config/mk/functions/messaging.mk
#
# SRC functions
include config/mk/functions/mk_src.mk
include config/mk/functions/mk_driver_src.mk
include config/mk/functions/mk_ypp_src.mk
#
# LIBs (locks driven)...
# ... internal
include config/mk/functions/mk_internal_lib.mk
# ... external
include config/mk/functions/mk_external_lib.mk
#
# Yambo LIBS
include config/mk/functions/mk_external_yambo_lib.mk
#
# Final exe's
include config/mk/functions/mk_exe.mk
#
# CLEANING
include config/mk/functions/cleaning.mk
