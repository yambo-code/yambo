#
# License-Identifier: GPL
#
# Copyright (C) 2006 The Yambo Team
#
# Authors (see AUTHORS file for details): AM DS
#
ifeq ($(wildcard config/mk/global/defs.mk),config/mk/global/defs.mk)
 include config/mk/global/defs.mk
 include config/mk/defs.mk
else ifeq ($(MAKECMDGOALS), download)
else ifeq ($(MAKECMDGOALS), check-files)
else ifeq ($(MAKECMDGOALS), check-packages)
else
 include config/mk/global/no_configure_help.mk
endif
#
MAKEFLAGS = --no-print-directory
#
# Targets
include config/mk/global/targets.mk
#
# Libraries (ordered for compiling & linking)
include config/mk/global/libraries.mk

.PHONY: interfaces ypp 

nothing: 
	@$(call yambo_help,"header")
help:           
	@$(call yambo_help,"$(what)")
changelog:
	./sbin/gitchangelog.py > ChangeLog
interfaces:
	@for target in $(INTERFCS) ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
gpl:
	@for target in $(GPL)      ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
core:
	@for target in $(CORE)     ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
ph-project:
	@for target in $(PH_PROJ)  ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
sc-project:
	@for target in $(SC_PROJ)  ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
mag-project: 
	@for target in $(MAG_PROJ) ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
rt-project:
	@for target in $(RT_PROJ)  ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
nl-project: 
	@for target in $(NL_PROJ)  ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
rtext-project:
	@for target in $(RTE_PROJ) ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
mod-project:
	@for target in $(MOD_PROJ) ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
kerr-project:
	@for target in $(KERR_PROJ); do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
main:
	@for target in $(MAIN)     ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
all:
	@for target in $(ALL)      ; do $(MAKE) $(MAKEFLAGS) $$target; if test ! -f "$(bindir)/$$target"; then echo "$$target build failed"; break;fi ; done
ext-libs:
	@for target in $(EXT_LIBS) ; do if ! test "$$target" = Ydriver; then $(MAKE) $(MAKEFLAGS) $$target; fi; done
int-libs:
	@for target in $(INT_LIBS) ; do $(MAKE) $(MAKEFLAGS) $$target; done
yambo-int-libs: 
	@for target in $(YAMBO_INT_LIBS) ; do $(MAKE) $(MAKEFLAGS) $$target; done
check-packages:
	@$(global_check)
#
#=====================
# DOUBLE PRECISION?
#=====================
#
# int-libs are compiled via an internal make call where MAKECMDGOALS is the lib itself.
#
STAMP_DBLE=
ifneq (,$(wildcard $(compdir)/config/stamps_and_lists/compilation_objects_in_DOUBLE_precision.stamp))
 STAMP_DBLE=-D_DOUBLE
else ifneq (,$(wildcard $(compdir)/config/stamps_and_lists/compiling_yambo_nl.stamp))
 STAMP_DBLE=-D_DOUBLE
else ifneq (,$(wildcard $(compdir)/config/stamps_and_lists/compiling_ypp_nl.stamp))
 STAMP_DBLE=-D_DOUBLE
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
# Interfaces 
include config/mk/global/actions/compile_interfaces.mk
#
# YPP 
include config/mk/global/actions/compile_ypp.mk
#
# Cleans
include config/mk/global/actions/clean.mk
#
#===========
# Functions
#===========
#
# Global Configuration Check 
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
# Messages
include config/mk/global/functions/help.mk
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
