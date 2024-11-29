#
# License-Identifier: GPL
#
# Copyright (C) 2020 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
# Variable definitions
#
PRECMP=
PIL_LIB=ymain
SRC_LIBS=$(MAIN_LIBS)
EXE_LIBS=$(MAIN_LIBS_LD)
ifneq (,$(findstring yambo_sc,$(MAKECMDGOALS)))
 PRECMP=-D_SC
 PIL_LIB=ymain_sc
 SRC_LIBS=$(PJ_SCLIBS)
 EXE_LIBS=$(PJ_SCLIBS_LD)
else ifneq (,$(findstring yambo_rt,$(MAKECMDGOALS)))
 PRECMP=-D_RT 
 PIL_LIB=ymain_rt
 SRC_LIBS=$(PJ_RTLIBS)
 EXE_LIBS=$(PJ_RTLIBS_LD)
else ifneq (,$(findstring yambo_ph,$(MAKECMDGOALS)))
 PRECMP=
 PIL_LIB=ymain_ph
 SRC_LIBS=$(PJ_PHLIBS)
 EXE_LIBS=$(PJ_PHLIBS_LD)
else ifneq (,$(findstring yambo_nl,$(MAKECMDGOALS)))
 PRECMP=-D_NL -D_RT -D_DOUBLE
 PIL_LIB=ymain_nl
 SRC_LIBS=$(PJ_NLLIBS)
 EXE_LIBS=$(PJ_NLLIBS_LD)
endif
#
# Compilation
#
yambo yambo_ph yambo_sc yambo_rt yambo_nl:
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@$(call todo_precision,$(PRECMP))
	@$(MAKE) $(MAKEFLAGS) dependencies
	@$(MAKE) $(MAKEFLAGS) ext-libs
	@$(MAKE) $(MAKEFLAGS) int-libs
	@+LIBS="$(SRC_LIBS)";LAB="_Y_";BASE="src";ADF="$(PRECMP) -D_yambo";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="$(PIL_LIB)";XLIBS="$(EXE_LIBS)";ADF="$(PRECMP)";$(todo_driver)
	@+X2DO="$@";BASE="$(PIL_LIB)";XLIBS="$(EXE_LIBS)";ADF="$(PRECMP)";$(mk_exe)
