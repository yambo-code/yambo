#
# License-Identifier: GPL
#
# Copyright (C) 2020 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
# Variable definitions
#
PIL_LIB=ymain
SRC_LIBS=$(MAIN_LIBS)
EXE_LIBS=$(MAIN_LIBS_LD)
ifneq (,$(findstring yambo_sc,$(MAKECMDGOALS)))
 PIL_LIB=ymain_sc
 SRC_LIBS=$(PJ_SCLIBS)
 EXE_LIBS=$(PJ_SCLIBS_LD)
else ifneq (,$(findstring yambo_rt,$(MAKECMDGOALS)))
 PIL_LIB=ymain_rt
 SRC_LIBS=$(PJ_RTLIBS)
 EXE_LIBS=$(PJ_RTLIBS_LD)
else ifneq (,$(findstring yambo_ph,$(MAKECMDGOALS)))
 PIL_LIB=ymain_ph
 SRC_LIBS=$(PJ_PHLIBS)
 EXE_LIBS=$(PJ_PHLIBS_LD)
else ifneq (,$(findstring yambo_nl,$(MAKECMDGOALS)))
 PIL_LIB=ymain_nl
 SRC_LIBS=$(PJ_NLLIBS)
 EXE_LIBS=$(PJ_NLLIBS_LD)
endif
#
# Compilation
#
yambo yambo_ph:
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@$(MAKE) $(MAKEFLAGS) dependencies
	@$(MAKE) $(MAKEFLAGS) ext-libs
	@$(MAKE) $(MAKEFLAGS) int-libs
	@+LIBS="$(SRC_LIBS)";LAB="_Y_";BASE="src";ADF="-D_yambo";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="$(PIL_LIB)";XLIBS="$(EXE_LIBS)";ADF="";$(todo_driver)
	@+X2DO="$@";BASE="$(PIL_LIB)";XLIBS="$(EXE_LIBS)";ADF="";$(mk_exe)

yambo_sc yambo_rt yambo_nl:
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@$(MAKE) $(MAKEFLAGS) dependencies
	@$(MAKE) $(MAKEFLAGS) ext-libs
	@$(MAKE) $(MAKEFLAGS) int-libs
	@$(MAKE) $(MAKEFLAGS) ham-libs
	@+LIBS="$(SRC_LIBS)";LAB="_Y_";BASE="src";ADF="-D_yambo";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="$(PIL_LIB)";XLIBS="$(EXE_LIBS)";ADF="";$(todo_driver)
	@+X2DO="$@";BASE="$(PIL_LIB)";XLIBS="$(EXE_LIBS)";ADF="";$(mk_exe)

ham-libs:
	@+LIBS="$(HAM_LIBS)";LAB="_Y_";BASE="src";ADF="-D_yambo";$(todo_lib);$(mk_lib)
