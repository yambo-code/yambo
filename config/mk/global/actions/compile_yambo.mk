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
ifneq (,$(findstring yambo_sc,$(MAKECMDGOALS)))
 PRECMP=-D_SC
else ifneq (,$(findstring yambo_rt_iterative,$(MAKECMDGOALS)))
 PRECMP=-D_RT -D_RT_SCATT -D_ELPH -D_PHEL -D_ELPH_ITERATIVE
else ifneq (,$(findstring yambo_rt_gpl,$(MAKECMDGOALS)))
 PRECMP=-D_RT 
else ifneq (,$(findstring yambo_rt,$(MAKECMDGOALS)))
 PRECMP=-D_RT -D_RT_SCATT -D_ELPH -D_PHEL
else ifneq (,$(findstring yambo_ph,$(MAKECMDGOALS)))
 PRECMP=-D_ELPH -D_PHEL
else ifneq (,$(findstring yambo_nl,$(MAKECMDGOALS)))
 PRECMP=-D_NL -D_RT -D_DOUBLE
else ifneq (,$(findstring yambo_qed,$(MAKECMDGOALS)))
 PRECMP=-D_QED -D_RT -D_RT_SCATT -D_ELPH
endif
#
# Compilation
#
yambo yambo_ph yambo_sc yambo_rt yambo_rt_gpl yambo_rt_iterative yambo_nl yambo_qed: 
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@$(call todo_precision,$(PRECMP))
	@$(MAKE) $(MAKEFLAGS) dependencies
	@$(MAKE) $(MAKEFLAGS) ext-libs
	@$(MAKE) $(MAKEFLAGS) int-libs
	@+LIBS="$(YLIBDRIVER)";LAB="$@_Ydriver_";BASE="lib/yambo/Ydriver/src";ADF="$(PRECMP) -D_yambo";$(todo_lib);$(mk_lib)
	@+LIBS="$(Y_LIBS)";BASE="src";ADF="$(PRECMP)";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="driver";XLIBS="$(Y_LIBS_LD)";ADF="$(PRECMP)";$(todo_driver)
	@+X2DO="$@";BASE="driver";XLIBS="$(Y_LIBS_LD)";ADF="$(PRECMP)";$(mk_exe)
