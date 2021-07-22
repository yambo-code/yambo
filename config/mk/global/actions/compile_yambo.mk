#
# Variable definitions
#
PRECMP=
SRC_LIBS=$(MAIN_LIBS)
EXE_LIBS=$(MAIN_LIBS_LD)
ifneq (,$(findstring yambo_sc,$(MAKECMDGOALS)))
 PRECMP=-D_SC
 SRC_LIBS=$(PJ_SCLIBS)
 EXE_LIBS=$(PJ_SCLIBS_LD)
else ifneq (,$(findstring yambo_rt_iterative,$(MAKECMDGOALS)))
 PRECMP=-D_RT -D_RT_SCATT -D_ELPH -D_PHEL -D_ELPH_ITERATIVE
 SRC_LIBS=$(PJ_RTITLIBS)
 EXE_LIBS=$(PJ_RTITLIBS_LD)
else ifneq (,$(findstring yambo_rt_gpl,$(MAKECMDGOALS)))
 PRECMP=-D_RT 
 SRC_LIBS=$(PJ_RT_GPL_LIBS)
 EXE_LIBS=$(PJ_RT_GPL_LIBS_LD)
else ifneq (,$(findstring yambo_rt,$(MAKECMDGOALS)))
 PRECMP=-D_RT -D_RT_SCATT -D_ELPH -D_PHEL
 SRC_LIBS=$(PJ_RTLIBS)
 EXE_LIBS=$(PJ_RTLIBS_LD)
else ifneq (,$(findstring yambo_ph,$(MAKECMDGOALS)))
 PRECMP=-D_ELPH -D_PHEL
 SRC_LIBS=$(PJ_PHLIBS)
 EXE_LIBS=$(PJ_PHLIBS_LD)
else ifneq (,$(findstring yambo_nl,$(MAKECMDGOALS)))
 PRECMP=-D_NL -D_RT
 SRC_LIBS=$(PJ_NLLIBS)
 EXE_LIBS=$(PJ_NLLIBS_LD)
else ifneq (,$(findstring yambo_qed,$(MAKECMDGOALS)))
 PRECMP=-D_QED -D_RT -D_RT_SCATT -D_ELPH
 SRC_LIBS=$(PJ_RTLIBS)
 EXE_LIBS=$(PJ_RTLIBS_LD)
endif
#
# Compilation
#
yambo yambo_ph yambo_sc yambo_rt yambo_rt_gpl yambo_rt_iterative yambo_nl yambo_qed: 
	@touch config/stamps_and_lists/compiling_$@.stamp
	@$(MAKE) conf-check
	@$(MAKE) $(MAKEFLAGS) dependencies
	@$(MAKE) $(MAKEFLAGS) ext-libs
	@$(MAKE) $(MAKEFLAGS) int-libs
	@+LIBS="$(YLIBDRIVER)";LAB="$@_Ydriver_";BASE="lib/yambo/driver/src";ADF="$(PRECMP) $(DOUBLE_PRECMP) -D_yambo";$(todo_lib);$(mk_lib)
	@+LIBS="$(SRC_LIBS)";BASE="src";ADF="$(PRECMP) $(DOUBLE_PRECMP)";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="driver";XLIBS="$(EXE_LIBS)";ADF="$(PRECMP) $(DOUBLE_PRECMP)";$(todo_driver)
	@sleep 0.1s;
	@+X2DO="$@";BASE="driver";XLIBS="$(EXE_LIBS)";ADF="$(PRECMP) $(DOUBLE_PRECMP)";$(mk_exe)
