#
# Variable definitions
#
Y_PRECMP=
ifneq (,$(findstring ypp_sc,$(MAKECMDGOALS)))
 Y_PRECMP=-D_SC
 YPP_PRECMP=-D_YPP_SC
else ifneq (,$(findstring ypp_rt_gpl,$(MAKECMDGOALS)))
 Y_PRECMP=-D_RT -D_YPP_RT
 YPP_PRECMP=-D_YPP_RT
else ifneq (,$(findstring ypp_rt,$(MAKECMDGOALS)))
 Y_PRECMP=-D_RT -D_YPP_RT -D_ELPH 
 YPP_PRECMP=-D_YPP_RT -D_ELPH -D_YPP_ELPH
else ifneq (,$(findstring ypp_ph,$(MAKECMDGOALS)))
 Y_PRECMP=-D_ELPH -D_PHEL
 YPP_PRECMP=-D_YPP_ELPH
else ifneq (,$(findstring ypp_nl,$(MAKECMDGOALS)))
 Y_PRECMP=-D_YPP_RT -D_YPP_NL -D_RT -D_NL -D_DOUBLE
 YPP_PRECMP=-D_YPP_RT -D_YPP_NL -D_DOUBLE
endif
#
# Compilation
#
ypp ypp_ph ypp_sc ypp_rt_gpl ypp_rt ypp_nl: 
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@$(call todo_precision,$(Y_PRECMP))
	@$(MAKE) $(MAKEFLAGS) dependencies
	@$(MAKE) $(MAKEFLAGS) ext-libs
	@$(MAKE) $(MAKEFLAGS) int-libs
	@+LIBS="$(YLIBDRIVER)";LAB="$@_Ydriver_";BASE="lib/yambo/Ydriver/src";ADF="$(YPP_PRECMP) -D_ypp";$(todo_lib);$(mk_lib)
	@+LIBS="$(Y_LIBS)";BASE="src";ADF="$(Y_PRECMP)";$(todo_lib);$(mk_lib)
	@+LIBS="$(YPP_LIBS)";LAB="_ypp_";BASE="ypp";ADF="$(YPP_PRECMP)";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="driver";XLIBS="$(Y_LIBS_LD)";X_ypp_LIBS="$(YPP_LIBS_LD)";ADF="$(YPP_PRECMP)";$(todo_driver)
	@+X2DO="$@";BASE="driver";XLIBS="$(Y_LIBS_LD)";X_ypp_LIBS="$(YPP_LIBS_LD)";ADF="$(YPP_PRECMP)";$(mk_exe)
