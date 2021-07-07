#
# Variable definitions
#
Y_PRECMP=
Y_SRC_LIBS=$(YPP_MAIN_LIBS)
Y_EXE_LIBS=$(YPP_MAIN_LIBS_LD)
YPP_SRC_LIBS=$(YPP_LIBS)
YPP_EXE_LIBS=$(YPP_LIBS_LD)
ifneq (,$(findstring ypp_sc,$(MAKECMDGOALS)))
 Y_PRECMP=-D_SC
 YPP_PRECMP=-D_YPP_SC
 Y_SRC_LIBS=$(YPPSC_MAIN_LIBS)
 Y_EXE_LIBS=$(YPPSC_MAIN_LIBS_LD)
else ifneq (,$(findstring ypp_rt_gpl,$(MAKECMDGOALS)))
 Y_PRECMP=-D_RT -D_YPP_RT
 YPP_PRECMP=-D_YPP_RT
 Y_SRC_LIBS=$(YPPRT_MAIN_LIBS)
 Y_EXE_LIBS=$(YPPRT_MAIN_LIBS_LD)
 YPP_SRC_LIBS=$(YPPRT_LIBS)
 YPP_EXE_LIBS=$(YPPRT_LIBS_LD)
else ifneq (,$(findstring ypp_rt,$(MAKECMDGOALS)))
 Y_PRECMP=-D_RT -D_YPP_RT -D_ELPH
 YPP_PRECMP=-D_YPP_RT -D_ELPH -D_YPP_ELPH
 Y_SRC_LIBS=$(YPPRT_MAIN_LIBS)
 Y_EXE_LIBS=$(YPPRT_MAIN_LIBS_LD)
 YPP_SRC_LIBS=$(YPPRT_LIBS)
 YPP_EXE_LIBS=$(YPPRT_LIBS_LD)
else ifneq (,$(findstring ypp_ph,$(MAKECMDGOALS)))
 Y_PRECMP=-D_ELPH
 YPP_PRECMP=-D_YPP_ELPH
 YPP_SRC_LIBS=$(YPPPH_LIBS)
 YPP_EXE_LIBS=$(YPPPH_LIBS_LD)
else ifneq (,$(findstring ypp_nl,$(MAKECMDGOALS)))
 Y_PRECMP=-D_RT -D_YPP_RT -D_ELPH
 YPP_PRECMP=-D_YPP_RT -D_ELPH -D_YPP_ELPH
 Y_SRC_LIBS=$(YPPRT_MAIN_LIBS)
 Y_EXE_LIBS=$(YPPRT_MAIN_LIBS_LD)
 YPP_SRC_LIBS=$(YPPRT_LIBS)
 YPP_EXE_LIBS=$(YPPRT_LIBS_LD)
endif
#
# Compilation
#
ypp ypp_ph ypp_sc ypp_rt_gpl ypp_rt ypp_nl: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)";LAB="$@_Ydriver_";BASE="lib/yambo/driver/src";ADF="$(YPP_PRECMP) -D_ypp";$(todo_lib);$(mk_lib)
	@+LIBS="$(Y_SRC_LIBS)";BASE="src";ADF="$(Y_PRECMP)";$(todo_lib);$(mk_lib)
	@+LIBS="$(YPP_SRC_LIBS)";LAB="_ypp_";BASE="ypp";ADF="$(YPP_PRECMP)";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="driver";XLIBS="$(Y_EXE_LIBS)";X_ypp_LIBS="$(YPP_EXE_LIBS)";ADF="$(Y_PRECMP)";$(todo_driver)
	@sleep 0.1s;
	@+X2DO="$@";BASE="driver";XLIBS="$(Y_EXE_LIBS)";X_ypp_LIBS="$(YPP_EXE_LIBS)";ADF="$(Y_PRECMP)";$(mk_exe)

