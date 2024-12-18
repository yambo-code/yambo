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
Y_EXE_LIBS=$(YPP_MAIN_LIBS_LD)
YPP_SRC_LIBS=$(YPP_LIBS)
YPP_EXE_LIBS=$(YPP_LIBS_LD)
ifneq (,$(findstring ypp_sc,$(MAKECMDGOALS)))
 PIL_LIB=ymain_sc
 Y_EXE_LIBS=$(YPPSC_MAIN_LIBS_LD)
 YPP_SRC_LIBS=$(YPPSC_LIBS)
 YPP_EXE_LIBS=$(YPPSC_LIBS_LD)
else ifneq (,$(findstring ypp_rt,$(MAKECMDGOALS)))
 PIL_LIB=ymain_rt
 Y_EXE_LIBS=$(YPPRT_MAIN_LIBS_LD)
 YPP_SRC_LIBS=$(YPPRT_LIBS)
 YPP_EXE_LIBS=$(YPPRT_LIBS_LD)
else ifneq (,$(findstring ypp_ph,$(MAKECMDGOALS)))
 PIL_LIB=ymain_ph
 Y_EXE_LIBS=$(YPPPH_MAIN_LIBS_LD)
 YPP_SRC_LIBS=$(YPPPH_LIBS)
 YPP_EXE_LIBS=$(YPPPH_LIBS_LD)
else ifneq (,$(findstring ypp_nl,$(MAKECMDGOALS)))
 PIL_LIB=ymain_nl
 Y_EXE_LIBS=$(YPPNL_MAIN_LIBS_LD)
 YPP_SRC_LIBS=$(YPPNL_LIBS)
 YPP_EXE_LIBS=$(YPPNL_LIBS_LD)
endif
#
# Compilation
#
ypp ypp_ph ypp_sc ypp_rt_gpl ypp_rt ypp_nl: yambo 
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@$(call todo_precision,$(Y_PRECMP))
	@$(MAKE) $(MAKEFLAGS) dependencies
	@$(MAKE) $(MAKEFLAGS) ext-libs
	@$(MAKE) $(MAKEFLAGS) int-libs
	@+LIBS="$(YPP_SRC_LIBS)";LAB="_YPP_";BASE="ypp";ADF="-D_ypp";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="driver";XLIBS="$(Y_EXE_LIBS)";X_ypp_LIBS="$(YPP_EXE_LIBS)";ADF="-D_ypp";$(todo_driver)
	@+X2DO="$@";BASE="driver";XLIBS="$(Y_EXE_LIBS)";X_ypp_LIBS="$(YPP_EXE_LIBS)";ADF="-D_ypp";$(mk_exe)
