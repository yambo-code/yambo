#
# License-Identifier: GPL
#
# Copyright (C) 2020 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
# Compilation
#
ypp: yambo
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@+LIBS="$(YPP_LIBS)";LAB="_YPP_";BASE="ypp";ADF="-D_ypp";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="ypp_def";XLIBS="$(YPP_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPP_LIBS_LD)";ADF="";$(todo_driver)
	@+X2DO="$@";BASE="ypp_def";XLIBS="$(YPP_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPP_LIBS_LD)";ADF="";$(mk_exe)

ypp_ph: ypp yambo_ph
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@+LIBS="$(YPPPH_LIBS)";LAB="_YPP_";BASE="ypp";ADF="-D_ypp";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="ypp_ph";XLIBS="$(YPPPH_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPPPH_LIBS_LD)";ADF="";$(todo_driver)
	@+X2DO="$@";BASE="ypp_ph";XLIBS="$(YPPPH_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPPPH_LIBS_LD)";ADF="";$(mk_exe)

ypp_sc: ypp yambo_sc
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@+LIBS="$(YPPSC_LIBS)";LAB="_YPP_";BASE="ypp";ADF="-D_ypp";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="ypp_sc";XLIBS="$(YPPSC_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPPSC_LIBS_LD)";ADF="";$(todo_driver)
	@+X2DO="$@";BASE="ypp_sc";XLIBS="$(YPPSC_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPPSC_LIBS_LD)";ADF="";$(mk_exe)

ypp_rt: ypp yambo_rt
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@+LIBS="$(YPP_LIBS)";LAB="_YPP_";BASE="ypp";ADF="-D_ypp";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="ypp_rt";XLIBS="$(YPPRT_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPPRT_LIBS_LD)";ADF="";$(todo_driver)
	@+X2DO="$@";BASE="ypp_rt";XLIBS="$(YPPRT_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPPRT_LIBS_LD)";ADF="";$(mk_exe)

ypp_nl: ypp_rt yambo_nl
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@+LIBS="$(YPPNL_LIBS)";LAB="_YPP_";BASE="ypp";ADF="-D_ypp";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="ypp_nl";XLIBS="$(YPPNL_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPPNL_LIBS_LD)";ADF="";$(todo_driver)
	@+X2DO="$@";BASE="ypp_nl";XLIBS="$(YPPNL_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPPNL_LIBS_LD)";ADF="";$(mk_exe)
