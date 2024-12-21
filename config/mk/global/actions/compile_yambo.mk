#
# License-Identifier: GPL
#
# Copyright (C) 2020 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
# Compilation
#
yambo:
	@rm -f ${compdir}/log/compile_yambo.log
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_yambo.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_yambo.stamp
	@$(MAKE) $(MAKEFLAGS) dependencies
	@$(MAKE) $(MAKEFLAGS) ext-libs
	@$(MAKE) $(MAKEFLAGS) int-libs
	@+LIBS="$(MAIN_LIBS)";LAB="_Y_";BASE="src";ADF="-D_yambo";$(todo_lib);$(mk_lib)
	@+X2DO="yambo";BASE=yambo_def;XLIBS="$(MAIN_LIBS_LD)";ADF="";$(todo_driver)
	@+X2DO="yambo";BASE=yambo_def;XLIBS="$(MAIN_LIBS_LD)";ADF="";$(mk_exe)

yambo_ph: yambo
	@rm -f ${compdir}/log/compile_yambo_ph.log
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_yambo_ph.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_yambo_ph.stamp
	@+LIBS="$(PJ_PHLIBS)";LAB="_Y_";BASE="src";ADF="-D_yambo";$(todo_lib);$(mk_lib)
	@+X2DO="yambo_ph";BASE=yambo_ph;XLIBS="$(PJ_PHLIBS_LD)";ADF="";$(todo_driver)
	@+X2DO="yambo_ph";BASE=yambo_ph;XLIBS="$(PJ_PHLIBS_LD)";ADF="";$(mk_exe)

yambo_sc: yambo ham-libs
	@rm -f ${compdir}/log/compile_yambo_sc.log
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_yambo_sc.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_yambo_sc.stamp
	@+LIBS="$(PJ_SCLIBS)";LAB="_Y_";BASE="src";ADF="-D_yambo";$(todo_lib);$(mk_lib)
	@+X2DO="yambo_sc";BASE=yambo_sc;XLIBS="$(PJ_SCLIBS_LD)";ADF="";$(todo_driver)
	@+X2DO="yambo_sc";BASE=yambo_sc;XLIBS="$(PJ_SCLIBS_LD)";ADF="";$(mk_exe)

yambo_rt: yambo ham-libs
	@rm -f ${compdir}/log/compile_yambo_rt.log
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_yambo_rt.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_yambo_rt.stamp
	@+LIBS="$(PJ_RTLIBS)";LAB="_Y_";BASE="src";ADF="-D_yambo";$(todo_lib);$(mk_lib)
	@+X2DO="yambo_rt";BASE=yambo_rt;XLIBS="$(PJ_RTLIBS_LD)";ADF="";$(todo_driver)
	@+X2DO="yambo_rt";BASE=yambo_rt;XLIBS="$(PJ_RTLIBS_LD)";ADF="";$(mk_exe)

yambo_nl: yambo ham-libs
	@rm -f ${compdir}/log/compile_yambo_nl.log
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_yambo_nl.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_yambo_nl.stamp
	@+LIBS="$(PJ_NLLIBS)";LAB="_Y_";BASE="src";ADF="-D_yambo";$(todo_lib);$(mk_lib)
	@+X2DO="yambo_nl";BASE=yambo_nl;XLIBS="$(PJ_NLLIBS_LD)";ADF="";$(todo_driver)
	@+X2DO="yambo_nl";BASE=yambo_nl;XLIBS="$(PJ_NLLIBS_LD)";ADF="";$(mk_exe)

ham-libs:
	@+LIBS="$(HAM_LIBS)";LAB="_Y_";BASE="src";ADF="-D_yambo";$(todo_lib);$(mk_lib)
