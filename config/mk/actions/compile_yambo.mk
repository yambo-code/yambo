yambo: ext-libs int-libs 
	@+LIBS="$(YLIBDRIVER)"; LAB="yambo_Ydriver_"; BASE="lib/yambo/driver/src";ADF="-D_yambo"; $(todo); $(mk_lib)
	@+LIBS="$(MAIN_LIBS)" ;                       BASE="src";                                 $(todo); $(mk_lib)
	@+X2DO="yambo";XLIBS="$(MAIN_LIBS_LD)";$(todo_driver);$(mk_exe)
#
# Yambo PROJECTS #
# 
yambo_sc: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="yambo_sc_Ydriver_"; BASE="lib/yambo/driver/src";ADF="-D_SC -D_yambo";$(todo); $(mk_lib)
	@+LIBS="$(PJ_SCLIBS)" ;                          BASE="src"                 ;ADF="-D_SC"         ;$(todo); $(mk_lib)
	@+X2DO="yambo_sc";XLIBS="$(PJ_SCLIBS_LD)";ADF="-D_SC";$(todo_driver);$(mk_exe)
yambo_rt_gpl: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="yambo_rt_gpl_Ydriver_"; BASE="lib/yambo/driver/src" ;ADF="-D_RT -D_yambo";$(todo); $(mk_lib)
	@+LIBS="$(PJ_RT_GPL_LIBS)" ;                         BASE="src"                  ;ADF="-D_RT"         ;$(todo); $(mk_lib)
	@+X2DO="yambo_rt_gpl";XLIBS="$(PJ_RT_GPL_LIBS_LD)";ADF="-D_RT";$(todo_driver);$(mk_exe)
yambo_rt: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="yambo_rt_Ydriver_"; BASE="lib/yambo/driver/src";ADF="-D_RT -D_RT_SCATT -D_ELPH -D_PHEL -D_yambo";$(todo); $(mk_lib)
	@+LIBS="$(PJ_RTLIBS)";                           BASE="src"                 ;ADF="-D_RT -D_RT_SCATT -D_ELPH -D_PHEL"         ;$(todo); $(mk_lib)
	@+X2DO="yambo_rt";XLIBS="$(PJ_RTLIBS_LD)";ADF="-D_RT -D_RT_SCATT -D_ELPH -D_PHEL";$(todo_driver);$(mk_exe)
yambo_ph: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="yambo_ph_Ydriver_"; BASE="lib/yambo/driver/src"; ADF="-D_ELPH -D_yambo -D_PHEL"; $(todo); $(mk_lib)
	@+LIBS="$(PJ_PHLIBS)"; BASE="src"; VPATH="$(compdir)/src"; ADF="-D_ELPH -D_PHEL"; $(todo); $(mk_lib)
	@+X2DO="yambo_ph"; XLIBS="$(PJ_PHLIBS_LD)" ;ADF="-D_ELPH -D_PHEL"; $(todo_driver); $(mk_exe)
yambo_rt_iterative: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="yambo_rt_iterative_Ydriver_"; BASE="lib/yambo/driver/src";ADF="-D_RT -D_RT_SCATT -D_ELPH -D_PHEL -D_yambo -D_ELPH_ITERATIVE" ;$(todo); $(mk_lib)
	@+LIBS="$(PJ_RTLIBS)";                           BASE="src"                 ;ADF="-D_RT -D_RT_SCATT -D_ELPH -D_PHEL -D_ELPH_ITERATIVE"         ;$(todo); $(mk_lib)
	@+X2DO="yambo_rt_iterative";XLIBS="$(PJ_RTLIBS_LD)";ADF="-D_RT -D_RT_SCATT -D_ELPH -D_PHEL -D_ELPH_ITERATIVE";$(todo_driver);$(mk_exe)
yambo_nl: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="yambo_nl_Ydriver_"; BASE="lib/yambo/driver/src"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT -D_yambo";$(todo); $(mk_lib)
	@+LIBS="$(PJ_NLLIBS)";                           BASE="src";                  ADF="-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT"; $(todo);$(mk_lib)
	@+X2DO="yambo_nl";XLIBS="$(PJ_NLLIBS_LD)";ADF="-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT";$(todo_driver); $(mk_exe)
yambo_qed: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="yambo_qed_Ydriver_"; BASE="lib/yambo/driver/src"; ADF="-D_QED -D_RT -D_RT_SCATT -D_ELPH -D_yambo"; $(todo); $(mk_lib)
	@+LIBS="$(PJ_RTLIBS)";                            BASE="src";                  ADF="-D_QED -D_RT -D_RT_SCATT -D_ELPH"; $(todo); $(mk_lib)
	@+X2DO="yambo_qed";XLIBS="$(PJ_RTLIBS_LD)";ADF="-D_QED -D_RT -D_ELPH"; $(todo_driver); $(mk_exe)
