ypp: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="ypp_Ydriver_"; BASE="lib/yambo/driver/src";  ADF="-D_ypp"; $(todo); $(mk_lib)
	@+LIBS="$(YPP_MAIN_LIBS)";                  BASE="src" ; $(todo) ; $(mk_lib)
	@+LIBS="$(YPP_LIBS)";   LAB="_ypp_";        BASE="ypp" ; $(todo) ; $(mk_lib)
	@+X2DO="ypp";XLIBS="$(YPP_MAIN_LIBS_LD)"; X_ypp_LIBS="$(YPP_LIBS_LD)"; $(todo_driver); $(mk_exe)
#
# Ypp projects #
#
ypp_ph: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="ypp_ph_Ydriver_"; BASE="lib/yambo/driver/src"; ADF="-D_YPP_ELPH -D_ypp"; $(todo); $(mk_lib)
	@+LIBS="$(YPP_MAIN_LIBS)";                     BASE="src"; ADF="-D_ELPH";     $(todo) ;$(mk_lib)
	@+LIBS="$(YPPPH_LIBS)"; LAB="_ypp_";           BASE="ypp"; ADF="-D_YPP_ELPH"; $(todo) ;$(mk_lib)
	@+X2DO="ypp_ph";XLIBS="$(YPP_MAIN_LIBS_LD)";X_ypp_LIBS="$(YPPPH_LIBS_LD)";ADF="-D_YPP_ELPH"; $(todo_driver); $(mk_exe)

ypp_rt_gpl: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="ypp_rt_gpl_Ydriver_"; BASE="lib/yambo/driver/src"; ADF="-D_RT -D_YPP_RT -D_ypp"; $(todo); $(mk_lib)
	@+LIBS="$(YPPRT_MAIN_LIBS)"; BASE="src"; ADF="-D_RT -D_YPP_RT"; $(mk_internal_lib)
	@+LIBS="$(YPPRT_LIBS)"; BASE="ypp"; ADF="-D_YPP_RT"; $(mk_ypp_src)
	@+X2DO="ypp_rt_gpl"; BASE="driver"; XLIBS="$(YPPRT_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPPRT_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_RT"; $(mk_ypp)
ypp_rt: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="ypp_rt_Ydriver_"; BASE="lib/yambo/driver/src"; ADF="-D_RT -D_ELPH -D_YPP_RT -D_ypp"; $(todo); $(mk_lib)
	@+LIBS="$(YPPRT_MAIN_LIBS)"; BASE="src"; ADF="-D_RT -D_ELPH -D_YPP_RT"; $(mk_internal_lib)
	@+LIBS="$(YPPRT_LIBS)"; BASE="ypp"; ADF="-D_ELPH -D_YPP_RT -D_YPP_ELPH"; $(mk_ypp_src)
	@+X2DO="ypp_rt"; BASE="driver"; XLIBS="$(YPPRT_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPPRT_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_RT"; $(mk_ypp)
ypp_nl: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="ypp_nl_Ydriver_"; BASE="lib/yambo/driver/src"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_YPP_RT -D_YPP_NL -D_ypp"; $(todo); $(mk_lib)
	@+LIBS="$(YPPNL_MAIN_LIBS)"; BASE="src"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_YPP_RT -D_YPP_NL -D_RT -D_NL"; $(mk_internal_lib)
	@+LIBS="$(YPPRT_LIBS)"; BASE="ypp"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_YPP_RT -D_YPP_NL"; $(mk_ypp_src)
	@+X2DO="ypp_nl"; DIBASE="driver"; XLIBS="$(YPPRT_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPPRT_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_YPP_NL -D_YPP_RT"; $(mk_ypp)
ypp_sc: ext-libs int-libs
	@+LIBS="$(YLIBDRIVER)"; LAB="ypp_sc_Ydriver_"; BASE="lib/yambo/driver/src"; ADF="-D_SC -D_ypp"; $(todo); $(mk_lib)
	@+LIBS="$(YPPSC_MAIN_LIBS)"; BASE="src"; ADF="-D_SC"; $(mk_internal_lib)
	@+LIBS="$(YPP_LIBS)"; BASE="ypp"; ADF="-D_YPP_SC"; $(mk_ypp_src)
	@+X2DO="ypp_sc"; BASE="driver"; XLIBS="$(YPPSC_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPP_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_SC"; $(mk_ypp)
