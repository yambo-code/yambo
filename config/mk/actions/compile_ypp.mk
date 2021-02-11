ypp: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_driver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(topdir)/lib/yambo/driver/src"; ADF="-D_ypp"; $(mk_external_yambo_lib)
	@+LIBS2DO="$(YPP_MAIN_LIBS)"; XPATH="src" ; VPATH="$(topdir)/src" ; $(mk_src)
	@+LIBS2DO="$(YPP_LIBS)"; XPATH="ypp" ; VPATH="$(topdir)/ypp" ; $(mk_ypp_src)
	@+X2DO="ypp" ;XPATH="driver"; VPATH="$(topdir)/driver" ; XLIBS="$(YPP_MAIN_LIBS_LD)"; X_ypp_LIBS="$(YPP_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; $(mk_ypp)
#
# Ypp projects #
#
ypp_ph: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_ph_driver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(topdir)/lib/yambo/driver/src"; ADF="-D_YPP_ELPH -D_ypp"; $(mk_external_yambo_lib)
	@+LIBS2DO="$(YPP_MAIN_LIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_ELPH"; $(mk_src)
	@+LIBS2DO="$(YPPPH_LIBS)"; XPATH="ypp"; VPATH="$(topdir)/ypp";  ADF="-D_YPP_ELPH"; $(mk_ypp_src)
	@+X2DO="ypp_ph"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(YPP_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPPPH_LIBS_LD) el-ph"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_ELPH"; $(mk_ypp)
ypp_rt: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_rt_driver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(topdir)/lib/yambo/driver/src"; ADF="-D_RT -D_ELPH -D_YPP_RT -D_ypp"; $(mk_external_yambo_lib)
	@+LIBS2DO="$(YPPRT_MAIN_LIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_RT -D_ELPH -D_YPP_RT"; $(mk_src)
	@+LIBS2DO="$(YPPRT_LIBS)"; XPATH="ypp"; VPATH="$(topdir)/ypp"; ADF="-D_ELPH -D_YPP_RT -D_YPP_ELPH"; $(mk_ypp_src)
	@+X2DO="ypp_rt"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(YPPRT_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPPRT_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_RT"; $(mk_ypp)
ypp_nl: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; ADF="-D_DOUBLE -D_SLEPC_OFF" ; $(mk_internal_lib)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_nl_driver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(topdir)/lib/yambo/driver/src"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_ELPH -D_YPP_RT -D_YPP_NL -D_YPP_ELPH -D_ypp"; $(mk_external_yambo_lib)
	@+LIBS2DO="$(YPPNL_MAIN_LIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_RT -D_NL -D_ELPH -D_YPP_NL"; $(mk_src)
	@+LIBS2DO="$(YPPRT_LIBS)"; XPATH="ypp"; VPATH="$(topdir)/ypp"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_ELPH -D_YPP_RT -D_YPP_NL -D_YPP_ELPH"; $(mk_ypp_src)
	@+X2DO="ypp_nl"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(YPPRT_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPPRT_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_YPP_NL -D_YPP_RT -D_YPP_ELPH"; $(mk_ypp)
ypp_sc: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_sc_driver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(topdir)/lib/yambo/driver/src"; ADF="-D_SC -D_ypp"; $(mk_external_yambo_lib)
	@+LIBS2DO="$(YPPSC_MAIN_LIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_SC"; $(mk_src)
	@+LIBS2DO="$(YPP_LIBS)"; XPATH="ypp"; VPATH="$(topdir)/ypp"; ADF="-D_YPP_SC"; $(mk_ypp_src)
	@+X2DO="ypp_sc"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(YPPSC_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPP_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_SC"; $(mk_ypp)
ypp_magnetic: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_magnetic_driver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(topdir)/lib/yambo/driver/src"; ADF="-D_YPP_MAGNETIC  -D_ypp"; $(mk_external_yambo_lib)
	@+LIBS2DO="$(YPPSC_MAIN_LIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_SC -D_MAGNETIC"; $(mk_src)
	@+LIBS2DO="$(YPP_LIBS)"; XPATH="ypp"; VPATH="$(topdir)/ypp"; ADF="-D_YPP_SC -D_YPP_MAGNETIC"; $(mk_ypp_src)
	@+X2DO="ypp_magnetic"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(YPPSC_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPP_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_SC -D_YPP_MAGNETIC"; $(mk_ypp)

