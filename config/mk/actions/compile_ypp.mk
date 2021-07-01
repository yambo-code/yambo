ypp: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,-D_ypp)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_ypp"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,)
	@+LIBS2DO="$(YPP_MAIN_LIBS)"; XPATH="src" ; VPATH="$(compdir)/src" ; $(mk_src)
	@$(call todo,$@,ypp,)
	@+LIBS2DO="$(YPP_LIBS)"; XPATH="ypp" ; VPATH="$(compdir)/ypp" ; $(mk_ypp_src)
	@+X2DO="ypp" ;XPATH="driver"; VPATH="$(compdir)/driver" ; XLIBS="$(YPP_MAIN_LIBS_LD)"; X_ypp_LIBS="$(YPP_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; $(mk_ypp)
#
# Ypp projects #
#
ypp_ph: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_YPP_ELPH -D_ypp")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_ph_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_YPP_ELPH -D_ypp"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,"-D_ELPH")
	@+LIBS2DO="$(YPP_MAIN_LIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_ELPH"; $(mk_src)
	@$(call todo,$@,ypp,"-D_YPP_ELPH")
	@+LIBS2DO="$(YPPPH_LIBS)"; XPATH="ypp"; VPATH="$(compdir)/ypp";  ADF="-D_YPP_ELPH"; $(mk_ypp_src)
	@+X2DO="ypp_ph"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(YPP_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPPPH_LIBS_LD) el-ph"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_ELPH"; $(mk_ypp)
ypp_rt_gpl: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_RT -D_YPP_RT -D_ypp")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_rt_gpl_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_RT -D_YPP_RT -D_ypp"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,"-D_RT -D_YPP_RT")
	@+LIBS2DO="$(YPPRT_MAIN_LIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_RT -D_YPP_RT"; $(mk_src)
	@$(call todo,$@,ypp,-D_YPP_RT)
	@+LIBS2DO="$(YPPRT_LIBS)"; XPATH="ypp"; VPATH="$(compdir)/ypp"; ADF="-D_YPP_RT"; $(mk_ypp_src)
	@+X2DO="ypp_rt_gpl"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(YPPRT_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPPRT_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_RT"; $(mk_ypp)
ypp_rt: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_RT -D_ELPH -D_YPP_RT -D_ypp")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_rt_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_RT -D_ELPH -D_YPP_RT -D_ypp"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,"-D_RT -D_ELPH -D_YPP_RT")
	@+LIBS2DO="$(YPPRT_MAIN_LIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_RT -D_ELPH -D_YPP_RT"; $(mk_src)
	@$(call todo,$@,ypp,"-D_ELPH -D_YPP_RT -D_YPP_ELPH")
	@+LIBS2DO="$(YPPRT_LIBS)"; XPATH="ypp"; VPATH="$(compdir)/ypp"; ADF="-D_ELPH -D_YPP_RT -D_YPP_ELPH"; $(mk_ypp_src)
	@+X2DO="ypp_rt"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(YPPRT_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPPRT_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_RT"; $(mk_ypp)
ypp_nl: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_DOUBLE -D_SLEPC_OFF -D_YPP_RT -D_YPP_NL -D_ypp")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_nl_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_YPP_RT -D_YPP_NL -D_ypp"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,"-D_DOUBLE -D_SLEPC_OFF -D_YPP_RT -D_YPP_NL -D_RT -D_NL")
	@+LIBS2DO="$(YPPNL_MAIN_LIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_YPP_RT -D_YPP_NL -D_RT -D_NL"; $(mk_src)
	@$(call todo,$@,ypp,"-D_DOUBLE -D_SLEPC_OFF -D_YPP_RT -D_YPP_NL -D_RT -D_NL")
	@+LIBS2DO="$(YPPRT_LIBS)"; XPATH="ypp"; VPATH="$(compdir)/ypp"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_YPP_RT -D_YPP_NL"; $(mk_ypp_src)
	@+X2DO="ypp_nl"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(YPPRT_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPPRT_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_YPP_NL -D_YPP_RT"; $(mk_ypp)
ypp_sc: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_SC -D_ypp")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="ypp_sc_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_SC -D_ypp"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,-D_SC)
	@+LIBS2DO="$(YPPSC_MAIN_LIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_SC"; $(mk_src)
	@$(call todo,$@,ypp,-D_SC)
	@+LIBS2DO="$(YPP_LIBS)"; XPATH="ypp"; VPATH="$(compdir)/ypp"; ADF="-D_YPP_SC"; $(mk_ypp_src)
	@+X2DO="ypp_sc"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(YPPSC_MAIN_LIBS_LD)"; \
	X_ypp_LIBS="$(YPP_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_YPP_SC"; $(mk_ypp)
