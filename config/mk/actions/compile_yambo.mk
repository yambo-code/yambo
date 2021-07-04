yambo: ext-libs int-libs 
	@$(call todo,$@,lib/yambo/driver,-D_yambo)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="yambo_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/$$DIR2GO"; ADF="-D_yambo"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,)
	@+LIBS2DO="$(MAIN_LIBS)"; XPATH="src"; VPATH="$(compdir)/src"; $(mk_src)
	@$(call todo,$@,driver,)
	@+X2DO="yambo"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(MAIN_LIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; $(mk_yambo)
#
# Yambo PROJECTS #
# 
yambo_sc: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_SC -D_yambo")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="yambo_sc_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_SC -D_yambo"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,-D_SC)
	@+LIBS2DO="$(PJ_SCLIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_SC"; $(mk_src)
	@+X2DO="yambo_sc"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(PJ_SCLIBS_LD)" ; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_SC"; $(mk_yambo)
yambo_rt_gpl: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_RT -D_yambo")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="yambo_rt_gpl_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_RT -D_yambo"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,-D_RT)
	@+LIBS2DO="$(PJ_RT_GPL_LIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_RT"; $(mk_src)
	@+X2DO="yambo_rt_gpl"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(PJ_RT_GPL_LIBS_LD)" ; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_RT"; $(mk_yambo)
yambo_rt: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_RT -D_RT_SCATT -D_ELPH -D_PHEL -D_yambo")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="yambo_rt_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_RT -D_RT_SCATT -D_ELPH -D_PHEL -D_yambo"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,"-D_RT -D_RT_SCATT -D_ELPH -D_PHEL")
	@+LIBS2DO="$(PJ_RTLIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_RT -D_RT_SCATT -D_ELPH -D_PHEL"; $(mk_src)
	@+X2DO="yambo_rt"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(PJ_RTLIBS_LD)" ; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_RT -D_ELPH -D_PHEL"; $(mk_yambo)
yambo_rt_iterative: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_RT -D_RT_SCATT -D_ELPH -D_ELPH_ITERATIVE -D_PHEL -D_yambo")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="yambo_rt_iterative_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_RT -D_RT_SCATT -D_ELPH -D_ELPH_ITERATIVE -D_PHEL -D_yambo"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,"-D_RT -D_RT_SCATT -D_ELPH -D_ELPH_ITERATIVE -D_PHEL")
	@+LIBS2DO="$(PJ_RTITLIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_RT -D_RT_SCATT -D_ELPH -D_ELPH_ITERATIVE -D_PHEL"; $(mk_src)
	@+X2DO="yambo_rt_iterative"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(PJ_RTITLIBS_LD)" ; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_RT -D_ELPH -D_ELPH_ITERATIVE -D_PHEL"; $(mk_yambo)
yambo_nl: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT -D_yambo")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="yambo_nl_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT -D_yambo"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,"-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT")
	@+LIBS2DO="$(PJ_NLLIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT"; $(mk_src)
	@+X2DO="yambo_nl"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(PJ_NLLIBS_LD)" ; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT"; $(mk_yambo)
yambo_ph: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_ELPH -D_yambo -D_PHEL")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="yambo_ph_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_ELPH -D_yambo -D_PHEL"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,"-D_ELPH -D_PHEL")
	@+LIBS2DO="$(PJ_PHLIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_ELPH -D_PHEL"; $(mk_src)
	@$(call todo,$@,driver,"-D_ELPH -D_PHEL")
	@+X2DO="yambo_ph"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(PJ_PHLIBS_LD)" ; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_ELPH -D_PHEL"; $(mk_yambo)
yambo_qed: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,"-D_QED -D_RT -D_RT_SCATT -D_ELPH -D_yambo")
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="yambo_qed_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_QED -D_RT -D_RT_SCATT -D_ELPH -D_yambo"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,"-D_QED -D_RT -D_RT_SCATT -D_ELPH")
	@+LIBS2DO="$(PJ_RTLIBS)"; XPATH="src"; VPATH="$(compdir)/src"; ADF="-D_QED -D_RT  -D_RT_SCATT -D_ELPH"; $(mk_src)
	@+X2DO="yambo_qed"; XPATH="driver"; VPATH="$(compdir)/driver"; XLIBS="$(PJ_RTLIBS_LD)" ; DRILIBS="$(YLIBDRIVER_LD)"; ADF="-D_QED -D_RT -D_ELPH"; $(mk_yambo)

