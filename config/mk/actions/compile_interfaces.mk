a2y: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,-D_a2y)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="a2y_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_a2y"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,)
	@+LIBS2DO="$(2YLIBS)"; DIR="src" ; VPATH="$(compdir)/src" ; $(mk_internal_lib)
	@$(call todo,$@,interfaces,)
	@+LIBS2DO="int_modules"; DIR2GO="interfaces" ; VPATH="$(compdir)/interfaces" ; $(mk_internal_lib)
	@+X2DO="a2y"; DIR="interfaces/a2y"; VPATH="$(compdir)/interfaces/a2y"; XLIBS="$(2YLIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; $(mk_yambo)
c2y: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,-D_c2y)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="c2y_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_c2y"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,)
	@+LIBS2DO="$(2YLIBS)"; DIR="src" ; VPATH="$(compdir)/src" ; $(mk_internal_lib)
	@$(call todo,$@,interfaces,)
	@+LIBS2DO="int_modules"; DIR2GO="interfaces" ; VPATH="$(compdir)/interfaces" ; $(mk_internal_lib)
	@+X2DO="c2y"; DIR="interfaces/c2y"; VPATH="$(compdir)/interfaces/c2y"; XLIBS="$(2YLIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; $(mk_yambo)
ifeq ($(do_p2y),yes)
p2y: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,-D_p2y)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="p2y_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_p2y"; $(mk_external_yambo_lib)
	@$(call todo,$@,src)
	@+LIBS2DO="$(2YLIBS)"; DIR="src" ; VPATH="$(compdir)/src" ; $(mk_internal_lib)
	@$(call todo,$@,interfaces,)
	@+LIBS2DO="int_modules"; DIR2GO="interfaces" ; VPATH="$(compdir)/interfaces" ; $(mk_internal_lib)
	@+X2DO="p2y" ; DIR="interfaces/p2y"; VPATH="$(compdir)/interfaces/p2y"; XLIBS="$(2YLIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; ADF="$(p2ycpp)"; $(mk_yambo) ;
endif
ifeq ($(do_e2y),yes)
e2y: ext-libs int-libs
	@$(call todo,$@,lib/yambo/driver,-D_e2y)
	@+LIBS2DO="$(YLIBDRIVER)"; NAME="e2y_Ydriver_"; DIR2GO="lib/yambo/driver/src"; VPATH="$(compdir)/lib/yambo/driver/src"; ADF="-D_e2y"; $(mk_external_yambo_lib)
	@$(call todo,$@,src,)
	@+LIBS2DO="$(2YLIBS)"; DIR="src" ; VPATH="$(compdir)/src" ; $(mk_internal_lib)
	@$(call todo,$@,interfaces,)
	@+LIBS2DO="int_modules"; DIR2GO="interfaces" ; VPATH="$(compdir)/interfaces" ; $(mk_internal_lib)
	@+X2DO="e2y" ; DIR="interfaces/e2y"; VPATH="$(compdir)/interfaces/e2y"; XLIBS="$(2YLIBS_LD)"; DRILIBS="$(YLIBDRIVER_LD)"; $(mk_yambo) ;
endif
