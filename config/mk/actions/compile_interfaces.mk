a2y: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_a2y"; $(mk_driver_src)
	@+LIBS2DO="$(2YLIBS)"; XPATH="src" ; VPATH="$(topdir)/src" ; $(mk_src)
	@+LIBS2DO="int_modules"; DIR2GO="interfaces" ; VPATH="$(topdir)/interfaces" ; $(mk_internal_lib)
	@+X2DO="a2y"; XPATH="interfaces/a2y"; VPATH="$(topdir)/interfaces/a2y"; XLIBS="$(2YLIBS_LD)"; $(mk_yambo)
c2y: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_c2y"; $(mk_driver_src)
	@+LIBS2DO="$(2YLIBS)"; XPATH="src" ; VPATH="$(topdir)/src" ; $(mk_src)
	@+LIBS2DO="int_modules"; DIR2GO="interfaces" ; VPATH="$(topdir)/interfaces" ; $(mk_internal_lib)
	@+X2DO="c2y"; XPATH="interfaces/c2y"; VPATH="$(topdir)/interfaces/c2y"; XLIBS="$(2YLIBS_LD)"; $(mk_yambo)
ifeq ($(do_p2y),yes)
p2y: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_p2y"; $(mk_driver_src)
	@+LIBS2DO="$(2YLIBS)"; XPATH="src" ; VPATH="$(topdir)/src" ; $(mk_src)
	@+LIBS2DO="int_modules"; DIR2GO="interfaces" ; VPATH="$(topdir)/interfaces" ; $(mk_internal_lib)
	@+X2DO="p2y" ; XPATH="interfaces/p2y"; VPATH="$(topdir)/interfaces/p2y"; XLIBS="$(2YLIBS_LD)"; ADF="@PW_CPP@"; $(mk_yambo) ;
endif
ifeq ($(do_e2y),yes)
e2y: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_e2y"; $(mk_driver_src)
	@+LIBS2DO="$(2YLIBS)"; XPATH="src" ; VPATH="$(topdir)/src" ; $(mk_src)
	@+LIBS2DO="int_modules"; DIR2GO="interfaces" ; VPATH="$(topdir)/interfaces" ; $(mk_internal_lib)
	@+X2DO="e2y" ; XPATH="interfaces/e2y"; VPATH="$(topdir)/interfaces/e2y"; XLIBS="$(2YLIBS_LD)"; $(mk_yambo) ;
endif

