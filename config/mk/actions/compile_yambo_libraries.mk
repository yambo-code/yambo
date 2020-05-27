yambo-libs: 
	@for target in $(YAMBO_LIBS) ; do $(MAKE) $$target; done
#
Yio: ext-libs
	@+LIBS2DO="$(YLIBIO)"; DIR2GO="src"; VPATH="$(topdir)/src"; ADF="-D_io_lib"; $(mk_external_yambo_lib_IO)
#
Ydriver: 
	@+LIBS2DO="$(YDRIVER)"; DIR2GO="lib/yambo"; VPATH="$(topdir)/lib/yambo"; ADF="-D_io_lib"; $(mk_external_yambo_lib)
