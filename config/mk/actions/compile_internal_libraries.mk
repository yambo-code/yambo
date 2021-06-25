int-libs: 
	@for target in $(INT_LIBS) ; do $(MAKE) $$target; done
#
qe_pseudo: 
	+LIBS2DO="qe_pseudo"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
slatec: 
	@+LIBS2DO="slatec"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
math77:
	@+LIBS2DO="math77"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
local:
	@+LIBS2DO="local" ; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
