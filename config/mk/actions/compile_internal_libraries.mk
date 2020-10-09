int-libs: 
	@for target in $(INT_LIBS) ; do $(MAKE) $$target; done
#
qe-pseudo: ext-libs
	@+LIBS2DO="qe_pseudo"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
slatec: ext-libs
	@+LIBS2DO="slatec"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
math77: ext-libs
	@+LIBS2DO="math77"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
local: ext-libs
	@+LIBS2DO="local" ; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
