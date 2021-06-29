ext-libs: 
	@for target in $(EXT_LIBS) ; do $(MAKE) $$target; \
	if test -f "$(topdir)/lib/$$target/Makefile" && test ! -f "$(topdir)/lib/$$target/package-installed"; then \
	echo "$$target build failed"; break; fi; done
#
# BLACS& SLK : Parallel compilation of blacs and scalapack fails
# SLEPC&PETSC: The internal build system of petsc and slepc already compiles the two libraries in parallel.
#
# Thus for these libraries the instruction to build in parallel (@+if) is not used
#
libxc: 
	@+if test "$(do_libxc)" = yes ; then LIBS2DO="libxc" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
lapack: 
	@+if test "$(do_lapack)" = yes ; then LIBS2DO="lapack" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
fftw: 
	@+if test "$(do_fftw)" = yes ; then LIBS2DO="fftw" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
fftqe: 
	@+if test "$(do_fftqe)" = yes ; then LIBS2DO="fftqe" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); $(mk_internal_lib); fi
yaml: 
	@+if test "$(do_yaml)" = yes ; then LIBS2DO="yaml" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
futile: 
	@+if test "$(do_futile)" = yes ; then LIBS2DO="futile" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
iotk: 
	@+if test "$(do_iotk)" = yes ; then LIBS2DO="iotk" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
hdf5: 
	@+if test "$(do_hdf5)" = yes ; then LIBS2DO="hdf5" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
pnetcdf:
	@+if test "$(do_pnetcdf)" = yes ; then LIBS2DO="pnetcdf" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
netcdf:
	@+if test "$(do_netcdf)" = yes ; then LIBS2DO="netcdf"; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
netcdff:
	@+if test "$(do_netcdf)" = yes ; then LIBS2DO="netcdff" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
etsf_io: 
	@+if test "$(do_etsf)" = yes ; then LIBS2DO="etsf_io" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
blacs: 
	@if test "$(do_blacs)" = yes ; then LIBS2DO="blacs" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
scalapack: 
	@if test "$(do_slk)" = yes ; then LIBS2DO="scalapack" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
petsc: 
	@if test "$(do_petsc)" = yes ; then LIBS2DO="petsc" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
slepc: 
	@if test "$(do_slepc)" = yes ; then LIBS2DO="slepc" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
