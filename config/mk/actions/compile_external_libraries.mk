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
	@+LIB2DO="libxc"; $(download_external_lib)
	@+if test "$(do_libxc)" = yes ; then LIBS2DO="libxc" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
lapack: 
	@+LIB2DO="lapack"; $(download_external_lib)
	@+if test "$(do_lapack)" = yes ; then LIBS2DO="lapack" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
fftw: 
	@+LIB2DO="fftw"; $(download_external_lib)
	@+if test "$(do_fftw)" = yes ; then LIBS2DO="fftw" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
fftqe: 
	@+LIB2DO="fftqe"; $(download_external_lib)
	@+if test "$(do_fftqe)" = yes ; then LIBS2DO="fftqe" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); $(mk_internal_lib); fi
yaml: 
	@+LIB2DO="fftqe"; $(download_external_lib)
	@+if test "$(do_yaml)" = yes ; then LIBS2DO="yaml" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
futile: 
	@+LIB2DO="futile"; $(download_external_lib)
	@+if test "$(do_futile)" = yes ; then LIBS2DO="futile" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
iotk: 
	@+LIB2DO="iotk"; $(download_external_lib)
	@+if test "$(do_iotk)" = yes ; then LIBS2DO="iotk" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
hdf5: 
	@+LIB2DO="hdf5"; $(download_external_lib)
	@+if test "$(do_hdf5)" = yes ; then LIBS2DO="hdf5" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
netcdf: 
	@+LIB2DO="netcdf"; $(download_external_lib)
	@+if test "$(do_netcdf)" = yes ; then LIBS2DO="pnetcdf netcdf netcdff" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
etsf_io: 
	@+LIB2DO="etsf_io"; $(download_external_lib)
	@+if test "$(do_etsf)" = yes ; then LIBS2DO="etsf_io" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
blacs: 
	@+LIB2DO="blacs"; $(download_external_lib)
	@if test "$(do_blacs)" = yes ; then LIBS2DO="blacs" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
scalapack: 
	@+LIB2DO="scalapack"; $(download_external_lib)
	@if test "$(do_slk)" = yes ; then LIBS2DO="scalapack" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
petsc: 
	@+LIB2DO="petsc"; $(download_external_lib)
	@if test "$(do_petsc)" = yes ; then LIBS2DO="petsc" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi
slepc: 
	@+LIB2DO="slepc"; $(download_external_lib)
	@if test "$(do_slepc)" = yes ; then LIBS2DO="slepc" ; \
	DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_external_lib); fi

