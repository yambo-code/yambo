#
# BLACS& SLK : Parallel compilation of blacs and scalapack fails
# SLEPC&PETSC: The internal build system of petsc and slepc already compiles the two libraries in parallel.
#
# Thus for these libraries the instruction to build in parallel (@+if) is not used
#
libxc: 
	@+if test "$(do_libxc)" = yes ; then LIBS="libxc" ; BASE="lib" ; $(MAKE) $(MAKEFLAGS) libxc-dl; $(mk_external_lib); fi
lapack: 
	@+if test "$(do_lapack)" = yes ; then LIBS="lapack" ; BASE="lib" ; $(MAKE) $(MAKEFLAGS) lapack-dl;  $(mk_external_lib); fi
fftw: 
	@+if test "$(do_fftw)" = yes ; then LIBS="fftw" ; BASE="lib" ; $(MAKE) $(MAKEFLAGS) fftw-dl; $(mk_external_lib); fi
fftqe: 
	@+if test "$(do_fftqe)" = yes ; then LIBS="fftqe" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) fftqe-dl; $(mk_external_lib); $(mk_lib); fi
yaml: 
	@+if test "$(do_yaml)" = yes ; then LIBS="yaml" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) yaml-dl; $(mk_external_lib); fi
futile: 
	@+if test "$(do_futile)" = yes ; then LIBS="futile" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) futile-dl ; $(mk_external_lib); fi
iotk: 
	@+if test "$(do_iotk)" = yes ; then LIBS="iotk" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) iotk-dl; $(mk_external_lib); fi
hdf5: 
	@+if test "$(do_hdf5)" = yes ; then LIBS="hdf5" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) hdf5-dl; $(mk_external_lib); fi
pnetcdf:
	@+if test "$(do_pnetcdf)" = yes ; then LIBS="pnetcdf" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) pnetcdf-dl ; $(mk_external_lib); fi
netcdf:
	@+if test "$(do_netcdf)" = yes ; then LIBS="netcdf"; BASE="lib"; $(MAKE) $(MAKEFLAGS) netcdf-dl ; $(mk_external_lib); fi
netcdff:
	@+if test "$(do_netcdf)" = yes ; then LIBS="netcdff" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) netcdff-dl ; $(mk_external_lib); fi
etsf_io: 
	@+if test "$(do_etsf)" = yes ; then LIBS="etsf_io" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) etsf_io-dl ; $(mk_external_lib); fi
blacs: 
	@if test "$(do_blacs)" = yes ; then LIBS="blacs" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) blacs-dl; $(mk_external_lib); fi
scalapack: 
	@if test "$(do_slk)" = yes ; then LIBS="scalapack" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) scalapack-dl ; $(mk_external_lib); fi
petsc: 
	@if test "$(do_petsc)" = yes ; then LIBS="petsc" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) petsc-dl; $(mk_external_lib); fi
slepc: 
	@if test "$(do_slepc)" = yes ; then LIBS="slepc" ; BASE="lib"; $(MAKE) $(MAKEFLAGS) slepc-dl; $(mk_external_lib); fi
