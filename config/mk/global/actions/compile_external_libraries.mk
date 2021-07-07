#
# BLACS& SLK : Parallel compilation of blacs and scalapack fails
# SLEPC&PETSC: The internal build system of petsc and slepc already compiles the two libraries in parallel.
#
# Thus for these libraries the instruction to build in parallel (@+if) is not used
#
libxc: 
	@+if test "$(do_libxc)" = yes ; then LIBS="libxc" ; BASE="lib" ; $(mk_external_lib); fi
lapack: 
	@+if test "$(do_lapack)" = yes ; then LIBS="lapack" ; BASE="lib" ; $(mk_external_lib); fi
fftw: 
	@+if test "$(do_fftw)" = yes ; then LIBS="fftw" ; BASE="lib" ; $(mk_external_lib); fi
fftqe: 
	@+if test "$(do_fftqe)" = yes ; then LIBS="fftqe" ; BASE="lib" ; $(mk_external_lib); $(mk_lib); fi
yaml: 
	@+if test "$(do_yaml)" = yes ; then LIBS="yaml" ; BASE="lib" ; $(mk_external_lib); fi
futile: 
	@+if test "$(do_futile)" = yes ; then LIBS="futile" ; BASE="lib" ; $(mk_external_lib); fi
iotk: 
	@+if test "$(do_iotk)" = yes ; then LIBS="iotk" ; BASE="lib" ; $(mk_external_lib); fi
hdf5: 
	@+if test "$(do_hdf5)" = yes ; then LIBS="hdf5" ; BASE="lib" ; $(mk_external_lib); fi
pnetcdf:
	@+if test "$(do_pnetcdf)" = yes ; then LIBS="pnetcdf" ; BASE="lib" ; $(mk_external_lib); fi
netcdf:
	@+if test "$(do_netcdf)" = yes ; then LIBS="netcdf"; BASE="lib" ; $(mk_external_lib); fi
netcdff:
	@+if test "$(do_netcdf)" = yes ; then LIBS="netcdff" ; BASE="lib" ; $(mk_external_lib); fi
etsf_io: 
	@+if test "$(do_etsf)" = yes ; then LIBS="etsf_io" ; BASE="lib" ; $(mk_external_lib); fi
blacs: 
	@if test "$(do_blacs)" = yes ; then LIBS="blacs" ; BASE="lib" ; $(mk_external_lib); fi
scalapack: 
	@if test "$(do_slk)" = yes ; then LIBS="scalapack" ; BASE="lib" ; $(mk_external_lib); fi
petsc: 
	@if test "$(do_petsc)" = yes ; then LIBS="petsc" ; BASE="lib" ; $(mk_external_lib); fi
slepc: 
	@if test "$(do_slepc)" = yes ; then LIBS="slepc" ; BASE="lib" ; $(mk_external_lib); fi
