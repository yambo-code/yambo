download:  
	@LIB2DO="all"; $(get_external_libraries)
libxc-dl: 
	@LIB2DO="libxc"; $(get_external_libraries)
lapack-dl: 
	@LIB2DO="lapack"; $(get_external_libraries)
fftw-dl: 
	@LIB2DO="fftw"; $(get_external_libraries)
fftqe-dl: 
	@LIB2DO="fftqe"; $(get_external_libraries)
yaml-dl: 
	@LIB2DO="fftqe"; $(get_external_libraries)
futile-dl: 
	@LIB2DO="futile"; $(get_external_libraries)
iotk-dl: 
	@LIB2DO="iotk"; $(get_external_libraries)
hdf5-dl: 
	@LIB2DO="hdf5"; $(get_external_libraries)
netcdf-dl: 
	@LIB2DO="netcdf"; $(get_external_libraries)
etsf_io-dl: 
	@LIB2DO="etsf_io"; $(get_external_libraries)
blacs-dl: 
	@LIB2DO="blacs"; $(get_external_libraries)
scalapack-dl: 
	@LIB2DO="scalapack"; $(get_external_libraries)
petsc-dl: 
	@LIB2DO="petsc"; $(get_external_libraries)
slepc-dl: 
	@LIB2DO="slepc"; $(get_external_libraries)
