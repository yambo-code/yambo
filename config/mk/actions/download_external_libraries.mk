download-all:  
	@LIB2DO="all"; $(download_external_libraries)

download:  
	@for target in $(EXT_LIBS) ; do $(MAKE) "$$target"-dl ; done
#
#
libxc-dl: 
	@LIB2DO="libxc"; $(download_external_libraries)
lapack-dl: 
	@LIB2DO="lapack"; $(download_external_libraries)
fftw-dl: 
	@LIB2DO="fftw"; $(download_external_libraries)
fftqe-dl: 
	@LIB2DO="fftqe"; $(download_external_libraries)
yaml-dl: 
	@LIB2DO="fftqe"; $(download_external_libraries)
futile-dl: 
	@LIB2DO="futile"; $(download_external_libraries)
iotk-dl: 
	@LIB2DO="iotk"; $(download_external_libraries)
hdf5-dl: 
	@LIB2DO="hdf5"; $(download_external_libraries)
netcdf-dl: 
	@LIB2DO="netcdf"; $(download_external_libraries)
etsf_io-dl: 
	@LIB2DO="etsf_io"; $(download_external_libraries)
blacs-dl: 
	@LIB2DO="blacs"; $(download_external_libraries)
scalapack-dl: 
	@LIB2DO="scalapack"; $(download_external_libraries)
petsc-dl: 
	@LIB2DO="petsc"; $(download_external_libraries)
slepc-dl: 
	@LIB2DO="slepc"; $(download_external_libraries)
