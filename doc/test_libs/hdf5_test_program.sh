
cat << EOF > hdf5_prog.f90
  program test_hdf5
    use hdf5
    use netcdf
    implicit none
    integer cmode
    cmode = NF90_HDF5
    cmode = nf90_abort(1)
    call h5open_f(cmode)
  end program
EOF

# COMPILER #
FC="gfortran"

local_libs="${HOME}/libs/compiled/gnu_etsfmi_4.8.5"

##########
# NETCDF #
##########
# put here what you would use in --with-netcdf-libs=""
NETCDF_LIBS="-L$local_libs/netcdf/lib -lnetcdf -lnetcdff"
# put here what you would use in --with-netcdf-includedir=""
NETCDF_INCS="$local_libs/netcdf/include"

##########
#  HDF5  #
##########
# put here what you would use in --with-hdf5-libs=""
HDF5_LIBS="-L/usr/lib/x86_64-linux-gnu/hdf5/openmpi/ -lhdf5hl_fortran -lhdf5_fortran -lhdf5_hl -lhdf5 -lcurl -lz"
# put here what you would use in --with-hdf5-includedir=""
HDF5_INCS="/usr/lib/x86_64-linux-gnu/hdf5/openmpi/include"

$FC -o hdf5_prog.x hdf5_prog.f90 $HDF5_LIBS $NETCDF_LIBS -I$HDF5_INCS -I$NETCDF_INCS
