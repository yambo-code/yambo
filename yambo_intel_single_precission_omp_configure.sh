#!/bin/bash

module load netcdf-c/4.8.1-intelmpi netcdf-fortran/4.5.4-intelmpi hdf5/intel/1.12.1 libxc/intel/6.2.2-no-fhc petsc/3.16.3-intelmpi slepc/3.16.1-intelmpi intel/icc/2022.0.1 intel/mkl/2022.0.1 

NETCDF_ROOT="/apps/software/netCDF-C/4.8.1"
NETCDFF_ROOT="/apps/software/netCDF-Fortran/4.5.4"
HDF5_ROOT="/apps/software/hdf5/1.12.1"
LIBXC_ROOT="/apps/software/libxc/intel/6.2.2-no-fhc"
PETSC_ROOT="/apps/software/petsc/3.16.3"
SLEPC_ROOT="/apps/software/slepc/3.16.1"
YAMBO_LIBS_ROOT="${HOME}/yambo-libs"

./configure \
FC=ifort \
CC=icc \
MPICC=mpiicc \
MPIFC=mpiifort \
FCFLAGS="-O0 -g -traceback -check bounds -fpe0" \
--enable-msgs-comps \
--enable-time-profile \
--enable-mpi \
--enable-open-mp \
--enable-iotk \
--without-editor \
--enable-memory-profile \
--enable-keep-src \
--enable-hdf5-par-io \
--with-mpi-path="${I_MPI_ROOT}" \
--with-netcdf-path=${NETCDF_ROOT} \
--with-netcdff-path=${NETCDFF_ROOT} \
--with-hdf5-path=${HDF5_ROOT} \
--with-fft-libs=" -L${MKLROOT}/lib/intel64 -lmkl_cdft_core -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -lmkl_blacs_intelmpi_lp64 -liomp5 -lpthread -lm -ldl" \
--with-blas-libs="-L${MKLROOT}/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -lmkl_blacs_intelmpi_lp64 -liomp5 -lpthread -lm -ldl" \
--with-lapack-libs="-L${MKLROOT}/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -lmkl_blacs_intelmpi_lp64 -liomp5 -lpthread -lm -ldl" \
--with-blacs-libs="mkl" \
--with-scalapack-libs="mkl" \
--with-extlibs-path="${YAMBO_LIBS_ROOT}"

#--enable-slepc-linalg
#--with-petsc-path="${PETSC_ROOT}" \
#--with-slepc-path="${SLEPC_ROOT}" \
#--with-libxc-path="${LIBXC_ROOT}" \

#FCFLAGS="-O2 -g -traceback -check bounds -fpe0" \
