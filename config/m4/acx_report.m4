#
# License-Identifier: GPL
#
# Copyright (C) 2006 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([ACX_REPORT],
[
# 
# - GENERAL CONFIGURATIONS -
# 
DP_check="-"
if test "$enable_dp" = "yes" ; then DP_check="X"; fi
#
KEEP_OBJS_check="-"
if test "$enable_keep_objects" = "yes" ; then KEEP_OBJS_check="X"; fi
#
TIME_profile_check="-"
if test "$enable_time_profile" = "yes" ; then TIME_profile_check="X"; fi
#
MEM_profile_check="-"
if test "$enable_memory_profile" = "yes" ; then MEM_profile_check="X"; fi
# 
# - PARALLEL/GPU SUPPORT -
# 
CUDA_check="-"
if ! test x"$enable_cuda" = "x"; then CUDA_check="X"; fi
#
MPI_check="-";
MPI_info=""
if test "$mpibuild" = "yes" ; then
  if test "$MPI_LIBS" = "" ; then
    MPI_info="(system default detected)";
    MPI_check="X";
  else
    MPI_check="E";
  fi
fi
#
OPENMP_check="-"
if test "$enable_open_mp" = "yes" ; then OPENMP_check="X"; fi
#
GPU_check="-"
if test "$enable_cuda_fortran" = "yes" ; then GPU_check="X"; fi
if test "$enable_openacc"      = "yes" ; then GPU_check="X"; fi
if test "$enable_openmp5"      = "yes" ; then GPU_check="X"; fi

#
# - LIBRARIES -
#
YAML_check="-"
if test "$internal_yaml" = "yes" ; then
  if test "$compile_yaml" = "yes" ; then YAML_check="C"; fi
  if test "$compile_yaml" = "no"  ; then YAML_check="I"; fi
elif test "$enable_yaml" = "yes" ; then
  YAML_check="E"
fi
#
FUTILE_check="-"
if test "$internal_futile" = "yes" ; then
  if test "$compile_futile" = "yes" ; then FUTILE_check="C"; fi
  if test "$compile_futile" = "no"  ; then FUTILE_check="I"; fi
elif test "$enable_futile" = "yes" ; then
  FUTILE_check="E"
fi
#
IOTK_check="-"
if test "$compile_p2y" = "yes" ; then
  IOTK_check="E"
  if test "$internal_iotk" = "yes" ; then
    if test "$compile_iotk" = "yes" ; then IOTK_check="C"; fi
    if test "$compile_iotk" = "no"  ; then IOTK_check="I"; fi
  fi
fi
#
ETSF_check="-"
if test "$compile_e2y" = "yes" ; then
  ETSF_check="E"
  if test "$internal_etsf" = "yes" ; then
    if test "$compile_etsf" = "yes" ; then ETSF_check="C"; fi
    if test "$compile_etsf" = "no"  ; then ETSF_check="I"; fi
  fi
fi
#
FFT_check="E"
if test "$internal_fft" = "yes" ; then
  if test "$compile_fftw" = "yes" || test "$compile_fftqe" = "yes"; then FFT_check="C"; fi
  if test "$compile_fftw" = "no"  && test "$compile_fftqe" = "no" ; then FFT_check="I"; fi
else
  if test "$compile_fftqe" = "yes" ; then FFT_check="E+I"; fi
fi
#
BLAS_check="E"
if test "$internal_blas" = "yes" ; then
  if test "$compile_blas" = "yes"; then BLAS_check="C"; fi
  if test "$compile_blas" = "no" ; then BLAS_check="I"; fi
fi
#
LAPACK_check="E"
if test "$internal_lapack" = "yes" ; then
  if test "$compile_lapack" = "yes"; then LAPACK_check="C"; fi
  if test "$compile_lapack" = "no" ; then LAPACK_check="I"; fi
fi
#
SLK_check="-"
if test "$enable_scalapack" = "yes" ; then SLK_check="E"; fi
if test "$internal_slk" = "yes" ; then
  if test "$compile_slk" = "yes"; then SLK_check="C"; fi
  if test "$compile_slk" = "no" ; then SLK_check="I"; fi
fi
#
BLACS_check="-"
if test "$enable_scalapack" = "yes" ; then BLACS_check="E"; fi
if test "$internal_blacs" = "yes" ; then
  if test "$compile_blacs" = "yes"; then BLACS_check="C"; fi
  if test "$compile_blacs" = "no" ; then BLACS_check="I"; fi
fi
#
PETSC_check="-"
PETSC_info=""
if test "$internal_petsc" = "yes" ; then
  if test "$compile_petsc" = "yes" ; then PETSC_check="C"; fi
  if test "$compile_petsc" = "no"  ; then PETSC_check="I"; fi
  if ! test "$with_petsc_branch" = "none"; then PETSC_info="(git branch $with_petsc_branch)"; fi
elif test "$enable_petsc" = "yes" ; then
  PETSC_check="E"
fi
#
SLEPC_check="-"
SLEPC_info=""
if test "$internal_slepc" = "yes" ; then
  if test "$compile_slepc" = "yes" ; then SLEPC_check="C"; fi
  if test "$compile_slepc" = "no"  ; then SLEPC_check="I"; fi
  if ! test "$with_slepc_branch" = "none"; then SLEPC_info="(git branch $with_slepc_branch)"; fi
elif test "$enable_slepc" = "yes" ; then
  SLEPC_check="E"
fi
#
LIBXC_check="E"
if test "$internal_libxc" = "yes" ; then
  if test "$compile_libxc" = "yes"; then LIBXC_check="C"; fi
  if test "$compile_libxc" = "no" ; then LIBXC_check="I"; fi
fi
#
DEVXLIB_check="E"
if test "$internal_devxlib" = "yes" ; then
  if test "$compile_devxlib" = "yes"; then DEVXLIB_check="C"; fi
  if test "$compile_devxlib" = "no" ; then DEVXLIB_check="I"; fi
fi
#
LIBCUDA_check="-"
if test "$use_libcuda" = "yes" ; then LIBCUDA_check="E"; fi

GPU_libinfo=""
if test "$GPU_SUPPORT" = "cudaf" && test "$LIBCUDA_check" = "-" ; then GPU_libinfo="with internal cuda library"; fi 
#
YDB_check="-";
if test "$enable_ydb" = "yes" ; then YDB_check="X"; fi
YPY_check="-";
if test "$enable_yambopy" = "yes" ; then YPY_check="X"; fi
#
# - I/O -
#
NETCDF_check="-"
if test "$internal_netcdf" = "yes" ; then
  if test "$compile_netcdf" = "yes" ; then NETCDF_check="C"; fi
  if test "$compile_netcdf" = "no"  ; then NETCDF_check="I"; fi
else
  NETCDF_check="E"
fi
NETCDF_info="Large Files Support enabled"
if test "$enable_netcdf_classic" = "yes"; then 
  NETCDF_info="NO Large Files Support"
else
  NETCDF_info="${NETCDF_info}, Version 4"
fi
#
#
PARIO_check="-"
HDF5_check="-"
HDF5_info="none"
if test "$hdf5" = "yes" ; then
  if test "$internal_hdf5" = "yes" ; then
    if test "$compile_hdf5" = "yes" ; then HDF5_check="C"; fi
    if test "$compile_hdf5" = "no"  ; then HDF5_check="I"; fi
  else
    HDF5_check="E"
    HDF5_info="external"
  fi
  if ! test "$enable_netcdf_classic" = "yes"  ; then
    if test "$IO_LIB_VER" = "parallel" ; then
      HDF5_info="Parallel_lib" ;
    else
      HDF5_info="Serial_lib" ;
    fi
    if test "$enable_hdf5_par_io" = "yes"; then      PARIO_check="X"; fi
    if test "$enable_hdf5_compression" = "yes"; then HDF5_info="${HDF5_info}, Data Compression enabled"; fi
  fi
fi
#
AC_SUBST(DP_check)
AC_SUBST(KEEP_OBJS_check)
AC_SUBST(TIME_profile_check)
AC_SUBST(MEM_profile_check)
#
AC_SUBST(CUDA_check)
AC_SUBST(OPENMP_check)
AC_SUBST(GPU_check)
AC_SUBST(GPU_libinfo)
AC_SUBST(PARIO_check)
AC_SUBST(HDF5_check)
AC_SUBST(HDF5_info)
AC_SUBST(NETCDF_check)
AC_SUBST(NETCDF_info)
#
AC_SUBST(YAML_check)
AC_SUBST(FUTILE_check)
AC_SUBST(IOTK_check)
AC_SUBST(ETSF_check)
#
AC_SUBST(FFT_check)
AC_SUBST(BLAS_check)
AC_SUBST(LAPACK_check)
AC_SUBST(BLACS_check)
AC_SUBST(SLK_check)
AC_SUBST(PETSC_check)
AC_SUBST(SLEPC_check)
AC_SUBST(PETSC_info)
AC_SUBST(SLEPC_info)
#
AC_SUBST(YDB_check)
AC_SUBST(YPY_check)
#
AC_SUBST(LIBXC_check)
AC_SUBST(DEVXLIB_check)
AC_SUBST(LIBCUDA_check)
AC_SUBST(MPI_check)
AC_SUBST(MPI_info)
#
# STRIPE [LIB] from paths
#
ACX_STRIPE_SUBPATH($IOTK_LIBS,"LIB")
IOTK_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($IOTK_INCS,"INC")
IOTK_INCS_R=$STRIPE
AC_SUBST(IOTK_LIBS_R)
AC_SUBST(IOTK_INCS_R)
#
ACX_STRIPE_SUBPATH($YAML_LIBS,"LIB")
YAML_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($YAML_INCS,"INC")
YAML_INCS_R=$STRIPE
AC_SUBST(YAML_LIBS_R)
AC_SUBST(YAML_INCS_R)
#
ACX_STRIPE_SUBPATH($FUTILE_LIBS,"LIB")
FUTILE_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($FUTILE_INCS,"INC")
FUTILE_INCS_R=$STRIPE
AC_SUBST(FUTILE_LIBS_R)
AC_SUBST(FUTILE_INCS_R)
#
ACX_STRIPE_SUBPATH($ETSF_LIBS,"LIB")
ETSF_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($ETSF_INCS,"INC")
ETSF_INCS_R=$STRIPE
AC_SUBST(ETSF_LIBS_R)
AC_SUBST(ETSF_INCS_R)
#
ACX_STRIPE_SUBPATH($NETCDFF_LIBS,"LIB")
NETCDFF_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($NETCDFF_INCS,"INC")
NETCDFF_INCS_R=$STRIPE
AC_SUBST(NETCDFF_LIBS_R)
AC_SUBST(NETCDFF_INCS_R)
#
ACX_STRIPE_SUBPATH($NETCDF_LIBS,"LIB")
NETCDF_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($NETCDF_INCS,"INC")
NETCDF_INCS_R=$STRIPE
AC_SUBST(NETCDF_LIBS_R)
AC_SUBST(NETCDF_INCS_R)
#
ACX_STRIPE_SUBPATH($HDF5_LIBS,"LIB")
HDF5_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($HDF5_INCS,"INC")
HDF5_INCS_R=$STRIPE
AC_SUBST(HDF5_LIBS_R)
AC_SUBST(HDF5_INCS_R)
#
ACX_STRIPE_SUBPATH($FFT_LIBS,"LIB")
FFT_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($FFT_INCS,"INC")
FFT_INCS_R=$STRIPE
AC_SUBST(FFT_LIBS_R)
AC_SUBST(FFT_INCS_R)
#
ACX_STRIPE_SUBPATH($BLAS_LIBS,"LIB")
BLAS_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($BLAS_INCS,"INC")
BLAS_INCS_R=$STRIPE
AC_SUBST(BLAS_LIBS_R)
AC_SUBST(BLAS_INCS_R)
#
ACX_STRIPE_SUBPATH($LAPACK_LIBS,"LIB")
LAPACK_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($LAPACK_INCS,"INC")
LAPACK_INCS_R=$STRIPE
AC_SUBST(LAPACK_LIBS_R)
AC_SUBST(LAPACK_INCS_R)
#
ACX_STRIPE_SUBPATH($SCALAPACK_LIBS,"LIB")
SCALAPACK_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($SCALAPACK_INCS,"INC")
SCALAPACK_INCS_R=$STRIPE
AC_SUBST(SCALAPACK_LIBS_R)
AC_SUBST(SCALAPACK_INCS_R)
#
ACX_STRIPE_SUBPATH($BLACS_LIBS,"LIB")
BLACS_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($BLACS_INCS,"INC")
BLACS_INCS_R=$STRIPE
AC_SUBST(BLACS_LIBS_R)
AC_SUBST(BLACS_INCS_R)
#
ACX_STRIPE_SUBPATH($PETSC_LIBS,"LIB")
PETSC_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($PETSC_INCS,"INC")
PETSC_INCS_R=$STRIPE
AC_SUBST(PETSC_LIBS_R)
AC_SUBST(PETSC_INCS_R)
#
ACX_STRIPE_SUBPATH($SLEPC_LIBS,"LIB")
SLEPC_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($SLEPC_INCS,"INC")
SLEPC_INCS_R=$STRIPE
AC_SUBST(SLEPC_LIBS_R)
AC_SUBST(SLEPC_INCS_R)
#
ACX_STRIPE_SUBPATH($LIBXC_LIBS,"LIB")
LIBXC_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($LIBXC_INCS,"INC")
LIBXC_INCS_R=$STRIPE
AC_SUBST(LIBXC_LIBS_R)
AC_SUBST(LIBXC_INCS_R)
#
ACX_STRIPE_SUBPATH($DEVXLIB_LIBS,"LIB")
DEVXLIB_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($DEVXLIB_INCS,"INC")
DEVXLIB_INCS_R=$STRIPE
AC_SUBST(DEVXLIB_LIBS_R)
AC_SUBST(DEVXLIB_INCS_R)
#
ACX_STRIPE_SUBPATH($LIBCUDA_LIBS,"LIB")
LIBCUDA_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($LIBCUDA_INCS,"INC")
LIBCUDA_INCS_R=$STRIPE
AC_SUBST(LIBCUDA_LIBS_R)
AC_SUBST(LIBCUDA_INCS_R)
#
ACX_STRIPE_SUBPATH($BLAS_PETSC_LIBS,"LIB")
BLAS_PETSC_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($BLAS_PETSC_INCS,"INC")
BLAS_PETSC_INCS_R=$STRIPE
AC_SUBST(BLAS_PETSC_LIBS_R)
AC_SUBST(BLAS_PETSC_INCS_R)
#
ACX_STRIPE_SUBPATH($LAPACK_PETSC_LIBS,"LIB")
LAPACK_PETSC_LIBS_R=$STRIPE
ACX_STRIPE_SUBPATH($LAPACK_PETSC_INCS,"INC")
LAPACK_PETSC_INCS_R=$STRIPE
AC_SUBST(LAPACK_PETSC_LIBS_R)
AC_SUBST(LAPACK_PETSC_INCS_R)
#
])
