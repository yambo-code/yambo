#
#        Copyright (C) 2000-2022 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM
#
# This file is distributed under the terms of the GNU
# General Public License. You can redistribute it and/or
# modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation;
# either version 2, or (at your option) any later version.
#
# This program is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
# MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
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
# - PARALLEL/CUDA SUPPORT -
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
if test "$internal_petsc" = "yes" ; then
  if test "$compile_petsc" = "yes" ; then PETSC_check="C"; fi
  if test "$compile_petsc" = "no"  ; then PETSC_check="I"; fi
elif test "$enable_petsc" = "yes" ; then
  PETSC_check="E"
fi
#
SLEPC_check="-"
if test "$internal_slepc" = "yes" ; then
  if test "$compile_slepc" = "yes" ; then SLEPC_check="C"; fi
  if test "$compile_slepc" = "no"  ; then SLEPC_check="I"; fi
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
YDB_check="-";
if test "$enable_ydb" = "yes" ; then YDB_check="X"; fi
YPY_check="-";
if test "$enable_yambopy" = "yes" ; then YPY_check="X"; fi
#
# - I/O -
#
HDF5_PAR_IO_check="-"
PNETCDF_check="-"
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
PNETCDF_check="-"
if test "$enable_netcdf_par_io" = "yes";  then
  PNETCDF_check="X"
  NETCDF_info="${NETCDF_info}, Version 4"
fi
#
PARIO_check="-"
if ! test "$PARIO_info" = " " ; then
 PARIO_check="X"
fi
#
HDF5_check="-"
HDF5_PAR_IO_check="X"
HDF5_PAR_IO_info=" "
if test "$hdf5" = "yes" ; then
  if test "$internal_hdf5" = "yes" ; then
    if test "$compile_hdf5" = "yes" ; then HDF5_check="C"; fi
    if test "$compile_hdf5" = "no"  ; then HDF5_check="I"; fi
  else
    HDF5_check="E"
  fi
  if test "$IO_LIB_VER" = "parallel" ; then HDF5_info="Parallel_lib" ; fi
  if ! test "$enable_netcdf_classic" = "yes"  ; then
    if test "$enable_hdf5_compression" = "yes"; then
      HDF5_PAR_IO_info="Data Compression enabled" ;
    else
      HDF5_PAR_IO_info="NO Data Compression" ;
    fi
    if ! test "$enable_hdf5_par_io" = "yes"; then
      HDF5_PAR_IO_check="-"
      HDF5_PAR_IO_info=" "
    fi
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
AC_SUBST(PARIO_check)
AC_SUBST(HDF5_check)
AC_SUBST(HDF5_info)
AC_SUBST(HDF5_PAR_IO_check)
AC_SUBST(HDF5_PAR_IO_info)
AC_SUBST(PNETCDF_check)
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
#
AC_SUBST(YDB_check)
AC_SUBST(YPY_check)
#
AC_SUBST(LIBXC_check)
AC_SUBST(MPI_check)
AC_SUBST(MPI_info)
])
