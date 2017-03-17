#
#        Copyright (C) 2000-2016 the YAMBO team
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
srcdir_path=$PWD
#
if test "$exec_prefix" = "NONE" ; then exec_prefix="$srcdir_path"; fi

# 
# - GENERAL CONFIGURATIONS -
# 
DP_str="-"
if test "$enable_dp" = "yes" ; then DP_str="X"; fi
#
Red_str="-"
if test "$enable_debug" = "yes" ; then Red_str="X"; fi
#
TIME_profile_str="-"
if test "$enable_time_profile" = "yes" ; then TIME_profile_str="X"; fi
#

# 
# - PARALLEL SUPPORT -
# 
MPI_str="-"
if test "$mpibuild" = "yes" ; then MPI_str="X" ; fi
#
OPENMP_str="-"
if test "$enable_open_mp" = "yes" ; then OPENMP_str="X"; fi

#
# - LIBRARIES -
#

#
IOTK_str=" - "
if test "$compile_p2y" = "yes" ; then
  IOTK_str=" E "
  if test "$internal_iotk" = "yes" ; then
    if test "$compile_iotk" = "yes" ; then IOTK_str="I+C"; fi
    if test "$compile_iotk" = "no"  ; then IOTK_str="I+F"; fi
  fi
fi
#
ETSF_str=" - "
if test "$compile_e2y" = "yes" ; then
  ETSF_str=" E "
  if test "$compile_etsf" = "yes" ; then ETSF_str=" I "; fi
fi
#
NETCDF_str=" - "
if test "$internal_netcdf" = "yes" ; then
  if test "$compile_netcdf" = "yes" ; then NETCDF_str="I+C"; fi
  if test "$compile_netcdf" = "no"  ; then NETCDF_str="I+F"; fi
else
  NETCDF_str=" E "
fi
NETCDF_LF_str="(With large files support)"
if test "$enable_netcdf_classic" = "yes"; then NETCDF_LF_str="(No large files support)"; fi
#
HDF5_str=" - "
if test "$hdf5" = "yes" ; then
  if test "$internal_hdf5" = "yes" ; then
    if test "$compile_hdf5" = "yes" ; then HDF5_str="I+C"; fi
    if test "$compile_hdf5" = "no"  ; then HDF5_str="I+F"; fi
  else
    HDF5_str=" E "
  fi
  HDF5_support="(No HDF5-IO format)"
  if test "$enable_netcdf_hdf5" = "yes"; then HDF5_support="(HDF5-IO format, no data compression)" ; fi
  if test "$enable_netcdf_hdf5" = "yes" && test "$enable_hdf5_compression" = "yes"; then HDF5_support="(HDF5-IO format with data compression)" ; fi
fi
#

#
FFT_str=" E "
if test "$internal_fft" = "yes" ; then
  if test "$compile_fftw" = "yes" || test "$compile_fftqe" = "yes"; then FFT_str="I+C"; fi
  if test "$compile_fftw" = "no"  && test "$compile_fftqe" = "no" ; then FFT_str="I+F"; fi
else
  if test "$compile_fftqe" = "yes" ; then FFT_str="E+I"; fi
fi
#
BLAS_str=" E "
if test "$compile_blas" = "yes" ; then BLAS_str=" I "; fi
#
LAPACK_str=" E "
if test "$compile_lapack" = "yes" ; then LAPACK_str=" I "; fi
#
BLACS_str=" - "
if ! test "$SLK_str" = " - " ; then
 if test "$enable_scalapack" = "yes" ; then BLACS_str=" E "; fi
 if test "$compile_blacs"    = "yes" ; then BLACS_str=" I "; fi
fi
#
SLK_str=" - "
if test "$enable_scalapack" = "yes" ; then SLK_str=" E "; fi
if test "$compile_slk"      = "yes" ; then SLK_str=" I "; fi
#
PET_str=" - "
if test "$enable_petsc"  = "yes" ; 
 then PET_str=" E "; 
 if test "$compile_petsc" = "yes" ; then PET_str=" I "; fi
fi
#
SLE_str=" - "
if test "$enable_slepc"  = "yes" ; 
 then SLE_str=" E "; 
 if test "$compile_slepc" = "yes" ; then SLE_str=" I "; fi
fi

#
LIBXC_str=" - "
if test "$acx_libxc_ok" = "yes" ; then
  LIBXC_str=" E "
else
  LIBXC_str=" I "
fi
#
MPI_LIBS_info=""
if test "$mpibuild" = "yes" ; then
  if test "$MPI_LIBS" = "" ; then
    MPI_LIBS_info="(system default detected)";
    MPI_LIBS_str=" X ";
  else
    MPI_LIBS_str=" E ";
  fi
fi


AC_SUBST(srcdir_path)
AC_SUBST(exec_prefix)
#
AC_SUBST(DP_str)
AC_SUBST(Red_str)
AC_SUBST(TIME_profile_str)
#
AC_SUBST(MPI_str)
AC_SUBST(OPENMP_str)
#
AC_SUBST(IOTK_str)
AC_SUBST(ETSF_str)
AC_SUBST(NETCDF_str)
AC_SUBST(NETCDF_LF_str)
AC_SUBST(HDF5_str)
AC_SUBST(HDF5_support)
#
AC_SUBST(FFT_str)
AC_SUBST(BLAS_str)
AC_SUBST(LAPACK_str)
AC_SUBST(BLACS_str)
AC_SUBST(SLK_str)
AC_SUBST(PET_str)
AC_SUBST(SLE_str)
#
AC_SUBST(LIBXC_str)
AC_SUBST(MPI_LIBS_str)
AC_SUBST(MPI_LIBS_info)

])
