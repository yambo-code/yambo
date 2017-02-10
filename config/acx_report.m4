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
srcdir_path=$PWD

DP_str="-"
if test "$enable_dp" = "yes" ; then DP_str="X"; fi

Red_str="-"
if test "$enable_debug" = "yes" ; then Red_str="X"; fi

MPI_str="-"
if test "$mpibuild" = "yes" ; then
  MPI_str="X"
fi

NETCDF_str="-"
if test "$compile_netcdf" = "yes" ; then
  NETCDF_str="I"
else
  NETCDF_str="E"
fi
NETCDF_LF_str="(With large files support)"
if test "$enable_netcdf_classic" = "yes"; then NETCDF_LF_str="(No large files support)"; fi

HDF5_str="-"
if test "$hdf5" = "yes" ; then
  HDF5_str="E"
  HDF5_support="(No HDF5-IO format)"
  if test "$enable_netcdf_hdf5" = "yes"; then HDF5_support="(HDF5-IO format, no data compression)" ; fi
  if test "$enable_netcdf_hdf5" = "yes" && test "$enable_hdf5_compression" = "yes"; then HDF5_support="(HDF5-IO format with data compression)" ; fi
fi

LIBXC_str="-"
if test "$acx_libxc_ok" = "yes" ; then
  LIBXC_str="E"
else
  LIBXC_str="I"
fi

TIME_profile_str="-"
if test "$enable_time_profile" = "yes" ; then TIME_profile_str="X"; fi

PW_str="-"
if test "$compile_p2y" = "yes" ; then
  PW_str="E"
  if test "$compile_iotk" = "yes" ; then PW_str="I"; fi
fi

OPENMP_str="-"
if test "$enable_open_mp" = "yes" ; then OPENMP_str="X"; fi
#
# OPTIONALS
#
ETSF_str="-"
if test "$compile_e2y" = "yes" ; then
  ETSF_str="E"
  if test "$compile_etsf" = "yes" ; then ETSF_str="I"; fi
fi

LAPACK_str="E"
if test "$compile_lapack" = "yes" ; then LAPACK_str="I"; fi

BLAS_str="E"
if test "$compile_blas" = "yes" ; then BLAS_str="I"; fi

PET_str="-"
if test "$enable_petsc"  = "yes" ; 
 then PET_str="E"; 
 if test "$compile_petsc" = "yes" ; then PET_str="I"; fi
fi

SLE_str="-"
if test "$enable_slepc"  = "yes" ; 
 then SLE_str="E"; 
 if test "$compile_slepc" = "yes" ; then SLE_str="I"; fi
fi

SLK_str="-"
if test "$enable_scalapack" = "yes" ; then SLK_str="E"; fi
if test "$compile_slk"      = "yes" ; then SLK_str="I"; fi

BLACS_str="-"
if ! test "$SLK_str" = "-" ; then
 if test "$enable_scalapack" = "yes" ; then BLACS_str="E"; fi
 if test "$compile_blacs"    = "yes" ; then BLACS_str="I"; fi
fi

SLEPC_str="-"
if test "$enable_slepc" = "yes" ; then SLEPC_str="E"; fi

BGQ_str="-"
if test "$enable_bluegene" = "yes" ; then BGQ_str="X"; fi

MPI_LIB_str="-"
if test "$enable_mpi_lib" = "yes" ; then MPI_LIB_str="E"; fi

if test "$exec_prefix" = "NONE" ; then exec_prefix="$srcdir_path"; fi

AC_SUBST(srcdir_path)
AC_SUBST(exec_prefix)
AC_SUBST(DP_str)
AC_SUBST(Red_str)
AC_SUBST(MPI_str)
AC_SUBST(MPI_LIB_str)
AC_SUBST(HDF5_str)
AC_SUBST(HDF5_support)
AC_SUBST(NETCDF_str)
AC_SUBST(LIBXC_str)
AC_SUBST(NETCDF_LF_str)
AC_SUBST(ETSF_str)
AC_SUBST(PW_str)
AC_SUBST(BLAS_str)
AC_SUBST(LAPACK_str)
AC_SUBST(SLK_str)
AC_SUBST(PET_str)
AC_SUBST(SLE_str)
AC_SUBST(BLACS_str)
AC_SUBST(OPENMP_str)
AC_SUBST(BGQ_str)
AC_SUBST(TIME_profile_str)

])
