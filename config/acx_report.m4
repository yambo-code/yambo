#
#        Copyright (C) 2000-2014 the YAMBO team
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

DP_str=" "
if test "$enable_dp" = "yes" ; then DP_str="X"; fi

Red_str=" "
if test "$enable_debug" = "yes" ; then Red_str="X"; fi

MPI_str=" "
if test "$mpibuild" = "yes" ; then MPI_str="X"; fi

NETCDF_str=" "
if test "$netcdf" = "yes" ; then NETCDF_str="X"; fi
NETCDF_loc_str=" "
if test "$compile_netcdf" = "yes" ; then NETCDF_loc_str="X"; fi

HDF5_str=" "
if test "$hdf5" = "yes" ; then HDF5_str="X"; fi

LIBXC_str=" "
if test "$acx_libxc_ok" = "yes" ; then LIBXC_str="X"; fi

NETCDF_LF_str=" "
if test "$enable_netcdf_LFS" = "yes" &&  test "$netcdf" = "yes" ; then NETCDF_LF_str="X"; fi

TIME_profile_str=" "
if test "$enable_time_profile" = "yes" ; then TIME_profile_str="X"; fi

PW_str=" "
if test "$compile_p2y" = "yes" ; then PW_str="X"; fi

OPENMP_str=" "
if test "$enable_open_mp" = "yes" ; then OPENMP_str="X"; fi

ETSF_str=" "
if test "$compile_e2y" = "yes" ; then ETSF_str="X"; fi
ETSF_loc_str=" "
if test "$compile_etsf" = "yes" ; then ETSF_loc_str="X"; fi

IOTK_loc_str=" "
if test "$compile_iotk" = "yes" ; then IOTK_loc_str="X"; fi

LAPACK_str=" "
if test "$compile_lapack" = "yes" ; then LAPACK_str="X"; fi

BLAS_str=" "
if test "$compile_blas" = "yes" ; then BLAS_str="X"; fi

LOCAL_str=" "
if test "$compile_local" = "yes" ; then LOCAL_str="X"; fi

SLK_str=" "
if test "$enable_scalapack" = "yes" ; then SLK_str="X"; fi

BGQ_str=" "
if test "$enable_bluegene" = "yes" ; then BGQ_str="X"; fi

OPENMPI_str=" "
if test "$enable_openmpi" = "yes" ; then OPENMPI_str="X"; fi

if test "$exec_prefix" = "NONE" ; then exec_prefix="$srcdir_path"; fi

AC_SUBST(srcdir_path)
AC_SUBST(exec_prefix)
AC_SUBST(DP_str)
AC_SUBST(Red_str)
AC_SUBST(MPI_str)
AC_SUBST(HDF5_str)
AC_SUBST(NETCDF_str)
AC_SUBST(NETCDF_loc_str)
AC_SUBST(LIBXC_str)
AC_SUBST(NETCDF_LF_str)
AC_SUBST(IOTK_loc_str)
AC_SUBST(ETSF_str)
AC_SUBST(ETSF_loc_str)
AC_SUBST(PW_str)
AC_SUBST(BLAS_str)
AC_SUBST(LAPACK_str)
AC_SUBST(LOCAL_str)
AC_SUBST(SLK_str)
AC_SUBST(OPENMP_str)
AC_SUBST(OPENMPI_str)
AC_SUBST(BGQ_str)
AC_SUBST(TIME_profile_str)

])
