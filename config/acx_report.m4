#
# Copyright (C) 2000-2009 A. Marini and the YAMBO team
#              http://www.yambo-code.org
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

NETCDF_LF_str=" "
if test "$enable_largedb" = "yes" ; then NETCDF_LF_str="X"; fi

PW_str=" "
if test "$compile_p2y" = "yes" ; then PW_str="X"; fi

ETSF_str=" "
if test "$compile_e2y" = "yes" ; then ETSF_str="X"; fi

BLAS_str=" "
if test "$compile_blas" = "yes" ; then BLAS_str="X"; fi

LAPACK_str=" "
if test "$compile_lapack" = "yes" ; then LAPACK_str="X"; fi

LOCAL_str=" "
if test "$compile_local" = "yes" ; then LOCAL_str="X"; fi

SLK_str=" "
if test "$enable_scalapack" = "yes" ; then SLK_str="X"; fi

AC_SUBST(srcdir_path)
AC_SUBST(DP_str)
AC_SUBST(Red_str)
AC_SUBST(MPI_str)
AC_SUBST(NETCDF_str)
AC_SUBST(NETCDF_LF_str)
AC_SUBST(ETSF_str)
AC_SUBST(PW_str)
AC_SUBST(BLAS_str)
AC_SUBST(LAPACK_str)
AC_SUBST(LOCAL_str)
AC_SUBST(SLK_str)

])
