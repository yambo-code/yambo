#
# autoconf macro for detecting NetCDF module file
# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
# Copyright (C) 2000-2008 A. Marini and the YAMBO team
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
AC_DEFUN([KH_PATH_NETCDF_F90],[

AC_ARG_WITH(netcdf_include,AC_HELP_STRING([--with-netcdf-include=<path>],
                                  [Path of the NetCDF include directory]))
AC_ARG_WITH(netcdf_lib,AC_HELP_STRING([--with-netcdf-lib=<path>],
                                  [Path of the NetCDF lib directory]))
netcdf="no"
dnetcdf=""
NCLIBS=""
if test -d "$with_netcdf_include" && test -d "$with_netcdf_lib" ; then
 AC_MSG_CHECKING([for NetCDF in $with_netcdf_lib])
 AC_LANG([Fortran])
 save_fcflags="$FCFLAGS"
 for flag in "-I" "-M" "-p"; do
    FCFLAGS="$flag$with_netcdf_include $save_fcflags"
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [use netcdf]),
     [netcdf=yes 
      for file in `find $with_netcdf_include \( -name '*netcdf*' -o -name '*typesizes*' \) `; do
       cp $file include/ 
      done
      for file in `find $with_netcdf_lib -name '*netcdf*.a'`; do
       cp $file lib/ 
      done
     ], [netcdf=no])
    FCFLAGS="$save_fcflags"
    if test "x$netcdf" = xyes; then
     AC_MSG_RESULT([yes])
     NCLIBS="-lnetcdf"
     dnetcdf="-D_NETCDF_IO"
     break
    fi
 done
 if test "x$netcdf" = xno; then
  AC_MSG_RESULT([no])
 fi
fi
AC_SUBST(NCLIBS)
AC_SUBST(netcdf)
AC_SUBST(dnetcdf)
])
