#
# autoconf macro for detecting NetCDF module file
# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
# Copyright (C) 2000-2014 A. Marini and the YAMBO team
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
AC_ARG_WITH(netcdf_link,AC_HELP_STRING([--with-netcdf-link=<path>],
                                  [Specific libs needed by NetCDF or NetCDF/HDF5]))
NETCDF_LINKS=""
case $with_netcdf_link in
        yes | "") ;;
        -* | */* | *.a | *.so | *.so.* | *.o) NETCDF_LINKS="$with_netcdf_link" ;;
        *) NETCDF_LINKS="-l$with_netcdf_link" ;;
esac


#
# LARGE DATABASES SUPPORT
#
AC_ARG_ENABLE(netcdf-LFS, AC_HELP_STRING([--enable-netcdf-LFS],
             [Enable NetCDF Large File Support. Default is no.]))
#
# HDF5 support
#
AC_ARG_ENABLE(netcdf_hdf5,AC_HELP_STRING([--enable-netcdf-hdf5],
                                  [Enable the HDF5 support. Default is no.]))
#
#
netcdf="no"
dnetcdf=""
NCLIBS=""
IFLAG=""
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
     dnetcdf="-D_NETCDF_IO"
     IFLAG="$flag"
     if test x"$enable_netcdf_LFS" = "xyes"; then dnetcdf="-D_NETCDF_IO -D_64BIT_OFFSET"; fi
     break
    fi
 done
 if test "x$netcdf" = xno; then
  AC_MSG_RESULT([no])
 fi
fi

hdf5="no"
if test "x$netcdf" = xyes; then  
  if test x"$enable_netcdf_hdf5" = "xyes"; then
    AC_MSG_CHECKING([for HDF5 support in NetCDF])
    AC_LANG([Fortran])       
    save_libs="$LIBS"
    save_fcflags="$FCFLAGS"
    HDF5_FLAGS="-L$with_netcdf_lib  -lnetcdf -lhdf5_fortran -lhdf5_hl -lhdf5"
    FCFLAGS="$IFLAG$with_netcdf_include"
    for ldflag in "$HDF5_FLAGS -lsz" "$HDF5_FLAGS -lz" "$HDF5_FLAGS $NETCDF_LINKS"; do
      LIBS="$ldflag"
      AC_LINK_IFELSE(AC_LANG_PROGRAM([], [
       use hdf5
       use netcdf
       implicit none
       integer cmode
       cmode = NF90_HDF5
       cmode = nf90_abort(1)
       call h5open_f(cmode)]),
       [hdf5=yes], [hdf5=no])
     if test "x$hdf5" = xyes; then
       dnetcdf="${dnetcdf} -D_HDF5_IO"
       NETCDF_LINKS="$ldflag"
       for file in `find $with_netcdf_include \( -name '*hdf5*'\) `; do
         cp $file include/ 
       done
       for file in `find $with_netcdf_lib -name '*hdf5*.a'`; do
         cp $file lib/ 
       done
       AC_MSG_RESULT([yes])
       break
     fi
    done
    FCFLAGS="$save_fcflags"    
    LIBS="$save_libs"
    if test "x$hdf5" = xno; then
      AC_MSG_RESULT([no])
    fi
  fi
fi

if test "x$netcdf" = xyes; then  
  if test "`$with_netcdf_include/../bin/nc-config --flibs`"; then
    NCLIBS="`$with_netcdf_include/../bin/nc-config --flibs`"
    NCLIBS="${NCLIBS} `$with_netcdf_include/../bin/nc-config --libs`"
  elif test "`$with_netcdf_include/../bin/nf-config --flibs`"; then
    NCLIBS="`$with_netcdf_include/../bin/nf-config --flibs`"
    NCLIBS="${NCLIBS} `$with_netcdf_include/../bin/nf-config --libs`"
  else
    NCLIBS="-lnetcdf ${NCLIBS}"
  fi
  if test "x$hdf5" = xyes; then
    NCLIBS="${NCLIBS} ${NETCDF_LINKS}"
  elif test  x"$NETCDF_LINKS" != x"" ; then
    NCLIBS="${NETCDF_LINKS}"
  fi
fi

AC_SUBST(NCLIBS)
AC_SUBST(netcdf)
AC_SUBST(dnetcdf)

])


