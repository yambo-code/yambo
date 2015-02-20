## Copyright (C) 2009-2013 M. Oliveira and F. Nogueira
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.
##
## $Id: libxc.m4 800 2013-10-09 11:18:39Z micael $

AC_DEFUN([ACX_LIBXC], [
acx_libxc_ok=no
compile_libxc=yes

dnl Check if the library was given in the command line
dnl if not, use environment variables or defaults
AC_ARG_WITH(libxc_path, [AS_HELP_STRING([--with-libxc-path=<path>], [Path to libxc install directory])])
AC_ARG_WITH(libxc_includedir, [AS_HELP_STRING([--with-libxc-includedir=<path>], [Path to the libxc include directory])])

if test -d "$with_libxc_path"; then
# Set FCFLAGS_LIBXC 
  case $with_libxc_path in
    "") ;;		  
    *)  libxc_include_path="$with_libxc_path/include" ;;
  esac
  case $with_libxc_includedir in
    "") ;;
    *)  libxc_include_path="$with_libxc_includedir" ;;
  esac
FCFLAGS_LIBXC="$ax_cv_f90_modflag$libxc_include_path"
dnl Backup LIBS and FCFLAGS
acx_libxc_save_LIBS="$LIBS"
acx_libxc_save_FCFLAGS="$FCFLAGS"

dnl The tests
AC_MSG_CHECKING([for libxc])

dnl The following program should work with all version of libxc
testprog="AC_LANG_PROGRAM([],[
    use xc_f90_lib_m
    implicit none

    integer :: i
    i = XC_EXCHANGE
])"

FCFLAGS="$FCFLAGS_LIBXC $acx_libxc_save_FCFLAGS"

# set from environment variable, if not blank
if test ! -z "$LIBS_LIBXC"; then
  LIBS="$LIBS_LIBXC $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

# static linkage, separate Fortran interface
if test x"$acx_libxc_ok" = xno; then
  LIBS_LIBXC="$with_libxc_path/lib/libxcf90.a $with_libxc_path/lib/libxc.a"
  LIBS="$LIBS_LIBXC $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

# dynamic linkage, separate Fortran interface
if test x"$acx_libxc_ok" = xno; then
  LIBS_LIBXC="-L$with_libxc_path/lib -lxcf90 -lxc"
  LIBS="$LIBS_LIBXC $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

# static linkage, combined Fortran interface (libxc pre-r10730)
if test x"$acx_libxc_ok" = xno; then
  LIBS_LIBXC="$with_libxc_path/lib/libxc.a"
  LIBS="$LIBS_LIBXC $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

# dynamic linkage, combined Fortran interface (libxc pre-r10730)
if test x"$acx_libxc_ok" = xno; then
  LIBS_LIBXC="-L$with_libxc_path/lib -lxc"
  LIBS="$LIBS_LIBXC $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

dnl The following programs will only work with specific version of libxc
testprog_21="AC_LANG_PROGRAM([],[
  use xc_f90_lib_m
  implicit none

  integer :: i
  i = XC_GGA_X_LV_RPW86])"

testprog_20="AC_LANG_PROGRAM([],[
  use xc_f90_lib_m
  implicit none
  integer :: major
  integer :: minor
  call xc_f90_version(major, minor)])"

testprog_12="AC_LANG_PROGRAM([],[
  use xc_f90_lib_m
  implicit none

  integer :: i
  i = XC_GGA_X_AIRY])"

testprog_11="AC_LANG_PROGRAM([],[
  use xc_f90_lib_m
  implicit none

  integer :: i
  i = XC_KINETIC])"

dnl Get libxc version
if test x"$acx_libxc_ok" = xyes; then
AC_LINK_IFELSE($testprog_11, [acx_libxc_version=110, acx_libxc_ok=no], [])
AC_LINK_IFELSE($testprog_12, [acx_libxc_version=120], [])
AC_LINK_IFELSE($testprog_20, [acx_libxc_version=200, acx_libxc_ok=yes], [])
AC_LINK_IFELSE($testprog_21, [acx_libxc_version=210], [])
AC_DEFINE_UNQUOTED([LIBXC_VERSION],[$acx_libxc_version],[Defined the LIBXC version.])
AC_MSG_RESULT([Found external LibXC version=$acx_libxc_version (should be >= 200)])			
fi

dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_libxc_ok" = xyes; then
  compile_libxc=no
  #	 
  for file in `find $libxc_include_path \( -name 'libxc*mod' -o -name 'xc_*mod' \)`;do	
    cp $file include/ 
  done
  AC_MSG_RESULT([                  ... Compatible external LibXC ($FCFLAGS_LIBXC $LIBS_LIBXC)])
  AC_DEFINE(HAVE_LIBXC, 1, [Defined if you have the LIBXC library.])
#else
#  AC_MSG_ERROR([Could not find required libxc library ( >= v 1.0.0).])
fi
fi

if test x"$acx_libxc_ok" = xno; then
  have_configured="no"
  LIBS_LIBXC="-lxc"
  AC_MSG_RESULT([Compatible external LibXC not found/specified. Internal used.])
  AC_MSG_CHECKING([the configuration of the LIBXC internal library])
  cd lib/libxc
  if test -f Makefile; then 
  have_configured="yes"
  else
  ./configure FCCPP="cpp -E -P -ansi" FC=$FC CC=$CC --prefix=$PWD/../../ >&/dev/null
  if test -f Makefile; then have_configured="yes";fi
  fi
  cd ../../
  AC_MSG_RESULT($have_configured)
  
  if test "x$have_configured" != xyes; then AC_MSG_ERROR([can't configure LIBXC ]); fi
fi 

AC_SUBST(FCFLAGS_LIBXC)
AC_SUBST(LIBS_LIBXC)
AC_SUBST(compile_libxc)
FCFLAGS="$acx_libxc_save_FCFLAGS"
LIBS="$acx_libxc_save_LIBS"
])dnl ACX_LIBXC
