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
AC_ARG_WITH(libxc_libs, [AS_HELP_STRING([--with-libxc-libs=<libs>], 
            [Use libxc libraries <libs>],[32])])
AC_ARG_WITH(libxc_path, [AS_HELP_STRING([--with-libxc-path=<path>], 
            [Path to libxc install directory],[32])])
AC_ARG_WITH(libxc_libdir, [AS_HELP_STRING([--with-libxc-libdir=<path>], 
            [Path to the libxc lib directory],[32])])
AC_ARG_WITH(libxc_includedir, [AS_HELP_STRING([--with-libxc-includedir=<path>], 
            [Path to the libxc include directory],[32])])


internal_libxc="no"
compile_libxc="no"

if test -d "$with_libxc_path"; then
   libxc_incdir="$with_libxc_path/include"
   libxc_libdir="$with_libxc_path/lib"
fi
if test -d "$with_libxc_includedir"; then libxc_incdir="$with_libxc_includedir" ; fi
if test -d "$with_libxc_libdir";     then libxc_libdir="$with_libxc_libdir" ; fi

LIBXC_INCS="$IFLAG$libxc_incdir"

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

FCFLAGS="$LIBXC_INCS $acx_libxc_save_FCFLAGS"

# set from environment variable, if not blank
if test ! -z "$LIBXC_LIBS"; then
  LIBS="$LIBXC_LIBS"
dnl $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

# set from --with-libxc-libs flag
if test x"$acx_libxc_ok" = xno && test ! -z "$with_libxc_libs" ; then
  LIBXC_LIBS="$with_libxc_libs"
  LIBS="$LIBXC_LIBS"
dnl $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

# dynamic linkage, separate Fortran interface
if test x"$acx_libxc_ok" = xno; then
  LIBXC_LIBS="-L$libxc_libdir -lxcf90 -lxc"
  LIBS="$LIBXC_LIBS"
dnl $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

# static linkage, separate Fortran interface
if test x"$acx_libxc_ok" = xno; then
  LIBXC_LIBS="$libxc_libdir/libxcf90.a $libxc_libdir/libxc.a"
  LIBS="$LIBXC_LIBS"
dnl $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

# static linkage, combined Fortran interface (libxc pre-r10730)
if test x"$acx_libxc_ok" = xno; then
  LIBXC_LIBS="$libxc_libdir/libxc.a"
  LIBS="$LIBXC_LIBS"
dnl $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

# dynamic linkage, combined Fortran interface (libxc pre-r10730)
if test x"$acx_libxc_ok" = xno; then
  LIBXC_LIBS="-L$libxc_libdir -lxc"
  LIBS="$LIBXC_LIBS"
dnl $acx_libxc_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libxc_ok=yes], [])
fi

dnl The following programs will only work with specific version of libxc
testprog_21="AC_LANG_PROGRAM([],[
  use xc_f90_lib_m
  implicit none

  integer :: i
  i = XC_GGA_X_LV_RPW86])"

testprog_203="AC_LANG_PROGRAM([],[
  use xc_f90_lib_m
  use xc_f90_types_m
  implicit none
  type(xc_f90_pointer_t) :: conf
  real(8) :: alpha
  real(8) :: gamma
  call xc_f90_hyb_gga_xc_hse_set_par(conf,alpha,gamma)])"

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
AC_LINK_IFELSE($testprog_20, [acx_libxc_version=200], [])
AC_LINK_IFELSE($testprog_203, [acx_libxc_version=203, acx_libxc_ok=yes], [])
AC_LINK_IFELSE($testprog_21, [acx_libxc_version=210], [])
AC_DEFINE_UNQUOTED([LIBXC_VERSION],[$acx_libxc_version],[Defined the LIBXC version.])
AC_MSG_RESULT([Found external LibXC version=$acx_libxc_version (should be >= 203)])			
fi

dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_libxc_ok" = xyes; then
  compile_libxc=no
  internal_libxc=no
  #
  AC_DEFINE(HAVE_LIBXC, 1, [Defined if you have the LIBXC library.])
fi

if test x"$acx_libxc_ok" = xno; then
  have_configured="no"
  internal_libxc="yes"
  # version y2.0.3
  #LIBXC_LIBS="-L${extlibs_path}/${FCKIND}/${FC}/lib -lxc"
  # version 2.2.3 is used
  LIBXC_LIBS="-L${extlibs_path}/${FCKIND}/${FC}/lib -lxcf90 -lxc"
  LIBXC_INCS="$IFLAG${extlibs_path}/${FCKIND}/${FC}/include"
  if test -e "${extlibs_path}/${FCKIND}/${FC}/lib/libxc.a" && test -e "${extlibs_path}/${FCKIND}/${FC}/lib/libxcf90.a"; then
    compile_libxc="no"
    AC_MSG_RESULT([Compatible external LibXC not found/specified. Found internal already compiled.])
  else
    compile_libxc="yes"
    AC_MSG_RESULT([Compatible external LibXC not found/specified. Internal to be compiled.])
  fi
fi 

AC_SUBST(LIBXC_LIBS)
AC_SUBST(LIBXC_INCS)
AC_SUBST(compile_libxc)
AC_SUBST(internal_libxc)
FCFLAGS="$acx_libxc_save_FCFLAGS"
LIBS="$acx_libxc_save_LIBS"
])dnl ACX_LIBXC

