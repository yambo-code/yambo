#
#
#        Copyright (C) 2000-2021 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): DS
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
AC_DEFUN([ACX_LIBCUDA], [

dnl Check if the library was given in the command line
dnl if not, use environment variables or defaults
AC_ARG_WITH(libcuda_libs, [AS_HELP_STRING([--with-libcuda-libs=<libs>], 
            [Use libcuda libraries <libs>],[32])])
AC_ARG_WITH(libcuda_path, [AS_HELP_STRING([--with-libcuda-path=<path>], 
            [Path to libcuda install directory],[32])])
AC_ARG_WITH(libcuda_libdir, [AS_HELP_STRING([--with-libcuda-libdir=<path>], 
            [Path to the libcuda lib directory],[32])])
AC_ARG_WITH(libcuda_includedir, [AS_HELP_STRING([--with-libcuda-includedir=<path>], 
            [Path to the libcuda include directory],[32])])


acx_cudalib_ok="no"
internal_libcuda="no"
compile_libcuda="no"
use_libcuda="no"

dnl Heuristics to detect CUDA dir
if test "x$with_libcuda_path" = "x" ; then with_libcuda_path="$CUDA_PATH" ; fi
if test "x$with_libcuda_path" = "x" ; then with_libcuda_path="$CUDA_ROOT" ; fi
if test "x$with_libcuda_path" = "x" ; then with_libcuda_path="$CUDA_HOME" ; fi

if test -d "$with_libcuda_path"; then
   libcuda_incdir="$with_libcuda_path/include"
   libcuda_libdir="$with_libcuda_path/lib"
   if ! test -d "$libcuda_libdir" ; then libcuda_libdir="$with_libcuda_path/lib64" ; fi
fi
if test -d "$with_libcuda_includedir"; then libcuda_incdir="$with_libcuda_includedir" ; fi
if test -d "$with_libcuda_libdir";     then libcuda_libdir="$with_libcuda_libdir"     ; fi

LIBCUDA_INCS="$IFLAG$libcuda_incdir"
LIBCUDA_PATH="$with_libcuda_path"


dnl Backup LIBS and FCFLAGS
acx_libcuda_save_LIBS="$LIBS"
acx_libcuda_save_FCFLAGS="$FCFLAGS"

#Test to be finalized, for now it is always going to succeed
dnl The tests
AC_MSG_CHECKING([for libcuda])

dnl The following program should work with all version of libcuda
testprog="AC_LANG_PROGRAM([],[
    implicit none

    integer :: ixx
])"

FCFLAGS="$LIBCUDA_INCS $acx_libcuda_save_FCFLAGS"

# set from environment variable, if not blank
if test ! -z "$LIBCUDA_LIBS"; then
  LIBS="$LIBCUDA_LIBS"
dnl $acx_libcuda_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libcuda_ok=yes], [])
fi

# set from --with-libcuda-libs flag
if test x"$acx_libcuda_ok" = xno && test ! -z "$with_libcuda_libs" ; then
  LIBCUDA_LIBS="$with_libcuda_libs"
  LIBS="$LIBCUDA_LIBS"
dnl $acx_libcuda_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libcuda_ok=yes], [])
fi

# dynamic linkage, separate Fortran interface
if test x"$acx_libcuda_ok" = xno; then
  LIBCUDA_LIBS="-L$libcuda_libdir -lcublas -lcudart -lcuda"
  LIBS="$LIBCUDA_LIBS"
dnl $acx_libcuda_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libcuda_ok=yes], [])
fi

# static linkage, separate Fortran interface
if test x"$acx_libcuda_ok" = xno; then
  LIBCUDA_LIBS="$libcuda_libdir/libcuda.a"
  LIBS="$LIBCUDA_LIBS"
dnl $acx_libcuda_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libcuda_ok=yes], [])
fi

# static linkage, combined Fortran interface (libcuda pre-r10730)
if test x"$acx_libcuda_ok" = xno; then
  LIBCUDA_LIBS="$libcuda_libdir/libcuda.a"
  LIBS="$LIBCUDA_LIBS"
dnl $acx_libcuda_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libcuda_ok=yes], [])
fi

# dynamic linkage, combined Fortran interface (libcuda pre-r10730)
if test x"$acx_libcuda_ok" = xno; then
  LIBCUDA_LIBS="-L$libcuda_libdir -lcublas -lcudart -lcuda"
  LIBS="$LIBCUDA_LIBS"
dnl $acx_libcuda_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libcuda_ok=yes], [])
fi


dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_libcuda_ok" = xyes; then
  use_libcuda=no
  compile_libcuda=no
  internal_libcuda=no
  #
  AC_DEFINE(HAVE_LIBCUDA, 1, [Defined if you have the LIBCUDA library.])
  AC_MSG_RESULT([Using version specified from input.])
else
  AC_MSG_RESULT([Not specified.])
fi

#
# Internal libcuda not available at the moment
#
#if test x"$acx_libcuda_ok" = xno; then
#  internal_libcuda="yes"
#  LIBCUDA_LIBS="${extlibs_path}/${FCKIND}/${FC}/lib/libcuda.a"
#  LIBCUDA_INCS="$IFLAG${extlibs_path}/${FCKIND}/${FC}/include"
#  if test -e "${extlibs_path}/${FCKIND}/${FC}/lib/libcuda.a"; then
#    compile_libcuda="no"
#    AC_MSG_RESULT([Compatible external DevXlib not found/specified. Found internal already compiled.])
#  else
#    compile_libcuda="yes"
#    AC_MSG_RESULT([Compatible external DevXlib not found/specified. Internal to be compiled.])
#  fi
#fi 

AC_SUBST(LIBCUDA_LIBS)
AC_SUBST(LIBCUDA_INCS)
AC_SUBST(LIBCUDA_PATH)
AC_SUBST(use_libcuda)
AC_SUBST(compile_libcuda)
AC_SUBST(internal_libcuda)

FCFLAGS="$acx_libcuda_save_FCFLAGS"
LIBS="$acx_libcuda_save_LIBS"

])
