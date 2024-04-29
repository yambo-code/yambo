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
AC_ARG_WITH(cuda_libs, [AS_HELP_STRING([--with-cuda-libs=<libs>], 
            [Use libcuda libraries <libs>],[32])])
AC_ARG_WITH(cuda_incs, [AS_HELP_STRING([--with-cuda-incs=<incs>], 
            [Use libcuda include options <incs>],[32])])
#
AC_ARG_WITH(cuda_libdir, [AS_HELP_STRING([--with-cuda-libdir=<path>], 
            [Path to the libcuda lib directory],[32])])
AC_ARG_WITH(cuda_includedir, [AS_HELP_STRING([--with-cuda-includedir=<path>], 
            [Path to the libcuda include directory],[32])])
#
AC_ARG_WITH(cuda_path, [AS_HELP_STRING([--with-cuda-path=<path>], 
            [Path to libcuda install directory],[32])])
            
#
AC_ARG_ENABLE([cuda-libs-check],
   [AS_HELP_STRING([--enable-cuda-libs-check=yes],[The configure script will check CUDA installation and report problems @<:@default=yes@:>@])],
   [],[enable_cuda_libs_check=yes])
#

acx_libcuda_ok="no"
internal_libcuda="no"
compile_libcuda="no"
use_libcuda="no"

dnl Backup LIBS and FCFLAGS
acx_libcuda_save_LIBS="$LIBS"
acx_libcuda_save_FCFLAGS="$FCFLAGS"

# Cuda libraries are needed only in one of the three following cases

if test x"$enable_cuda_fortran" != "xno" || test x"$enable_openacc" != "xno" ; then

#if test -z "$NVHPC" ; then
#
dnl Heuristics to detect CUDA dir
if test "x$with_cuda_path" = "x" ; then with_cuda_path="$CUDA_PATH" ; fi
if test "x$with_cuda_path" = "x" ; then with_cuda_path="$CUDA_ROOT" ; fi
if test "x$with_cuda_path" = "x" ; then with_cuda_path="$CUDA_HOME" ; fi

LIBCUDA_PATH="$with_cuda_path"

if test -d "$with_cuda_path"; then
   libcuda_incdir="$with_cuda_path/include"
   libcuda_libdir="$with_cuda_path/lib"
   if ! test -d "$libcuda_libdir" ; then libcuda_libdir="$with_cuda_path/lib64" ; fi
fi
if test -d "$with_cuda_includedir"; then libcuda_incdir="$with_cuda_includedir" ; fi
if test -d "$with_cuda_libdir";     then libcuda_libdir="$with_cuda_libdir"     ; fi


#Test to be finalized, for now it is always going to succeed
dnl The tests


AC_LANG_PUSH(Fortran)
dnl The following program should work with all version of libcuda
testprog="AC_LANG_PROGRAM([],[
  integer ierr
  ierr=cuInit
  ierr=cudaMalloc
  ierr=cublasInit
  ierr=cufftPlanMany
])"

LIBCUDA_INCS=""
if test x"$CUDA_INCS"      != "x" ; then LIBCUDA_INCS="$CUDA_INCS"            ; fi
if test x"$libcuda_incdir" != "x" ; then LIBCUDA_INCS="$IFLAG$libcuda_incdir" ; fi
if test x"$with_cuda_incs" != "x" ; then LIBCUDA_INCS="$with_cuda_incs"       ; fi

FCFLAGS="$LIBCUDA_INCS $acx_libcuda_save_FCFLAGS"

# set from environment variable, if not blank
if test ! -z "$CUDA_LIBS"; then
  AC_MSG_CHECKING([for libcuda from environment])
  LIBCUDA_LIBS="$CUDA_LIBS"
  LIBS="$LIBCUDA_LIBS"
dnl $acx_libcuda_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libcuda_ok=yes], [])
fi

# set from --with-cuda-libs flag
if test x"$acx_libcuda_ok" = xno && test ! -z "$with_cuda_libs" ; then
  AC_MSG_CHECKING([for libcuda from --with-cuda-libs])
  LIBCUDA_LIBS="$with_cuda_libs"
  LIBS="$LIBCUDA_LIBS"
dnl $acx_libcuda_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libcuda_ok=yes], [])
fi

# dynamic linkage, separate Fortran interface
if test x"$acx_libcuda_ok" = xno; then
  AC_MSG_CHECKING([for libcuda from specified libcuda path, dynamic])
  LIBCUDA_LIBS="-L$libcuda_libdir -lcufft -lcusolver -lcublas -lcudart -lcuda"
  LIBS="$LIBCUDA_LIBS"
dnl $acx_libcuda_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libcuda_ok=yes], [])
fi

# static linkage, separate Fortran interface
if test x"$acx_libcuda_ok" = xno; then
  AC_MSG_CHECKING([static])
  LIBCUDA_LIBS="$libcuda_libdir/libcuda.a"
  LIBS="$LIBCUDA_LIBS"
dnl $acx_libcuda_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_libcuda_ok=yes], [])
fi

dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_libcuda_ok" = xyes; then
  use_libcuda=yes
  compile_libcuda=no
  internal_libcuda=no
  #
  AC_DEFINE(HAVE_LIBCUDA, 1, [Defined if you have the LIBCUDA library.])
  AC_MSG_RESULT([yes.])
else
  AC_MSG_RESULT([not found.])
  use_libcuda=no
  compile_libcuda=no
  internal_libcuda=no
  LIBCUDA_LIBS=""
  LIBCUDA_INCS=""
  LIBCUDA_PATH=""
fi
AC_LANG_POP(Fortran)

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

#else
#  use_libcuda=yes
#  compile_libcuda=no
#  internal_libcuda=no
#  AC_DEFINE(HAVE_LIBCUDA, 1, [Defined if you have the LIBCUDA library.])
#  AC_MSG_RESULT([yes.])
#fi
fi


FCFLAGS="$acx_libcuda_save_FCFLAGS"
LIBS="$acx_libcuda_save_LIBS"

AC_SUBST(LIBCUDA_LIBS)
AC_SUBST(LIBCUDA_INCS)
AC_SUBST(LIBCUDA_PATH)
AC_SUBST(use_libcuda)
AC_SUBST(compile_libcuda)
AC_SUBST(internal_libcuda)

])
