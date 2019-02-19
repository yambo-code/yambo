#
#        Copyright (C) 2000-2019 the YAMBO team
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
AC_DEFUN([AC_PETSC_SLEPC_SETUP],[
#
AC_ARG_ENABLE(slepc_linalg,   AC_HELP_STRING([--enable-slepc-linalg],         [Enable suport for the diagonalization of BSE using SLEPC. Default is no]))
#
AC_ARG_WITH(slepc_libs,AC_HELP_STRING([--with-slepc-libs=<libs>],
            [Use Slepc libraries <libs>],[32]))
AC_ARG_WITH(slepc_incs,AC_HELP_STRING([--with-slepc-incs=<incs>],
            [Use Slepc includes <incs>],[32]))
AC_ARG_WITH(slepc_path, AC_HELP_STRING([--with-slepc-path=<path>],
            [Path to the Slepc install directory],[32]),[],[])
AC_ARG_WITH(slepc_libdir,AC_HELP_STRING([--with-slepc-libdir=<path>],
            [Path to the Slepc lib directory],[32]))
AC_ARG_WITH(slepc_includedir,AC_HELP_STRING([--with-slepc-includedir=<path>],
            [Path to the Slepc include directory],[32]))
#
AC_ARG_WITH(petsc_libs,AC_HELP_STRING([--with-petsc-libs=<libs>],
            [Use Petsc libraries <libs>],[32]))
AC_ARG_WITH(petsc_incs,AC_HELP_STRING([--with-petsc-incs=<incs>],
            [Use Petsc includes <incs>],[32]))
AC_ARG_WITH(petsc_path, AC_HELP_STRING([--with-petsc-path=<path>],
            [Path to the Petsc install directory],[32]),[],[])
AC_ARG_WITH(petsc_libdir,AC_HELP_STRING([--with-petsc-libdir=<path>],
            [Path to the Petsc lib directory],[32]))
AC_ARG_WITH(petsc_includedir,AC_HELP_STRING([--with-petsc-includedir=<path>],
            [Path to the Petsc include directory],[32]))

#
def_slepc=""
petsc="no"
slepc="no"
enable_slepc="no"
enable_petsc="no"
internal_petsc="no"
internal_slepc="no"
compile_petsc="no"
compile_slepc="no"
#
# Other libs
#
AC_LANG_PUSH(C)
AC_CHECK_LIB(dl, dlopen,  [use_libdl="yes";  ],[use_libdl="no";  ],[])
AC_LANG_POP(C)
#
if test x"$enable_slepc_linalg" = "xyes"; then
  enable_petsc="yes";
  enable_slepc="yes";
fi
#
# PETSC global options
#
if test  x"$with_petsc_libs" = "xyes" ; then
  enable_petsc="yes" ;
  with_petsc_libs="";
elif test  x"$with_petsc_libs" = "xno" ; then
  enable_petsc="no" ;
  with_petsc_libs="";
fi
#
if test  x"$with_petsc_libdir" != "x" ; then enable_petsc="yes" ; fi
if test  x"$with_petsc_path"   != "x" ; then enable_petsc="yes" ; fi
if test  x"$with_petsc_libs"   != "x" ; then enable_petsc="yes" ; fi
#
# Set PETSC LIBS and FLAGS from INPUT
#
if test -d "$with_petsc_path" || test -d "$with_petsc_libdir" || test x"$with_petsc_libs" != "x" ; then
  #
  # external petsc
  #
  if   test   x"$with_petsc_libs" != "x" ; then  AC_MSG_CHECKING([for Petsc using $with_petsc_libs]) ;
  elif test -d "$with_petsc_libdir"      ; then  AC_MSG_CHECKING([for Petsc in $with_petsc_libdir]) ;
  elif test -d "$with_petsc_path"        ; then  AC_MSG_CHECKING([for Petsc in $with_petsc_path]) ;
  fi
  #
  if test -d "$with_petsc_path" ; then 
      try_petsc_libdir="$with_petsc_path/lib" ;
      try_petsc_incdir="$with_petsc_path/include" ;
  fi
  #
  if test -d "$with_petsc_libdir"     ; then try_petsc_libdir="$with_petsc_libdir" ; fi
  if test -d "$with_petsc_includedir" ; then try_petsc_incdir="$with_petsc_includedir" ; fi
  #
  try_PETSC_INCS="$IFLAG$try_petsc_incdir" ;
  try_PETSC_LIBS="-L$try_petsc_libdir -lpetsc" ;
  #
  if test "$use_libdl"    = "yes"; then try_PETSC_LIBS="$try_PETSC_LIBS -ldl"   ; fi
  #
  if test x"$with_petsc_libs" != "x" ; then try_PETSC_LIBS="$with_petsc_libs" ; fi
  if test x"$with_petsc_incs" != "x" ; then try_PETSC_INCS="$with_petsc_incs" ; fi
  #
  if test -z "$try_PETSC_LIBS" ; then AC_MSG_ERROR([No libs specified]) ; fi
  if test -z "$try_PETSC_INCS" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
  #
  AC_LANG([Fortran])
  #
  save_fcflags="$FCFLAGS" ;
  save_libs="$LIBS" ;
  #
  FCFLAGS="$try_PETSC_INCS $save_fcflags";
  LIBS="$try_PETSC_LIBS $save_libs";
  #
  AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h90>]),
       [petsc=yes], [petsc=no]);
  #
  if test "x$petsc" = "xyes"; then
    AC_MSG_RESULT([yes]) ;
    PETSC_INCS="$try_PETSC_INCS" ;
    PETSC_LIBS="$try_PETSC_LIBS" ;
    compile_petsc="no";
    internal_petsc="no";
  else
    AC_MSG_RESULT([no]) ;
    #
  fi
  # 
  FCFLAGS="$save_fcflags" ;
  LIBS="$save_libs" ;
  # 
fi
#
if test "x$enable_petsc" = "xyes" && test "x$petsc" = "xno" ; then
  #
  # internal petsc
  #
  AC_MSG_CHECKING([for internal Petsc library])
  #
  internal_petsc="yes"
  #
  PETSC_LIBS="${extlibs_path}/${FCKIND}/${FC}/${build_precision}/lib/libpetsc.a" ;
  PETSC_INCS="${IFLAG}${extlibs_path}/${FCKIND}/${FC}/${build_precision}/include" ;
  #
  if test "$use_libdl"    = "yes"; then PETSC_LIBS="$PETSC_LIBS -ldl"   ; fi
  #
  petsc=yes
  if test -e "${extlibs_path}/${FCKIND}/${FC}/${build_precision}/lib/libpetsc.a" ; then
    compile_petsc="no" ;
    AC_MSG_RESULT([already compiled]) ;
  else
    compile_petsc="yes" ;
    AC_MSG_RESULT([to be compiled]) ;
  fi
  #
fi
#
# SLEPC global options
#
#
if test  x"$with_slepc_libs" = "xyes" ; then
  enable_slepc="yes" ;
  with_slepc_libs="";
elif test  x"$with_slepc_libs" = "xno" ; then
  enable_slepc="no" ;
  with_slepc_libs="";
fi
#
if test x"$with_slepc_libdir" != "x" ; then enable_slepc="yes" ; fi
if test x"$with_slepc_path"   != "x" ; then enable_slepc="yes" ; fi
if test x"$with_slepc_libs"   != "x" ; then enable_slepc="yes" ; fi
#
# Set SLEPC LIBS and FLAGS from INPUT
#
if test "x$compile_petsc" = "xno" && test "x$enable_petsc" = "xyes" ; then
if test -d "$with_slepc_path" || test -d "$with_slec_libdir" || test x"$with_slepc_libs" != "x" ; then
  #
  # external slepc
  #
  if   test   x"$with_slepc_libs" != "x" ; then  AC_MSG_CHECKING([for Slepc using $with_slepc_libs]) ;
  elif test -d "$with_slepc_libdir"      ; then  AC_MSG_CHECKING([for Slepc in $with_slepc_libdir]) ;
  elif test -d "$with_slepc_path"        ; then  AC_MSG_CHECKING([for Slepc in $with_slepc_path]) ;
  fi
  #
  if test -d "$with_slepc_path" ; then 
      try_slepc_libdir="$with_slepc_path/lib" ;
      try_slepc_incdir="$with_slepc_path/include" ;
  fi
  #
  if test -d "$with_slepc_libdir"     ; then try_slepc_libdir="$with_slepc_libdir" ; fi
  if test -d "$with_slepc_includedir" ; then try_slepc_incdir="$with_slepc_includedir" ; fi
  #
  try_SLEPC_INCS="$IFLAG$try_slepc_incdir" ;
  try_SLEPC_LIBS="-L$try_slepc_libdir -lslepc" ;
  #
  if test x"$with_slepc_libs" != "x" ; then  try_SLEPC_LIBS="$with_slepc_libs" ; fi
  if test x"$with_slepc_incs" != "x" ; then  try_SLEPC_INCS="$with_slepc_incs" ; fi
  #
  if test -z "$try_SLEPC_LIBS" ; then AC_MSG_ERROR([No libs specified]) ; fi
  if test -z "$try_SLEPC_INCS" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
  #
  AC_LANG([Fortran])
  #
  save_fcflags="$FCFLAGS" ;
  save_libs="$LIBS" ;
  #
  FCFLAGS="$try_SLEPC_INCS $PETSC_INCS $save_fcflags";
  LIBS="$try_SLEPC_LIBS $PETSC_LIBS $save_libs";
  #
  AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>]),
       [slepc=yes], [slepc=no]);
  #
  if test "x$slepc" = "xyes"; then
    AC_MSG_RESULT([yes]) ;
    SLEPC_INCS="$try_SLEPC_INCS" ;
    SLEPC_LIBS="$try_SLEPC_LIBS" ;
    compile_slepc="no";
    internal_slepc="no";
    def_slepc="-D_SLEPC";
  else
    AC_MSG_RESULT([no]) ;
    #
  fi
  # 
  FCFLAGS="$save_fcflags" ;
  LIBS="$save_libs" ;
  # 
fi
fi
#
if test "x$enable_slepc" = "xyes" && test "x$slepc" = "xno" && test "x$enable_petsc" = "xyes" ; then
  #
  # internal slepc
  #
  AC_MSG_CHECKING([for internal Slepc library])
  #
  internal_slepc="yes";
  #
  SLEPC_LIBS="${extlibs_path}/${FCKIND}/${FC}/${build_precision}/lib/libslepc.a" ;
  SLEPC_INCS="${IFLAG}${extlibs_path}/${FCKIND}/${FC}/${build_precision}/include" ;
  #
  slepc=yes
  if test -e "${extlibs_path}/${FCKIND}/${FC}/${build_precision}/lib/libslepc.a" ; then
    compile_slepc="no" ;
    AC_MSG_RESULT([already compiled]) ;
  else
    compile_slepc="yes" ;
    AC_MSG_RESULT([to be compiled]) ;
  fi
  def_slepc="-D_SLEPC";
  #
fi
#
AC_SUBST(PETSC_LIBS)
AC_SUBST(PETSC_INCS)
AC_SUBST(SLEPC_LIBS)
AC_SUBST(SLEPC_INCS)
AC_SUBST(def_slepc)
AC_SUBST(enable_petsc)
AC_SUBST(enable_slepc)
AC_SUBST(compile_petsc)
AC_SUBST(compile_slepc)
AC_SUBST(internal_petsc)
AC_SUBST(internal_slepc)
#
])
