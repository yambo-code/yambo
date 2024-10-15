#
#        Copyright (C) 2000-2022 the YAMBO team
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
AC_DEFUN([AC_MAGMA_SETUP],[
#
AC_ARG_ENABLE(magma_linalg,   AS_HELP_STRING([--enable-magma-linalg],[Enable suport for the diagonalization of BSE using MAGMA. Default is no]))
#
AC_ARG_WITH(magma_libs,AS_HELP_STRING([--with-magma-libs=<libs>],[Use Magma libraries <libs>],[32]))
AC_ARG_WITH(magma_incs,AS_HELP_STRING([--with-magma-incs=<incs>],[Use Magma includes <incs>],[32]))
AC_ARG_WITH(magma_path, AS_HELP_STRING([--with-magma-path=<path>],[Path to the Magma install directory],[32]),[],[])
AC_ARG_WITH(magma_libdir,AS_HELP_STRING([--with-magma-libdir=<path>],[Path to the Magma lib directory],[32]))
AC_ARG_WITH(magma_includedir,AS_HELP_STRING([--with-magma-includedir=<path>],[Path to the Magma include directory],[32]))

#
def_magma=""
magma="no"
enable_magma="no"
internal_magma="no"
compile_magma="no"
compile_magma_fmodules="no"
#
if test x"$enable_magma_linalg" = "xyes"; then
  enable_magma="yes";
fi
#
# MAGMA global options
#
if test  x"$with_magma_libs" = "xyes" ; then
  enable_magma="yes" ;
  compile_magma_fmodules="yes" ;
  with_magma_libs="";
elif test  x"$with_magma_libs" = "xno" ; then
  enable_magma="no" ;
  compile_magma_fmodules="no" ;
  with_magma_libs="";
fi
#
if test  x"$with_magma_libdir" != "x" ; then enable_magma="yes" ; fi
if test  x"$with_magma_path"   != "x" ; then enable_magma="yes" ; fi
if test  x"$with_magma_libs"   != "x" ; then enable_magma="yes" ; fi
#
# Set MAGMA LIBS and FLAGS from INPUT
#
if test -d "$with_magma_path" || test -d "$with_magma_libdir" || test x"$with_magma_libs" != "x" ; then
  #
  # external magma
  #
  if   test   x"$with_magma_libs" != "x" ; then  AC_MSG_CHECKING([for Magma using $with_magma_libs]) ;
  elif test -d "$with_magma_libdir"      ; then  AC_MSG_CHECKING([for Magma in $with_magma_libdir]) ;
  elif test -d "$with_magma_path"        ; then  AC_MSG_CHECKING([for Magma in $with_magma_path]) ;
  fi
  #
  if test -d "$with_magma_path" ; then 
      try_magma_libdir="$with_magma_path/lib" ;
      try_magma_incdir="$with_magma_path/include" ;
  fi
  #
  if test -d "$with_magma_libdir"     ; then try_magma_libdir="$with_magma_libdir" ; fi
  if test -d "$with_magma_includedir" ; then try_magma_incdir="$with_magma_includedir" ; fi
  #
  try_MAGMA_INCS="$IFLAG$try_magma_incdir" ;
  try_MAGMA_LIBS="-L$try_magma_libdir -lmagma" ;
  #
  if test x"$with_magma_libs" != "x" ; then try_MAGMA_LIBS="$with_magma_libs" ; fi
  if test x"$with_magma_incs" != "x" ; then try_MAGMA_INCS="$with_magma_incs" ; fi
  #
  if test -z "$try_MAGMA_LIBS" ; then AC_MSG_ERROR([No libs specified]) ; fi
  if test -z "$try_MAGMA_INCS" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
  #
  AC_LANG([Fortran])
  #
  save_fcflags="$FCFLAGS" ;
  save_libs="$LIBS" ;
  #
  FCFLAGS="$try_MAGMA_INCS $save_fcflags";
  LIBS="$try_MAGMA_LIBS $save_libs";
  #
  AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [
use magma
implicit none
integer :: lda
!magma_devptr_t :: dA]),
       [magma=yes], [magma=no]);
  #
  if test "x$magma" = "xyes"; then
    AC_MSG_RESULT([yes]) ;
    MAGMA_INCS="$try_MAGMA_INCS" ;
    MAGMA_LIBS="$try_MAGMA_LIBS" ;
    compile_magma="no";
    internal_magma="no";
    def_magma="-D_MAGMA"
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
# TO BE FIXED: needs internal compilation support and paths have to be corrected with GPU_SUPPORT folder
#
if test "x$enable_magma" = "xyes" && test "x$magma" = "xno" ; then
  #
  # internal magma
  #
  AC_MSG_CHECKING([for internal Magma library])
  #
  internal_magma="yes"
  #
  #if test "x$lapack_shared" = "x1" ; then
  #  MAGMA_LIBS="${extlibs_path}/${FCKIND}/${FC}/lib/libmagma.so" ;
  #  #MAGMA_LIBS="" ;
  #else
    MAGMA_LIBS="${extlibs_path}/${FCKIND}/${FC}/lib/libmagma.a" ;
  #fi
  MAGMA_INCS="${IFLAG}${extlibs_path}/${FCKIND}/${FC}/include" ;
  #
  magma=yes
  def_magma="-D_MAGMA"
  if test -e "${extlibs_path}/${FCKIND}/${FC}/lib/libmagma.a" ; then
    compile_magma="no" ;
    compile_magma_fmodules="no" ;
    AC_MSG_RESULT([already compiled]) ;
  elif test -e "${extlibs_path}/${FCKIND}/${FC}/lib/libmagma.so" ; then
    compile_magma="no" ;
    compile_magma_fmodules="no" ;
    AC_MSG_RESULT([already compiled]) ;
  else
    compile_magma="yes" ;
    compile_magma_fmodules="no" ;
    AC_MSG_RESULT([Compatible external Magma not found/specified. To be compiled.]) ;
  fi
  #
fi
#
# Check if fortran modules are available
#
if test -e "$MAGMA_INCS/mod_magma2_common.F" ; then   compile_magma_fmodules="no" ;  fi
#
# switch off internal magma compilation
#
deactivate_internal=no
if test "x$compile_magma" = "xyes" && test "x$internal_magma" = "xyes" && test "x$deactivate_internal" = "xyes"  ; then
  AC_MSG_RESULT([Internal Magma compilation not available yet. Deactivating it.]) ;
  compile_magma="no"
  def_magma=""
  enable_magma="no"
  MAGMA_INCS="" ;
  MAGMA_LIBS="" ;
fi
#
AC_SUBST(MAGMA_LIBS)
AC_SUBST(MAGMA_INCS)
AC_SUBST(def_magma)
AC_SUBST(enable_magma)
AC_SUBST(compile_magma)
AC_SUBST(compile_magma_fmodules)
AC_SUBST(internal_magma)
#
])
