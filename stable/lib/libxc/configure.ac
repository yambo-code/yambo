## Copyright (C) 2002-2006 M. Marques, A. Castro, A. Rubio, G. Bertsch
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.
##
## $Id: configure.ac 2422 2006-09-23 20:43:15Z marques $

AC_INIT([libxc],[1.2.0],[octopus-users@tddft.org],[libxc])
AC_CONFIG_SRCDIR([src/xc.h])
AM_INIT_AUTOMAKE

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])

# Installation prefix by default
AC_PREFIX_DEFAULT([/opt/etsf])

AC_DISABLE_SHARED
AC_PROG_LIBTOOL

# Checks for programs.
AC_PROG_CC
AC_LANG_PREPROC
AM_PROG_CC_C_O
AC_C_INLINE

# Checks for header files.
AC_HEADER_STDC
AC_FUNC_ALLOCA

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_SIZEOF(void*)

# Check for presence of some C99 libm functions
AC_CHECK_LIB([m],[sqrtf],[AC_DEFINE([HAVE_SQRTF],[1],[libm includes sqrtf])])
AC_CHECK_LIB([m],[cbrt],[AC_DEFINE([HAVE_CBRT],[1],[libm includes cbrt])])
AC_CHECK_LIB([m],[cbrtf],[AC_DEFINE([HAVE_CBRTF],[1],[libm includes cbrtf])])

dnl build/install fortran bits unless --disable-fortran is used
AC_ARG_ENABLE([fortran],
	      AS_HELP_STRING([--disable-fortran], [don't build/install fortran components]),
	      [ac_cv_build_fortran=$enableval],
	      [ac_cv_build_fortran=yes])
AM_CONDITIONAL([ENABLE_FORTRAN], [test $ac_cv_build_fortran = yes])

if test $ac_cv_build_fortran = yes; then
  HAVE_FORTRAN=1
  AC_DEFINE(HAVE_FORTRAN, [1], [Defined if libxc is compiled with fortran support])

  dnl try to find out what is the default FORTRAN 90 compiler
  acx_save_fcflags="${FCFLAGS}"
  AC_PROG_FC([], [Fortran 90])
  if test x"$FC" = x; then
    AC_MSG_ERROR([could not find Fortran 90 compiler])
  fi

  AC_LANG_PUSH(Fortran)
  AC_FC_SRCEXT(f90)
  FCFLAGS="${acx_save_fcflags}"

  dnl Fortran default flags
  ACX_FCFLAGS

  dnl libxc needs the preprocessor. The result goes to FCCPP
  dnl this is a hack, and should be changed in the future
  AC_LANG_PREPROC

  dnl how Fortran mangles function names
  AC_FC_WRAPPERS

  dnl check whether we have or not a compiler that allows for very long lines...
  ACX_LONG_FORTRAN_LINES

  dnl check whether the Fortran 90 compiler accepts line markers cast by the preprocessor.
  ACX_F90_ACCEPTS_LINE_NUMBERS

  AX_F90_MODULE_EXTENSION

  dnl Need to know the size of a Fortran integer
  ACX_FC_INTEGER_SIZE
  ACX_CC_FORTRAN_INT

  AC_LANG_POP([Fortran])
else
  HAVE_FORTRAN=0
fi
AC_SUBST(ax_cv_f90_modext)
AM_CONDITIONAL(F90_MOD_UPPERCASE, [test x$ax_f90_mod_uppercase = xyes])
AM_CONDITIONAL(COMPILE_FORTRAN, test x${HAVE_FORTRAN} = x1)

dnl should I compile libxc in single precision?
AC_ARG_ENABLE([single], 
	      AS_HELP_STRING([--enable-single], [compile libxc in single precision]),
              [ac_cv_single_prec=$enableval],
	      [ac_cv_single_prec=no])

AM_CONDITIONAL(COMPILE_SINGLE, test  $ac_cv_single_prec = yes)


AC_CONFIG_FILES([Makefile
  src/Makefile
  testsuite/Makefile
  build/Makefile
  build/libxc.pc
  build/libxc.spec
])

AC_OUTPUT

