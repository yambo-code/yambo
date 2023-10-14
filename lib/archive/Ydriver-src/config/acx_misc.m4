#
# Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
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
################################################
# Check size of a pointer
# ----------------------------------
AC_DEFUN([ACX_POINTER_SIZE],
[AC_MSG_CHECKING([for the size of a pointer])
  AC_REQUIRE([AC_PROG_CC])
  if test -z "$POINTER_SIZE"; then
  cat >pointertest.c <<EOF
#include <stdio.h>
void main()
{
  printf("%ld", sizeof(void *));
}
EOF
  ac_try='$CC $CFLAGS -o pointertest.x pointertest.c 1>&AC_FD_CC'
  if AC_TRY_EVAL(ac_try); then
    ac_try=""
  else
    echo "configure: failed program was:" >&AC_FD_CC
    cat pointertest.c >&AC_FD_CC
    rm -rf pointertest*
    AC_MSG_ERROR(failed to compile c program to find the size of a pointer)
  fi
  ac_pointersize=`./pointertest.x`;
  rm -rf pointertest*
  AC_DEFINE_UNQUOTED(POINTER_SIZE, ${ac_pointersize}, [The size of a C pointer])
  AC_MSG_RESULT([${ac_pointersize} bytes])
fi
])
################################################
# AC_LANG_FUNC_LINK_TRY(Fortran)(FUNCTION)
# ----------------------------------
m4_define([AC_LANG_FUNC_LINK_TRY(Fortran)],
[AC_LANG_PROGRAM([], [call [$1]])])

################################################
# Set various default FLAGS
# ----------------------------------
AC_DEFUN([ACX_WIDESETUP],
[
AC_REQUIRE([AC_CANONICAL_HOST])

AC_MSG_CHECKING([if the current OS is supported])
#TIMER="ct_cclock.o"
TIMER="ct_cptimer.o"
case "${host}" in
 i?86*linux* | ia64*linux* | *x86*64* | *86*cygwin )
   build_os="linux"
   #TIMER="ct_etime.o"
   if test -z "$F90SUFFIX"; then F90SUFFIX=".f90"; fi
   ;;
 *86*apple* )
   build_os="apple"
   #TIMER="ct_etime.o"
   if test -z "$F90SUFFIX"; then F90SUFFIX=".f90"; fi
   ;;
 powerpc64*linux* )
   build_os="linux"
   #TIMER="ct_etime.o"
   if test -z "$F90SUFFIX"; then F90SUFFIX=".f90"; fi
   ;;
 powerpc-ibm* )
   build_os="aix"  
   save=$AR_FLAGS
   AR_FLAGS="$save -X32_64"
   if test -z "$F90SUFFIX"; then F90SUFFIX=".f90"; fi
   ;;
 mips-sgi-irix*)
   build_os="irix"  
   if test -z "$F90SUFFIX"; then F90SUFFIX=".f90"; fi
   ;;
 alphaev*)
   build_os="tru64"  
   if test -z "$F90SUFFIX"; then F90SUFFIX=".f90"; fi
   ;;
  *)
   AC_MSG_RESULT([no])
   AC_MSG_NOTICE(Platform <${host}> is not supported.)
   AC_MSG_NOTICE(Please contact the $PACKAGE_NAME team at $PACKAGE_BUGREPORT)
   AC_MSG_NOTICE(providing either a list of compilers and options or)
   AC_MSG_NOTICE(a guest account on this machine.) 
   AC_MSG_ERROR(stopping) 
   ;;
 *)
esac
AC_MSG_RESULT([yes])

AC_MSG_NOTICE([WIDESETUP: using build_os="$build_os"])
AC_MSG_NOTICE([WIDESETUP: using F90SUFFIX="$F90SUFFIX"])
AC_MSG_NOTICE([WIDESETUP: using AR="$AR"])
AC_MSG_NOTICE([WIDESETUP: using AR_FLAGS="$AR_FLAGS"])
AC_SUBST(F90SUFFIX)
AC_SUBST(TIMER)
AC_SUBST(AR)
AC_SUBST(AR_FLAGS)
])


################################################
# Get External C routines naming scheme
# ----------------------------------
AC_DEFUN([ACX_EXTUS],
[
c_success=no
msg="unknown"
AC_LANG_PUSH(C)
AC_COMPILE_IFELSE(
[AC_LANG_SOURCE(#define F90_FUNC(name,NAME) name ## _
 #define F90_FUNC_(name,NAME) name ## _
 void F90_FUNC_(ftest, FTEST)(){})],[
 mv conftest.$ac_objext ftest.$ac_objext
 AC_LANG_PUSH(Fortran)
 save="$LIBS"
 LIBS="ftest.$ac_objext"
 AC_LINK_IFELSE([AC_LANG_CALL([], [ftest])],
                 [c_success="yes"; msg="test_"; save="$CFLAGS"; CFLAGS="$save -D_C_US"])
 LIBS="$save"
 AC_LANG_POP(Fortran)
 rm -f ftest.$ac_objext],[])
if test "$c_success" = "no" ; then
 AC_COMPILE_IFELSE(
 [AC_LANG_SOURCE(#define F90_FUNC(name,NAME) name
  #define F90_FUNC_(name,NAME) name
  void F90_FUNC_(ftest, FTEST)(){})],[
  mv conftest.$ac_objext ftest.$ac_objext
  AC_LANG_PUSH(Fortran)
  save="$LIBS"
  LIBS="ftest.$ac_objext"
  AC_LINK_IFELSE([AC_LANG_CALL([], [ftest])],
                 [c_success="yes"; msg="test"])
  LIBS="$save"
  AC_LANG_POP(Fortran)
  rm -f ftest.$ac_objext],[])
fi
AC_MSG_CHECKING([for external C routine (test) naming scheme])
AC_MSG_RESULT([$msg])

AC_FC_WRAPPERS
f_success="no"
case $ac_cv_fc_mangling in
 "lower case, underscore, no extra underscore")
    f_success="yes" 
    save="$CFLAGS"
    CFLAGS="$save -D_FORTRAN_US" ;;
 "lower case, underscore, extra underscore")
    f_success="no" ;;
 "lower case, no underscore, no extra underscore")
    f_success="yes";;
 "lower case, no underscore, extra underscore")
    f_success="yes"
    save="$CFLAGS"
    CFLAGS="$save -D_FORTRAN_US" ;;
 "upper case, underscore, no extra underscore")
    f_success="yes"
    save="$CFLAGS"
    CFLAGS="$save -D_FORTRAN_US" ;;
 "upper case, underscore, extra underscore")
    f_success="no" ;;
 "upper case, no underscore, no extra underscore")
    f_success="yes";;
 "upper case, no underscore, extra underscore")
    f_success="yes"
    save="$CFLAGS"
    CFLAGS="$save -D_FORTRAN_US" ;;
esac
AC_LANG_POP(C)
if test "$c_success" = "no" || test "$f_success" = "no" ; then 
 AC_MSG_ERROR(unknown Fortran <-> C subroutines name conventions)
fi

])
