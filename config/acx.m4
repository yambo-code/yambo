#
# Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
#
# Copyright (C) 2000-2005 A. Marini and the SELF team
#         http://www.fisica.uniroma2.it/~self       
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
    rm -f pointertest*
    AC_MSG_ERROR(failed to compile c program to find the size of a pointer)
  fi
  ac_pointersize=`./pointertest.x`;
  rm -f pointertest*
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
# AC_LANG_PREPROC(Fortran)
# ---------------------------
m4_define([AC_LANG_PREPROC(Fortran)],[
  # this should not be hardwired
  if test -z "$FCCPP"; then FCCPP="/lib/cpp"; fi
  AC_SUBST(FCCPP)
])

################################################
# Set various default FLAGS
# ----------------------------------
AC_DEFUN([ACX_WIDESETUP],
[
AC_REQUIRE([AC_CANONICAL_HOST])

AC_MSG_CHECKING([if the current OS is supported])
TIMER="ct_cclock.o"
case "${host}" in
 i?86*linux* | ia64*linux* | *x86*64* | *86*apple*)
   build_os="linux"
   TIMER="ct_etime.o"
   if test -z "$F90SUFFIX"; then F90SUFFIX=".f90"; fi
   case "${FC}" in
   *g95*)
     if test -z "$FCPPFLAGS"; then FCPPFLAGS="-P -traditional"; fi
     ;;
   *)
     if test -z "$FCPPFLAGS"; then FCPPFLAGS="-traditional"; fi
     ;;
   esac
   ;;
 powerpc64*linux* )
   build_os="linux"
   TIMER="ct_etime.o"
   if test -z "$FCPPFLAGS"; then FCPPFLAGS="-P -traditional-cpp"; fi
   if test -z "$F90SUFFIX"; then F90SUFFIX=".f"; fi
   ;;
 powerpc-ibm* )
   build_os="aix"  
   save=$AR_FLAGS
   AR_FLAGS="$save -X32_64"
   if test -z "$FCPPFLAGS"; then FCPPFLAGS="-P"; fi
   if test -z "$F90SUFFIX"; then F90SUFFIX=".f"; fi
   ;;
 mips-sgi-irix*)
   build_os="irix"  
   if test -z "$FCPPFLAGS"; then FCPPFLAGS="-P"; fi
   if test -z "$F90SUFFIX"; then F90SUFFIX=".f90"; fi
   ;;
 alphaev*)
   build_os="tru64"  
   if test -z "$FCPPFLAGS"; then FCPPFLAGS="-P -C"; fi
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
AC_MSG_NOTICE([WIDESETUP: using FCPPFLAGS="$FCPPFLAGS"])
AC_SUBST(F90SUFFIX)
AC_SUBST(TIMER)
AC_SUBST(AR)
AC_SUBST(AR_FLAGS)
AC_SUBST(FCPPFLAGS)
])

################################################
# Set FC FLAGS
# ----------------------------------
AC_DEFUN([ACX_FCSETUP],
[
AC_REQUIRE([AC_CANONICAL_HOST])

if test -z "${CFLAGS}"; then CFLAGS="-O2"; fi

if test -z "${FCFLAGS}"; then
  case "${host}" in
  i?86*linux*)
    case "${FC}" in
    *pgf90*)
      FCFLAGS="-O2 -fast -Munroll -Mnoframe -Mdalign"
      UFFLAGS="-O0"
      FCMFLAG="-Mnomain"
      ;;
    *abf90*)
      FCFLAGS="-B101 -YEXT_NAMES=LCS -YEXT_SFX=_"
      ;;
    *ifc*)
      FCFLAGS="-O3 -w -tpp7"
      UFFLAGS="-O0 -w -tpp7"
      FCMFLAG=""
      ;;
    *g95*)
      FCFLAGS="-O0 -fbackslash -g -fno-second-underscore"
      UFFLAGS="-O0 -fbackslash -g -fno-second-underscore"
      FCMFLAG=""
      ;;
    *ifort*)
      CPU_FLAG=""
      case "${FCVERSION}" in
        *10*)
         CPU_FLAG="-xT"
         ;;
        *)
         CPU_FLAG="-tpp7"
         ;;
      esac
      FCFLAGS="-assume bscc -O3 $CPU_FLAG"
      UFFLAGS="-assume bscc -O0 $CPU_FLAG"
      FCMFLAG="-nofor_main"
    ;;
    *)
      FCFLAGS="-O"
    esac
   ;;
  *86*apple* )
    case "${FC}" in
    *g95*)
      FCFLAGS="-O3 -fno-second-underscore -mtune=pentium4"
      UFFLAGS="-O0 -fno-second-underscore"
      FCMFLAG=""
      ;;
    *ifort*)
      CPU_FLAG=""
      case "${FCVERSION}" in
        *10*)
         CPU_FLAG="-mtune=pentium4"
         ;;
        *)
         CPU_FLAG="-mtune=pentium4"
         ;;
      esac
      FCFLAGS="-assume bscc -O3 $CPU_FLAG"
      UFFLAGS="-assume bscc -O0 $CPU_FLAG"
      FCMFLAG="-nofor_main"
      ;;
    *)
      FCFLAGS="-O"
    esac
    ;;
  ia64*linux* | *x86*64* )
    case "${FC}" in
    *pgf90*)
      FCFLAGS="-O2 -fast -Munroll -Mnoframe -Mdalign"
      FCMFLAG="-Mnomain"
      ;;
    *abf90*)
      FCFLAGS="-B101 -YEXT_NAMES=LCS -YEXT_SFX=_"
      ;;
    *ifc*)
      FCFLAGS="-O3 -w -tpp2"
      UFFLAGS="-O0 -w -tpp2"
      ;;
    *ifort*)
      FCFLAGS="-assume bscc -O3 -tpp7"
      UFFLAGS="-assume bscc -O0 -tpp7"
      FCMFLAG="-nofor_main"
      ;;
    *)
      FCFLAGS="-O"
    esac
    ;;
  alphaev*)
    FCFLAGS="-O3 -arch host -tune host"
    UFFLAGS="-O0"
    FCMFLAG="-nofor_main"
    ;;
  powerpc64*linux* )
    CFLAGS="-q64 -O2"
    FCFLAGS="-q64 -O3 -qnostrict -qarch=ppc970 -qtune=ppc970"
    UFFLAGS="-q64 -O0"
    ;;
  powerpc-ibm* )
    CFLAGS="-O -q64"
    FCFLAGS="-O3 -q64 -qstrict -qarch=pwr5 -qtune=pwr5 -qmaxmem=-1"
    UFFLAGS="-q64"
    ;;
  mips-sgi-irix*)
    FCFLAGS="-O3 -r10000 -mips4"
    ;;
  *)
    FCFLAGS="-O"
  esac
else
  case "${FC}" in
  *pgf90*)
    FCMFLAG="-Mnomain"
    ;;
  *ifort*)
    FCMFLAG="-nofor_main"
  esac
fi
AC_MSG_CHECKING([for specific $FC flags])
AC_MSG_RESULT([$FCFLAGS])
AC_MSG_CHECKING([for specific $CC flags])
AC_MSG_RESULT([$CFLAGS])
if ! test "$UFFLAGS" = ""; then 
 AC_MSG_CHECKING([for unoptimized flags])
 AC_MSG_RESULT([$UFFLAGS])
fi
FCUFLAGS=$UFFLAGS
AC_SUBST(CFLAGS)
AC_SUBST(FCFLAGS)
AC_SUBST(FCUFLAGS)
AC_SUBST(UFFLAGS)
AC_SUBST(FCMFLAG)
])

################################################
# Get MYECHO
# ----------------------------------
AC_DEFUN([ACX_MYECHO],
[
cmd_found=no
AC_PATH_PROG(TCSH, tcsh, :)

case "${host}" in
*86*apple*)
  nlines=`$TCSH -fc 'printf "a\nb\n" |wc -l'` 
;;
*)
  nlines=`$TCSH -fc 'echo "a\nb" |wc -l'`  
esac

if test "$nlines" -eq "2" ; then 
 cmd_found="yes"
 MYECHO="echo"
fi
AC_MSG_CHECKING([for built-in echo to accept newline sequence])
AC_MSG_RESULT([$cmd_found])

if test "$cmd_found" = "no"; then
  case "${host}" in
  *86*apple*)
     AC_PATH_PROG(save_MYECHO, printf, :)
     nlines=`$TCSH -fc 'set MYECHO='$save_MYECHO' ; $MYECHO "a\nb\n" |wc -l'`
  ;;
  *)
     AC_PATH_PROG(save_MYECHO, echo, :)
     nlines=`$TCSH -fc 'set MYECHO='$save_MYECHO' ; $MYECHO "a\nb" |wc -l'`
  esac
 if test "$nlines" -eq "2" ; then cmd_found="yes" MYECHO="$save_MYECHO" ; fi
 AC_MSG_CHECKING([for $save_MYECHO to accept newline sequence])
 AC_MSG_RESULT([$cmd_found])
fi
if test "$cmd_found" = "no"; then
 AC_MSG_ERROR(Echo command compatible with newline sequence not found)
fi
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
[#define F90_FUNC(name,NAME) name ## _
 #define F90_FUNC_(name,NAME) name ## _
 void F90_FUNC_(ftest, FTEST)(){}],[
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
 [#define F90_FUNC(name,NAME) name
  #define F90_FUNC_(name,NAME) name
  void F90_FUNC_(ftest, FTEST)(){}],[
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
