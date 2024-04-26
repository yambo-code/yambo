#
# License-Identifier: GPL
#
# Copyright (C) 2008 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([ACX_CPP],
[
#
case "${CPP}" in
 *icc* )
   if test -z "$CPPFLAGS"; then CPPFLAGS="-ansi"; fi
   ;;
 *icx* )
   if test -z "$CPPFLAGS"; then CPPFLAGS="-ansi"; fi
   ;;
 *gcc* )
   case "${host}" in
     *86*apple* )
       if test -z "$CPPFLAGS"; then CPPFLAGS="-P -D_apple"; fi
       ;;
     * )
       if test -z "$CPPFLAGS"; then CPPFLAGS="-P"; fi
       ;;
   esac
   ;;
 *cpp* )
   case "${host}" in
     i?86*linux* | ia64*linux* | *x86*64* )
     case "${FC}" in
       *g95*)
         if test -z "$CPPFLAGS"; then CPPFLAGS="-P -traditional"; fi
         ;;
       *)
         if test -z "$CPPFLAGS"; then CPPFLAGS="-traditional"; fi
         ;;
     esac
     ;;
     *86*apple* )
       if test -z "$CPPFLAGS"; then CPPFLAGS="-P -traditional -D_apple"; fi
       ;;
     powerpc64*linux* )
       if test -z "$CPPFLAGS"; then CPPFLAGS="-E -traditional"; fi
       ;;
     powerpc-ibm* )
       if test -z "$CPPFLAGS"; then CPPFLAGS="-P"; fi
       ;;
     mips-sgi-irix*)
       if test -z "$CPPFLAGS"; then CPPFLAGS="-P"; fi
       ;;
     alphaev*)
       if test -z "$CPPFLAGS"; then CPPFLAGS="-P -C"; fi
       ;;
    esac
    ;;
esac
#
#
AC_MSG_NOTICE([testing C-preprocessor $CPP $CPPFLAGS])
#
# TESTS
#=======
#
CPP_TESTS_PASSED=yes
#
# Select C to use the CPP in AC_PREPROC_IFELSE
#
AC_LANG(C)
#
acx_C_ok=no
AC_MSG_CHECKING([if C precompiler works on C source])
AC_PREPROC_IFELSE([
 AC_LANG_SOURCE([
 #if defined _C_US
  #define F90_FUNC_(name,NAME) name ## _
 #else
  #define F90_FUNC_(name,NAME) name
 #endif
 ])],
 [acx_C_ok=yes],[CPP_TESTS_PASSED=no])
AC_MSG_RESULT([$acx_C_ok])
#
if test "x$CPP_TESTS_PASSED" = xno ; then
  AC_MSG_ERROR(Found C precompiler problems in processing C source.);
fi
#
# AS CPPFLAGS are used (dunno why) in the MPI check of MPICC
# we need to rename the CPPFLAGS as CPPFLAGS_yambo

CPPFLAGS_yambo=$CPPFLAGS
CPPFLAGS=""
#
AC_SUBST(CPPFLAGS_yambo)
#
])
