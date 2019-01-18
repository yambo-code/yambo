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
AC_DEFUN([ACX_CPP],
[
#
case "${CPP}" in
 *icc* )
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
