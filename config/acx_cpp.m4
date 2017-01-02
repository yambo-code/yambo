#
#        Copyright (C) 2000-2016 the YAMBO team
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
AC_ARG_VAR(FPP,Fortran preprocessor)
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
case "${FC}" in
  #
  #  does not work properly
  #
  #*ifort*)
  #   if test -z "$FPP";    then FPP="${FC} -E -P"; fi
  #   ;;
  *gfortran | *g95)
     if test -z "$FPP";    then FPP="${FC} -E -P -cpp"; fi
     ;;
  #
  # some of the following could be uncommented once explicitly checked
  #
  #*sunf95)
  #   if test -z "$FPP";    then FPP="${FC} -E -P -fpp"; fi
  #   ;;
  #*openf95)
  #   if test -z "$FPP";    then FPP="${FC} -E -P -ftpp"; fi
  #   ;;
  #*pathf*)
  #   if test -z "$FPP";    then FPP="${FC} -E -P -cpp"; fi
  #   ;;
esac 
#
if test -z "$FPP" ; then FPP="cpp -E -P -ansi"; fi
#
AC_MSG_NOTICE([testing C-preprocessor $CPP $CPPFLAGS])
AC_MSG_NOTICE([testing F90-preprocessor $FPP])
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
# Fortran Source
#
acx_F90_ok=yes
FPP_TESTS_PASSED=yes
AC_MSG_CHECKING([if FC precompiler works on F90 source])
cat > conftest.F << EOF_
 program conftest
 character (1) :: a
 a="a"
 write (*,'('//a//')') 'hello'
 end program
EOF_
# ! Replace "S" with "\" and find the max length of
(eval $FPP conftest.F > conftest.${F90SUFFIX}) 2> conftest.er1
if ! test -s conftest.er1 || test -n "`grep successful conftest.er1`" ||
                             test -n "`grep "warning" conftest.er1`" ||
                             test -n "`grep "command line remark" conftest.er1`" ; then 
 eval $FPP conftest.F > conftest.${F90SUFFIX} 
 eval $FC $FCFLAGS -c conftest.${F90SUFFIX} 2> conftest.er2 >&5
 if test -s conftest.er2 ; then 
  if ! ( test -n "`grep successful conftest.er2`" ||
         test -n "`grep "warning" conftest.er2`" || 
         test -n "`grep "command line remark" conftest.er2`" ) ; then 
   acx_F90_ok=no ; 
   FPP_TESTS_PASSED=no;
  fi
 fi 
else
 acx_F90_ok=no ; 
 FPP_TESTS_PASSED=no
fi 
AC_MSG_RESULT([$acx_F90_ok])
#
if test "x$CPP_TESTS_PASSED" = xno ; then
  AC_MSG_ERROR(Found C precompiler problems in processing C source.);
fi
if test "x$FPP_TESTS_PASSED" = xno ; then
  AC_MSG_ERROR(Found FC precompiler problems in processing F90 source.);
fi
#
# AS CPPFLAGS are used (dunno why) in the MPI check of MPICC
# we need to rename the CPP precompiler in C_AS_CPP
C_AS_CPP=$CPP
C_AS_CPP_FLAGS=$CPPFLAGS
CPPFLAGS=""
AC_SUBST(C_AS_CPP)
AC_SUBST(C_AS_CPP_FLAGS)
AC_SUBST(FPP)
])
