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
AC_DEFUN([ACX_FPP],
[
#
AC_ARG_VAR(FPP,Fortran preprocessor)
#
case "${FC}" in
  #
  #  does not work properly
  #
  *ifort*)
    if test -z "$FPP";     then FPP="fpp -free -P"; fi
     ;;
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
AC_MSG_NOTICE([testing FC-preprocessor $FPP])
#
# TESTS
#=======
#
# Fortran Source
#
acx_FC_ok=yes
FPP_TESTS_PASSED=yes
AC_MSG_CHECKING([if FC precompiler works on FC source])
cat > conftest.F << EOF_
 program conftest
 character (1) :: a
 a="a"
 write (*,'('//a//')') 'hello'
 end program
EOF_
# ! Replace "S" with "\" and find the max length of
(eval $FPP conftest.F > conftest.${FCSUFFIX}) 2> conftest.er1
if ! test -s conftest.er1 || test -n "`grep successful conftest.er1`" ||
                             test -n "`grep "warning" conftest.er1`" ||
                             test -n "`grep "command line remark" conftest.er1`" ; then 
 eval $FPP conftest.F > conftest.${FCSUFFIX} 
 eval $FC $FCFLAGS -c conftest.${FCSUFFIX} 2> conftest.er2 >&5
 if test -s conftest.er2 ; then 
  if ! ( test -n "`grep successful conftest.er2`" ||
         test -n "`grep "warning" conftest.er2`" || 
         test -n "`grep "command line remark" conftest.er2`" ) ; then 
   acx_FC_ok=no ; 
   FPP_TESTS_PASSED=no;
  fi
 fi 
else
 acx_FC_ok=no ; 
 FPP_TESTS_PASSED=no
fi 
AC_MSG_RESULT([$acx_FC_ok])
#
if test "x$FPP_TESTS_PASSED" = xno ; then
  AC_MSG_ERROR(Found FC precompiler problems in processing FC source.);
fi
#
AC_SUBST(FPP)
#
])
