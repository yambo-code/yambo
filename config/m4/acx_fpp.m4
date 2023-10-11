#
# License-Identifier: GPL
#
# Copyright (C) 2008 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
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
#
(eval $FPP conftest.F > conftest.${FCSUFFIX}) 2> conftest.er1
if ! test -s conftest.er1 || test -n "`grep successful conftest.er1`" ||
                             test -n "`grep -i "warning" conftest.er1`" ||
                             test -n "`grep "command line remark" conftest.er1`" ; then 
 eval $FPP conftest.F > conftest.${FCSUFFIX} 
 eval $FC $FCFLAGS -c conftest.${FCSUFFIX} 2> conftest.er2 >&5
 if test -s conftest.er2 ; then 
  if ! ( test -n "`grep successful conftest.er2`" ||
         test -n "`grep -i "warning" conftest.er2`" ||
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
