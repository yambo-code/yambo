#
# autoconf macro for detecting NetCDF module file
# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
AC_DEFUN([SLK_SETUP],[
AC_ARG_WITH(blacs, AC_HELP_STRING([--with-blacs=<path>],
 [Path of the BLACS library directory]),
 [], [with_blacs="/usr/local/lib"])
AC_ARG_WITH(scalapack, AC_HELP_STRING([--with-scalapack=<path>],
 [Path of the SCALAPACK library directory]),
 [], [with_scalapack="$with_blacs"])

SCALAPACK_LIBS=""
BLACS_LIBS=""
enable_scalapack="no"

if test "$mpibuild"  = "yes" && ! test "$with_blacs" = "no" && 
        ! test "$with_scalapack" = "no"; then
#
# BLACS first
#
 AC_MSG_CHECKING([the BLACS library])
 cinit_lib=`find $with_blacs -name '*blacsCinit*.a'`
 blacs_lib=`find $with_blacs -name '*blacs.a'`
 if ! test "$cinit_lib" = "" && ! test "$blacs_lib" = "" ; then
   BLACS_LIBS="$cinit_lib $blacs_lib"
   AC_MSG_RESULT(yes)
 else
   AC_MSG_RESULT(no)
 fi
 if ! test "$BLACS_LIBS" = ""; then
#
# then SLPK if BLACS has been found
#
  AC_MSG_CHECKING([the SCALAPACK library])
  SCALAPACK_LIBS=`find $with_scalapack -name '*scalapack.a'`
  if ! test "$SCALAPACK_LIBS" = "" ; then
   enable_scalapack="yes"
   AC_MSG_RESULT(yes)
  else
   BLACS_LIBS=""
   enable_scalapack="no"
   AC_MSG_RESULT(no)
  fi
 fi
fi

dscalapack=""
if test "$enable_scalapack" = "yes" ; then dscalapack="-D_SCALAPACK";fi

AC_SUBST(BLACS_LIBS)
AC_SUBST(SCALAPACK_LIBS)
AC_SUBST(enable_scalapack)
AC_SUBST(dscalapack)

])
