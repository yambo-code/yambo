#
# autoconf macro for detecting NetCDF module file
# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
AC_DEFUN([SLK_SETUP],[
AC_ARG_WITH(blacs, AC_HELP_STRING([--with-blacs=<lib>],
 [Path of the BLACS library directory]),
 [], [with_blacs="/usr/local/lib"])
AC_ARG_WITH(scalapack, AC_HELP_STRING([--with-scalapack=<lib>],
 [Path of the SCALAPACK library directory]),
 [], [with_scalapack="$with_blacs"])
SCALAPACK_LIBS=""
BLACS_LIBS=""
enable_scalapack="no"
if test "$mpibuild"  = "yes" && ! test "$with_blacs" = "no" && 
        ! test "$with_scalapack" = "no"; then
 AC_MSG_CHECKING([the BLACS library])
 if ! test "$with_blacs" = "" ; then
  BLACS_LIBS=`find $with_blacs -name 'blacsCinit*.a'`
  save_blib=$BLACS_LIBS
  BLACS_LIBS="$save_blib $with_blacs/libblacs.a"
  AC_MSG_RESULT(yes)
  AC_MSG_CHECKING([the SCALAPACK library])
  if ! test "$with_scalapack" = ""; then
   SCALAPACK_LIBS="$with_scalapack/libscalapack.a"
   enable_scalapack="yes"
  fi
  AC_MSG_RESULT($enable_scalapack)
 else
  AC_MSG_RESULT(no)
 fi
fi
dscalapack=""
if test "$enable_scalapack" = "yes" ; then dscalapack="-DSLK";fi
AC_SUBST(BLACS_LIBS)
AC_SUBST(SCALAPACK_LIBS)
AC_SUBST(enable_scalapack)
AC_SUBST(dscalapack)
])
