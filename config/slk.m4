#
# autoconf macro for detecting NetCDF module file
# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
AC_DEFUN([SLK_SETUP],[

AC_ARG_WITH(blacs,
        [AC_HELP_STRING([--with-blacs=<lib>], [Use BLACS library <lib>])])
AC_ARG_WITH(scalapack,
        [AC_HELP_STRING([--with-scalapack=<lib>], [Use SCALAPACK library <lib>])])

SCALAPACK_LIBS=""
BLACS_LIBS=""
acx_blacs_ok="no"
acx_scalapack_ok="no"
enable_scalapack="no"

case $with_blacs in
        yes | "") ;;
        no) acx_blacs_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) BLACS_LIBS="$with_blacs" ;;
        *) BLACS_LIBS="-l$with_blacs" ;;
esac

case $with_scalapack in
        yes | "") ;;
        no) acx_scalapack_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) SCALAPACK_LIBS="$with_scalapack" ;;
        *) SCALAPACK_LIBS="-l$with_scalapack" ;;
esac

# Set fortran linker names of BLACS/SCALAPACK functions to check for.
blacs_routine="blacs_set"
scalapack_routine="pcheev"

if test "$mpibuild"  = "yes" && ! test "$with_blacs" = "no" && ! test "$with_scalapack" = "no"; then

acx_blacs_save_LIBS="$BLACS_LIBS"
LIBS="$LIBS $FLIBS"
# First, check BLACS_LIBS environment variable
if test "x$BLACS_LIBS" != x; then
        save_LIBS="$LIBS"; LIBS="$BLACS_LIBS $LIBS"
        AC_MSG_CHECKING([for $blacs_routine in $BLACS_LIBS])
        AC_TRY_LINK_FUNC($blacs_routine, [acx_blacs_ok=yes], [BLACS_LIBS=""])
        AC_MSG_RESULT($acx_blacs_ok)
        BLACS_LIBS="$acx_blacs_save_LIBS"
fi

acx_scalapack_save_LIBS="$SCALAPACK_LIBS"
LIBS="$LIBS $FLIBS"
# First, check SCALAPACK_LIBS environment variable
if test "x$SCALAPACK_LIBS" != x; then
        save_LIBS="$LIBS"; LIBS="$SCALAPACK_LIBS $LIBS"
        AC_MSG_CHECKING([for $scalapack_routine in $SCALAPACK_LIBS])
        AC_TRY_LINK_FUNC($scalapack_routine, [acx_scalapack_ok=yes], [SCALAPACK_LIBS=""])
        AC_MSG_RESULT($acx_scalapack_ok)
        SCALAPACK_LIBS="$acx_scalapack_save_LIBS"
fi

fi

if test "$acx_blacs_ok" = "yes" ; then
 if test "$acx_scalapack_ok" = "yes" ; then
  enable_scalapack="yes"  
 fi
fi

dscalapack=""
if test "$enable_scalapack" = "yes" ; then dscalapack="-D_SCALAPACK";fi

AC_SUBST(BLACS_LIBS)
AC_SUBST(SCALAPACK_LIBS)
AC_SUBST(enable_scalapack)
AC_SUBST(dscalapack)

])
