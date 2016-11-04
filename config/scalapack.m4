#
# autoconf macro for detecting NetCDF module file
# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
AC_DEFUN([SLK_SETUP],[

AC_ARG_WITH(blacs_libs,
        [AC_HELP_STRING([--with-blacs-libs=<libs>], [Use BLACS libraries <libs> or leave empty to use internal lib],[32])])
AC_ARG_WITH(scalapack_libs,
        [AC_HELP_STRING([--with-scalapack-libs=<libs>], [Use SCALAPACK libraries <libs> or leave empty to use internal lib],[32])])

SCALAPACK_LIBS=""
BLACS_LIBS=""
acx_blacs_ok="no"
acx_scalapack_ok="no"
enable_scalapack="no"

case $with_blacs_libs in
        yes | "") ;;
        no) acx_blacs_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) BLACS_LIBS="$with_blacs_libs" ;;
        *) BLACS_LIBS="-l$with_blacs_libs" ;;
esac

case $with_scalapack_libs in
        yes | "") ;;
        no) acx_scalapack_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) SCALAPACK_LIBS="$with_scalapack_libs" ;;
        *) SCALAPACK_LIBS="-l$with_scalapack_libs" ;;
esac

# Set fortran linker names of BLACS/SCALAPACK functions to check for.
blacs_routine="blacs_set"
scalapack_routine="pcheev"

if test "$mpibuild"  = "yes" && ! test "$with_blacs_libs" = "no" && ! test "$with_scalapack_libs" = "no"; then
  #
  acx_blacs_save_LIBS="$BLACS_LIBS"
  LIBS="$LIBS $FLIBS $LAPACK_LIBS $BLAS_LIBS"
  # First, check BLACS_LIBS environment variable
  if test "x$BLACS_LIBS" != x; then
          save_LIBS="$LIBS"; LIBS="$BLACS_LIBS $LIBS"
          AC_MSG_CHECKING([for $blacs_routine in $BLACS_LIBS])
          AC_TRY_LINK_FUNC($blacs_routine, [acx_blacs_ok=yes], [BLACS_LIBS=""])
          AC_MSG_RESULT($acx_blacs_ok)
          BLACS_LIBS="$acx_blacs_save_LIBS"
          LIBS="$save_LIBS"
  fi

  acx_scalapack_save_LIBS="$SCALAPACK_LIBS"
  LIBS="$LIBS $FLIBS $LAPACK_LIBS $BLAS_LIBS $BLACS_LIBS"
  # First, check SCALAPACK_LIBS environment variable
  if test "x$SCALAPACK_LIBS" != x; then
          save_LIBS="$LIBS"; LIBS="$SCALAPACK_LIBS $LIBS"
          AC_MSG_CHECKING([for $scalapack_routine in $SCALAPACK_LIBS])
          AC_TRY_LINK_FUNC($scalapack_routine, [acx_scalapack_ok=yes], [SCALAPACK_LIBS=""])
          AC_MSG_RESULT($acx_scalapack_ok)
          SCALAPACK_LIBS="$acx_scalapack_save_LIBS"
          LIBS="$save_LIBS"
  fi
  #
fi
#
if test "$acx_blacs_ok" = "yes" &&  test "$acx_scalapack_ok" = "yes"; then
  enable_scalapack="yes"
  dscalapack="-D_SCALAPACK"
else
  enable_scalapack="no"
  dscalapack=""
  BLACS_LIBS=""
  SCALAPACK_LIBS=""
fi
#
compile_blacs="no"
if test "$mpibuild"  = "yes" && test "$with_blacs_libs" = "yes"; then
compile_blacs="yes"
BLACS_LIBS="-lblacs_init -lblacs"
fi
compile_slk="no"
if test "$mpibuild"  = "yes" && test "$with_scalapack_libs" = "yes"; then
compile_slk="yes"
SCALAPACK_LIBS="-lscalapack"
enable_scalapack="yes"
dscalapack="-D_SCALAPACK"

fi
#
AC_SUBST(BLACS_LIBS)
AC_SUBST(SCALAPACK_LIBS)
AC_SUBST(enable_scalapack)
AC_SUBST(dscalapack)
AC_SUBST(compile_slk)
AC_SUBST(compile_blacs)

])
