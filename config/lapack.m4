#
# Available from the GNU Autoconf Macro Archive at:
# http://autoconf-archive.cryp.to/macros-by-category.html
#
AC_DEFUN([ACX_LAPACK], [
AC_REQUIRE([ACX_BLAS])
acx_lapack_ok=no

LAPACK_LIBS=""
AC_ARG_WITH(lapack_libs,
        [AC_HELP_STRING([--with-lapack-libs=<libs>], [Use LAPACK libraries <libs>],[32])])
case $with_lapack_libs in
        yes | "") ;;
        no) acx_lapack_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) LAPACK_LIBS="$with_lapack_libs" ;;
        *) LAPACK_LIBS="-l$with_lapack_libs" ;;
esac


internal_lapack="no"
compile_lapack="no"

# Set fortran linker name of LAPACK function to check for.
cheev="cheev"

# We cannot use LAPACK if BLAS is not found
if test "x$acx_blas_ok" != xyes; then
        acx_lapack_ok=noblas
fi

# First, check LAPACK_LIBS environment variable
if test "x$LAPACK_LIBS" != x; then
        save_LIBS="$LIBS"; LIBS="$LAPACK_LIBS $BLAS_LIBS $LIBS $FLIBS"
        AC_MSG_CHECKING([for $cheev in $LAPACK_LIBS])
        AC_TRY_LINK_FUNC($cheev, [acx_lapack_ok=yes], [LAPACK_LIBS=""])
        AC_MSG_RESULT($acx_lapack_ok)
        LIBS="$save_LIBS"
        if test acx_lapack_ok = no; then
                LAPACK_LIBS=""
        fi
fi

# LAPACK linked to by default?  (is sometimes included in BLAS lib)
if test $acx_lapack_ok = no; then
        save_LIBS="$LIBS"; LIBS="$LIBS $BLAS_LIBS $FLIBS"
        AC_CHECK_FUNC($cheev, [acx_lapack_ok=yes])
        LIBS="$save_LIBS"
fi

# Generic LAPACK library?
for lapack in lapack lapack_rs6k; do
        if test $acx_lapack_ok = no; then
                save_LIBS="$LIBS"; LIBS="$BLAS_LIBS $LIBS"
                AC_CHECK_LIB($lapack, $cheev,
                    [acx_lapack_ok=yes; LAPACK_LIBS="-l$lapack"], [], [$FLIBS])
                LIBS="$save_LIBS"
        fi
done

# Finally, check for dlaran routine in lapack. This routine
# sometimes is not included in the package and must be directly compiled
# and linked
if test x"$acx_lapack_ok" = xyes; then
 AC_F77_FUNC(dlaran)
 if test "x$LAPACK_LIBS" != x; then
        save_LIBS="$LIBS"; LIBS="$LAPACK_LIBS"
        AC_MSG_CHECKING([for $dlaran in $LAPACK_LIBS])
        AC_TRY_LINK_FUNC($dlaran, [acx_dlaran_ok=yes], [acx_dlaran_ok=no])
        AC_MSG_RESULT($acx_dlaran_ok)
        LIBS="$save_LIBS"
 fi
fi

# Execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_lapack_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_LAPACK,1,[Define if you have LAPACK library.]),[$1])
        :
else
        acx_lapack_ok=no
        $2
fi

if test $acx_lapack_ok = "no"; then
  internal_lapack="yes";
  AC_MSG_NOTICE([Could not find lapack. Using the built-in library])
elif (test -d  "$with_lapack_libs" && test "$with_lapack_libs" = "") || test x"$enable_int_linalg" = "xyes" ; then
  internal_lapack="yes"
  if test "$acx_lalpack_ok" = "yes"; then AC_MSG_NOTICE([Lapack found in ${LAPACK_LIBS} but imposing built-in library]); fi
fi
  
if test "$internal_lapack" = "yes"; then
  LAPACK_LIBS="-L${extlibs_path}/${FCKIND}/${FC}/lib -llapack";
  if test -e ${extlibs_path}/${FCKIND}/${FC}/lib/liblapack.a; then
    compile_lapack="no";
  else
    compile_lapack="yes";
  fi
fi

AC_SUBST(internal_lapack)
AC_SUBST(compile_lapack)
AC_SUBST(LAPACK_LIBS)

])dnl ACX_LAPACK

