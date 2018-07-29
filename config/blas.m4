#
# Available from the GNU Autoconf Macro Archive at:
# http://autoconf-archive.cryp.to/macros-by-category.html
#
AC_DEFUN([ACX_BLAS], [
AC_PREREQ(2.50)
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
acx_blas_ok=no

AC_ARG_WITH(blas_libs,[AC_HELP_STRING([--with-blas-libs=<libs>], [Use BLAS libraries <libs>],[32])])
AC_ARG_ENABLE(int_linalg,   AC_HELP_STRING([--enable-int-linalg],[Force internal linear algebra. Default is no]))
AC_ARG_ENABLE(openmp_int_linalg,   AC_HELP_STRING([--enable-openmp-int-linalg],[Use openmp internal linear algebra for few selected operations. Default is no]))

BLAS_LIBS=""
AC_ARG_WITH(blas_libs,
        [AC_HELP_STRING([--with-blas-libs=<libs>], [Use BLAS libraries <libs>],[32])])
case $with_blas_libs in
        yes | "") ;;
        no) acx_blas_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) BLAS_LIBS="$with_blas_libs" ;;
        *) BLAS_LIBS="-l$with_blas_libs" ;;
esac

# Set fortran linker names of BLAS functions to check for.
caxpy="caxpy"
daxpy="daxpy"

acx_blas_save_LIBS="$LIBS"
LIBS="$LIBS $FLIBS"

compile_blas="no"
internal_blas="no"

# First, check BLAS_LIBS environment variable
if test $acx_blas_ok = no; then
if test "x$BLAS_LIBS" != x; then
        save_LIBS="$LIBS"; LIBS="$BLAS_LIBS $LIBS"
        AC_MSG_CHECKING([for $caxpy in $BLAS_LIBS])
        AC_TRY_LINK_FUNC($caxpy, [acx_blas_ok=yes], [BLAS_LIBS=""])
        AC_MSG_RESULT($acx_blas_ok)
        LIBS="$save_LIBS"
fi
fi


# BLAS linked to by default?  (happens on some supercomputers)
if test $acx_blas_ok = no; then
        save_LIBS="$LIBS"; LIBS="$LIBS"
        AC_CHECK_FUNC($caxpy, [acx_blas_ok=yes])
        LIBS="$save_LIBS"
fi

# BLAS in ATLAS library? (http://math-atlas.sourceforge.net/)
if test $acx_blas_ok = no; then
        AC_CHECK_LIB(atlas, ATL_xerbla,
                [AC_CHECK_LIB(f77blas, $caxpy,
                [AC_CHECK_LIB(cblas, cblas_daxpy,
                        [acx_blas_ok=yes
                         BLAS_LIBS="-lcblas -lf77blas -latlas"],
                        [], [-lf77blas -latlas])],
                        [], [-latlas])])
fi

# BLAS in PhiPACK libraries? (requires generic BLAS lib, too)
if test $acx_blas_ok = no; then
        AC_CHECK_LIB(blas, $caxpy,
                [AC_CHECK_LIB(daxpy, $daxpy,
                [AC_CHECK_LIB(caxpy, $caxpy,
                        [acx_blas_ok=yes; BLAS_LIBS="-lcaxpy -ldaxpy -lblas"],
                        [], [-lblas])],
                        [], [-lblas])])
fi

# BLAS in Alpha CXML library?
if test $acx_blas_ok = no; then
        AC_CHECK_LIB(cxml, $caxpy, [acx_blas_ok=yes;BLAS_LIBS="-lcxml"])
fi

# BLAS in Alpha DXML library? (now called CXML, see above)
if test $acx_blas_ok = no; then
        AC_CHECK_LIB(dxml, $caxpy, [acx_blas_ok=yes;BLAS_LIBS="-ldxml"])
fi

# BLAS in Sun Performance library?
if test $acx_blas_ok = no; then
        if test "x$GCC" != xyes; then # only works with Sun CC
                AC_CHECK_LIB(sunmath, acosp,
                        [AC_CHECK_LIB(sunperf, $caxpy,
                                [BLAS_LIBS="-xlic_lib=sunperf -lsunmath"
                                 acx_blas_ok=yes],[],[-lsunmath])])
        fi
fi

# BLAS in SCSL library?  (SGI/Cray Scientific Library)
if test $acx_blas_ok = no; then
        AC_CHECK_LIB(scs, $caxpy, [acx_blas_ok=yes; BLAS_LIBS="-lscs"])
fi

# BLAS in SGIMATH library?
if test $acx_blas_ok = no; then
        AC_CHECK_LIB(complib.sgimath, $caxpy,
                     [acx_blas_ok=yes; BLAS_LIBS="-lcomplib.sgimath"])
fi

# BLAS in IBM ESSL library? (requires generic BLAS lib, too)
if test $acx_blas_ok = no; then
        AC_CHECK_LIB(blas, $caxpy,
                [AC_CHECK_LIB(essl, $caxpy,
                        [acx_blas_ok=yes; BLAS_LIBS="-lessl -lblas"],
                        [], [-lblas $FLIBS])])
fi

# Generic BLAS library?
if test $acx_blas_ok = no; then
        AC_CHECK_LIB(blas, $caxpy, [acx_blas_ok=yes; BLAS_LIBS="-lblas"])
fi

LIBS="$acx_blas_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_blas_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_BLAS,1,[Define if you have a BLAS library.]),[$1])
        :
else
        acx_blas_ok=no
        $2
fi

if test $acx_blas_ok = "no"; then
  internal_blas="yes";
  AC_MSG_NOTICE([Could not find blas. Using the built-in library])
elif (test -d "$with_blas_libs" && test "$with_blas_libs" = "") || test x"$enable_int_linalg" = "xyes" ; then
  internal_blas="yes"
  if test $acx_blas_ok = "yes"; then AC_MSG_NOTICE([Blas found in ${BLAS_LIBS} but imposing built-in library]); fi
fi
  
if test "$internal_blas" = "yes"; then
  BLAS_LIBS="${extlibs_path}/${FCKIND}/${FC}/lib/libblas.a";
  if test -e ${extlibs_path}/${FCKIND}/${FC}/lib/libblas.a; then
    compile_blas="no";
  else
    compile_blas="yes";
  fi
fi

BLAS_info=""
def_openmp_int_linalg=""
if test x"$enable_openmp_int_linalg" = "xyes" ; then
  if test x"$enable_openmp_int_linalg" = "xyes" ; then
    BLAS_info="(Blas with some operations internally OpenMP distributed)"
    def_openmp_int_linalg="-D_OPENMP_INT_LINALG"
  else
    enable_openmp_int_linalg="no"
  fi
fi

AC_SUBST(internal_blas)
AC_SUBST(enable_int_linalg)
AC_SUBST(enable_openmp_int_linalg)
AC_SUBST(def_openmp_int_linalg)
AC_SUBST(compile_blas)
AC_SUBST(BLAS_LIBS)
AC_SUBST(BLAS_info)

])dnl ACX_BLAS

