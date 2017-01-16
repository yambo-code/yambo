#
# autoconf macro for detecting SLEPC module file
#
AC_DEFUN([SLEPC_SETUP],[

AC_ARG_WITH(slepc_libs,
        [AC_HELP_STRING([--with-slepc-libs=<libs>], [Use SLEPc libraries <libs>],[32])])
AC_ARG_WITH(petsc_libs,
        [AC_HELP_STRING([--with-petsc-libs=<libs>], [Use PETSc libraries <libs>],[32])])
AC_ARG_WITH(slepc_include,
        [AC_HELP_STRING([--with-slepc-include=<incs>], [Use SLEPc includes <incs>],[33])])
AC_ARG_WITH(petsc_include,
        [AC_HELP_STRING([--with-petsc-include=<incs>], [Use PETSc includes <incs>],[32])])

if test -d "$with_petsc_include"; then petsc_include="$with_petsc_include" ; fi
if test -d "$with_slepc_include"; then slepc_include="$with_slepc_include" ; fi

petsc_include="$with_petsc_include"
slepc_include="$with_slepc_include"
slepc_idir="$petsc_include $slepc_include"
#C_AS_CPP_FLAGS="$C_AS_CPP_FLAGS $slepc_idir"

SLEPC_LIBS=""
PETSC_LIBS=""
acx_slepc_ok="no"
acx_petsc_ok="no"
enable_slepc="no"

case $with_petsc_libs in
        yes | "") ;;
        no) acx_petsc_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) PETSC_LIBS="$with_petsc_libs" ;;
        *) PETSC_LIBS="-l$with_petsc_libs" ;;
esac

case $with_slepc_libs in
        yes | "") ;;
        no) acx_slepc_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) SLEPC_LIBS="$with_slepc_libs" ;;
        *) SLEPC_LIBS="-l$with_slepc_libs" ;;
esac

# Set fortran linker names of PETSC/SLEPC functions to check for.
petsc_routine="VecGetArrayF90"
slepc_routine="EPSSolve"

if test "$mpibuild"  = "yes" && ! test "$with_petsc_libs" = "no" && ! test "$with_slepc_libs" = "no"; then
  #
  acx_petsc_save_LIBS="$PETSC_LIBS"
  LIBS="$LIBS $FLIBS $LAPACK_LIBS $BLAS_LIBS"
  # First, check PETSC_LIBS environment variable
  if test "x$PETSC_LIBS" != x; then
          save_LIBS="$LIBS"; LIBS="$PETSC_LIBS $LIBS"
          AC_MSG_CHECKING([for $petsc_routine in $PETSC_LIBS])
          AC_TRY_LINK_FUNC($petsc_routine, [acx_petsc_ok=yes], [PETSC_LIBS=""])
          AC_MSG_RESULT($acx_petsc_ok)
          PETSC_LIBS="$acx_petsc_save_LIBS"
          LIBS="$save_LIBS"
  fi

  acx_slepc_save_LIBS="$SLEPC_LIBS"
  LIBS="$LIBS $FLIBS $LAPACK_LIBS $BLAS_LIBS $PETSC_LIBS"
  # First, check SLEPC_LIBS environment variable
  if test "x$SLEPC_LIBS" != x; then
          save_LIBS="$LIBS"; LIBS="$SLEPC_LIBS $LIBS"
          AC_MSG_CHECKING([for $slepc_routine in $SLEPC_LIBS])
          AC_TRY_LINK_FUNC($slepc_routine, [acx_slepc_ok=yes], [SLEPC_LIBS=""])
          AC_MSG_RESULT($acx_slepc_ok)
          SLEPC_LIBS="$acx_slepc_save_LIBS"
          LIBS="$save_LIBS"
  fi
  #
fi

if test "$acx_petsc_ok" = "yes" &&  test "$acx_slepc_ok" = "yes"; then
  enable_slepc="yes"
  dslepc="-D_SLEPC"
else
  enable_slepc="no"
  dslepc=""
  PETSC_LIBS=""
  SLEPC_LIBS=""
fi

AC_SUBST(C_AS_CPP_FLAGS)
AC_SUBST(PETSC_LIBS)
AC_SUBST(SLEPC_LIBS)
AC_SUBST(slepc_idir)
AC_SUBST(enable_slepc)
AC_SUBST(dslepc)

])
