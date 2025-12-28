#
# License-Identifier: GPL
#
# Copyright (C) 2006 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
#
AC_DEFUN([AC_SLK_SETUP],[

AC_ARG_ENABLE(par_linalg,   AS_HELP_STRING([--enable-par-linalg],[Use parallel linear algebra. Default is no]))
AC_ARG_WITH(scalapack_libs,[AS_HELP_STRING([--with-scalapack-libs=(libs|mkl)],[Use SCALAPACK libraries <libs> or setup MKL],[32])])
AC_ARG_WITH(elpa_libs,     [AS_HELP_STRING([--with-elpa-libs=(libs)],[Use ELPA libraries <libs>],[32])])
AC_ARG_WITH(elpa_includedir,AS_HELP_STRING([--with-elpa-includedir=<path>],[Path to the elpa include directory],[32]))

SCALAPACK_LIBS=""
ELPA_LIBS=""
ELPA_INCS=""

reset_LIBS="$LIBS"

enable_scalapack="no"
enable_elpa="no"

internal_slk="no"
internal_elpa="no"

compile_slk="no"
compile_elpa="no"

#
# Set fortran linker names of SCALAPACK functions to check for.
#
scalapack_routine="pcheev"
elpa_routine="elpa_init"
mpi_routine=MPI_Init
#
AC_LANG([Fortran])       
testprog_elpa="AC_LANG_PROGRAM([],[
 use elpa
 !class(elpa_t), pointer :: elpa
 integer :: success
 ! We urge the user to always check the error code of all ELPA functions
 if (elpa_init(20200417) /= elpa_ok) then
   print *, 'ELPA API version not supported'
   stop
 endif
])"
#
# Search for MKL-Scalapack
#
try_mkl_scalapack="no"
#
if test -d "${MKLROOT}" ; then
   #
   # Check for MPI libraries
   #
   mkl_libdir="${MKLROOT}/lib/intel64"
   #
   case "${MPIKIND}" in
   *Sgi* | *sgi* | *SGI* )
      lib_mkl_blacs="mkl_blacs_sgimpt_lp64" ;;
   *OpenMPI* | *Open* )
      lib_mkl_blacs="mkl_blacs_openmpi_lp64" ;;
   *)
      lib_mkl_blacs="mkl_blacs_intelmpi_lp64" ;;
   esac
   #
   try_mkl_scalapack="-L${mkl_libdir} -lmkl_scalapack_lp64 -l${lib_mkl_blacs}"
fi
#
# Parse configure options
#
if test "$enable_par_linalg" = "yes" ; then
  enable_scalapack="internal" ;
fi
#
case $with_scalapack_libs in
  yes) enable_scalapack="internal" ;;
  no) enable_scalapack="no" ; enable_par_linalg="no" ;;
  mkl) 
    if test "$try_mkl_scalapack" = "no" ; then
       enable_scalapack="no" ; enable_par_linalg="no" 
    else
       enable_scalapack="check"; SCALAPACK_LIBS="$try_mkl_scalapack" 
    fi
    ;; 
  *) enable_scalapack="check"; SCALAPACK_LIBS="$with_scalapack_libs" ;;
esac
#
case $with_elpa_libs in
  yes) enable_elpa="internal" ;;
  no) enable_elpa="no" ;;
  *) enable_elpa="check"; ELPA_LIBS="$with_elpa_libs" ; ELPA_INCS="$IFLAG$with_elpa_includedir" ;;
esac
#
if test "$mpibuild"  = "yes"; then
  #
  #
  if test "$enable_scalapack" = "check" ; then
    acx_scalapack_save_LIBS="$SCALAPACK_LIBS"
    LIBS="$LIBS $FLIBS $LAPACK_LIBS $BLAS_LIBS"
    # First, check SCALAPACK_LIBS environment variable
    if test "x$SCALAPACK_LIBS" != x; then
      save_LIBS="$LIBS"; LIBS="$SCALAPACK_LIBS $LIBS"
      AC_MSG_CHECKING([for $scalapack_routine in $SCALAPACK_LIBS])
      AC_TRY_LINK_FUNC($scalapack_routine, [enable_scalapack="yes"], [enable_scalapack="internal"; SCALAPACK_LIBS=""])
      AC_MSG_RESULT($enable_scalapack)
      SCALAPACK_LIBS="$acx_scalapack_save_LIBS"
      LIBS="$save_LIBS"
    else
      enable_scalapack="no";
    fi
  fi
  #
  if test "$enable_elpa" = "check" ; then
    acx_elpa_save_LIBS="$ELPA_LIBS"
    LIBS="$LIBS $FLIBS $SCALAPACK_LIBS $LAPACK_LIBS $BLAS_LIBS"
    # First, check ELPA_LIBS environment variable
    if test "x$ELPA_LIBS" != x; then
      save_LIBS="$LIBS"; LIBS="$ELPA_LIBS $LIBS"
      save_FCFLAGS="$FCFLAGS"; FCFLAGS="$ELPA_INCS/modules/ $FCFLAGS"
      #AC_MSG_CHECKING([for $elpa_routine in $ELPA_LIBS])
      #AC_TRY_LINK_FUNC($elpa_routine, [enable_elpa="yes"], [enable_elpa="internal"; ELPA_LIBS=""])
      #AC_MSG_RESULT($enable_elpa)
      AC_MSG_CHECKING([for $elpa_routine in $ELPA_LIBS])
      AC_LINK_IFELSE($testprog_elpa,  [enable_elpa="yes"; elpa_msg="yes"], [enable_elpa="internal"; elpa_msg="not working, fallback to internal"; ELPA_LIBS=""])
      AC_MSG_RESULT($enable_elpa)
      ELPA_LIBS="$acx_elpa_save_LIBS"
      LIBS="$save_LIBS"
      FCFLAGS="$save_FCFLAGS"
    else
      enable_elpa="no";
    fi
  fi
  #
  if test x"$enable_par_linalg" = "xyes"; then
    if test x"$enable_int_linalg" = "xyes"; then
      enable_scalapack="internal";
    else
      if test "$enable_scalapack" = "no"; then enable_scalapack="internal"; fi
    fi
  fi
  #
  #if test x"$enable_par_linalg" = "xyes"; then
  #  if test x"$enable_int_linalg" = "xyes"; then
  #    enable_elpa="internal";
  #  else
  #    if test "$enable_elpa" = "no"; then enable_elpa="internal"; fi
  #  fi
  #fi
  #
  #
  if test "$mpif_found" = "yes" && test "$enable_scalapack" = "internal"; then
    enable_scalapack="yes"
    internal_slk="yes"
    SCALAPACK_LIBS="${extlibs_path}/${FCKIND}/${FC}/lib/libscalapack.a"
    if test -e "${extlibs_path}/${FCKIND}/${FC}/lib/libscalapack.a"; then
      compile_slk="no"
    else
      compile_slk="yes"
    fi
  fi
  #
  if test "$mpif_found" = "yes" && test "$enable_elpa" = "internal"; then
    enable_elpa="yes"
    internal_elpa="yes"
    ELPA_LIBS="${extlibs_path}/${FCKIND}/${FC}/${GPU_SUPPORT}/lib/libelpa.a"
    ELPA_INCS="${IFLAG}${extlibs_path}/${FCKIND}/${FC}/${GPU_SUPPORT}/include/"
    if test -e "${extlibs_path}/${FCKIND}/${FC}/${GPU_SUPPORT}/lib/libelpa.a"; then
      compile_elpa="no"
    else
      compile_elpa="yes"
    fi
  fi
  #
fi
#
if test "$enable_scalapack" = "yes" ; then
  def_scalapack="-D_SCALAPACK"
  if test "$enable_elpa" = "yes" ; then
    def_elpa="-D_ELPA"
  else
    enable_elpa="no"
    def_elpa=""
    ELPA_LIBS=""
    ELPA_INCS=""
    compile_elpa="no"
    internal_elpa="no"
  fi
else
  enable_scalapack="no"
  enable_elpa="no"
  def_scalapack=""
  def_elpa=""
  SCALAPACK_LIBS=""
  ELPA_LIBS=""
  ELPA_INCS=""
  compile_slk="no"
  compile_elpa="no"
  internal_slk="no"
  internal_elpa="no"
fi
#
LIBS="$reset_LIBS"
#
AC_SUBST(SCALAPACK_LIBS)
AC_SUBST(ELPA_LIBS)
AC_SUBST(ELPA_INCS)
AC_SUBST(enable_scalapack)
AC_SUBST(enable_elpa)
AC_SUBST(def_scalapack)
AC_SUBST(def_elpa)
AC_SUBST(compile_slk)
AC_SUBST(internal_slk)
AC_SUBST(compile_elpa)
AC_SUBST(internal_elpa)

])
