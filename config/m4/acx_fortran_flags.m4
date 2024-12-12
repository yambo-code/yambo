#
# License-Identifier: GPL
#
# Copyright (C) 2010 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
# #
################################################
# Set FC FLAGS
# ----------------------------------
AC_DEFUN([ACX_FCSETUP],
[
AC_REQUIRE([AC_CANONICAL_HOST])
AC_ARG_VAR(UFLAGS,[Unoptimized Fortran flags])
#
if test -z "${CFLAGS}"; then CFLAGS="-O2"; fi
#
AC_ARG_ENABLE(debug-flags, AS_HELP_STRING([--enable-debug-flags],[Debug flags are set for compilation. Default is no.]))
if test x"$enable_debug_flags" = "x"; then enable_debug_flags="no"; fi
#
HDF5_MODE="production";
#
def_compiler=
SLK_FC_FLAGS=""
#
case "${host}" in
i?86*linux*)
  case "${FC}" in
  *ftn* )
    SYSFLAGS="-O1 -g -emf -eZ"
    FUFLAGS="-O0 -g -emf  -eZ"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    DEBUG_FLAGS="-g "
  ;;
  *pgf9* | *ftn* | *pgfortran* )
    SYSFLAGS="-O1 -gopt -Mnoframe -Mdalign -Mbackslash -cpp"
    FUFLAGS="-O0 -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    NETCDFFLAGS="-DpgiFortran"
    def_compiler="-D_PGI"
    DEBUG_FLAGS="-g -Minform=inform -Mbounds -Mchkptr -Mchkstk -Meh_frame -Mbackslash"
    ;;
  *nvfortran* )
    SYSFLAGS="-O1 -gopt -Mnoframe -Mdalign -Mbackslash -cpp"
    FUFLAGS="-O0 -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    NETCDFFLAGS="-DpgiFortran"
    def_compiler="-D_PGI"
    DEBUG_FLAGS="-g -Minform=inform -Mbounds -Mchkptr -Mchkstk -Meh_frame -Mbackslash"
    ;;
  *abf90*)
    SYSFLAGS="-B101 -YEXT_NAMES=LCS -YEXT_SFX=_"
    ;;
  *ifc*)
    SYSFLAGS="-O3 -g -w -tpp7"
    FUFLAGS="-O0 -w -tpp7"
    FCMFLAG=""
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    DEBUG_FLAGS=""
    ;;
  *g95*)
    SYSFLAGS="-O3 -g -fbackslash -fno-second-underscore -mtune=pentium4"
    FUFLAGS="-g -O0 -fbackslash -fno-second-underscore"
    FCMFLAG=""
    OMPFLAGS=""
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check -ftrace=full"
    ;;
  *gfortran*)
    SYSFLAGS="-O3 -g -mtune=native -fno-lto"
    FUFLAGS="-O0 -mtune=native"
    SLK_FC_FLAGS="-fallow-argument-mismatch"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    NETCDFFLAGS="-DgFortran"
    DEBUG_FLAGS="-Og -g -Wall -pedantic -fbounds-check -ffpe-trap=invalid,zero,overflow"
    ;;
  *ifort* | *ifx* )
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    CPU_FLAG=""
    FCMFLAG="-nofor_main"
    case "${INTELVERSION}" in
      *11* | *12* | *13* |*14* | *15* | *16* )
       CPU_FLAG="-xHost"
       #CPU_FLAG=" "
       ;;
      *2021* | *2022* | *2023* | *2024* | *2025* )
       CPU_FLAG=" "
       OMPFLAGS="-qopenmp -parallel"
       FCMFLAG="-nofor-main"
       ;;
      *17* | *18* | *19* )
       CPU_FLAG=" "
       OMPFLAGS="-qopenmp"
       ;;
      *10*)
       CPU_FLAG="-xW"
       ;;
      *)
       CPU_FLAG=" "
       ;;
    esac
    SYSFLAGS="-assume bscc -O2 -g $CPU_FLAG"
    FUFLAGS="-assume bscc -O0 $CPU_FLAG"
    DEBUG_FLAGS="-check all -CB -traceback -check bound"
  ;;
  *pathf9*)
    SYSFLAGS="-O2 -g -fno-second-underscore"
    FUFLAGS="-O0 -g -fno-second-underscore"
    FCMFLAG=""
    OMPFLAGS=""
    DEBUG_FLAGS="-ffortran-bounds-check -C"
    ;;
  *)
    SYSFLAGS="-O -g"
    FUFLAGS="-O0 -g"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-Df2cFortran"
  esac
 ;;
*86*apple* | aarch*apple* | arm*apple* )
  case "${FC}" in
  *pgf9* | *ftn* | *pgfortran* )
    SYSFLAGS="-O1 -gopt -Mnoframe -Mdalign -Mbackslash -cpp"
    FUFLAGS="-O0 -g -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    NETCDFFLAGS="-DpgiFortran"
    DEBUG_FLAGS="-g -Minform=inform -Mbounds -Mchkptr -Mchkstk -Meh_frame"
    ;;
  *nvfortran* )
    SYSFLAGS="-O1 -gopt -Mnoframe -Mdalign -Mbackslash -cpp"
    FUFLAGS="-O0 -g -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    NETCDFFLAGS="-DpgiFortran"
    def_compiler="-D_PGI"
    DEBUG_FLAGS="-g -Minform=inform -Mbounds -Mchkptr -Mchkstk -Meh_frame"
    ;;
  *gfortran*)
    SYSFLAGS="-O3 -g -mtune=native"
    FUFLAGS="-O0 -g -mtune=native"
    SLK_FC_FLAGS="-fallow-argument-mismatch"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    NETCDFFLAGS="-DgFortran"
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check -ffpe-trap=invalid,zero,overflow"
    ;;
  *g95*)
    SYSFLAGS="-O3 -g -fno-second-underscore -mtune=pentium4"
    FUFLAGS="-O0 -g -fno-second-underscore"
    FCMFLAG=""
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check -ftrace=full"
    ;;
  *ifort*)
    CPU_FLAG=""
    case "${INTELVERSION}" in
      *1*)
       CPU_FLAG="-mtune=pentium4"
       ;;
      *)
       CPU_FLAG="-mtune=pentium4"
       ;;
    esac
    SYSFLAGS="-assume bscc -O3 -g ${CPU_FLAG}"
    FUFLAGS="-assume bscc -O0 -g ${CPU_FLAG}"
    FCMFLAG="-nofor_main"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    DEBUG_FLAGS="-check all -CB -traceback -check bound"
    ;;
  *)
    SYSFLAGS="-O -g"
    FUFLAGS="-O -g0"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-Df2cFortran"
  esac
  ;;
ia64*linux* )
  case "${FC}" in
  *ftn* )
    SYSFLAGS="-O1 -g -emf -eZ"
    FUFLAGS="-O0 -g -emf  -eZ"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    DEBUG_FLAGS="-g "
  ;;
  *pgf9* | *ftn* | *pgfortran* )
    SYSFLAGS="-O1 -gopt -Mnoframe -Mdalign -Mbackslash -cpp"
    #SYSFLAGS="-O2 -g -fast -Munroll -Mnoframe -Mdalign -Mbackslash"
    FUFLAGS="-O0 -g -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    NETCDFFLAGS="-DpgiFortran"
    def_compiler="-D_PGI"
    DEBUG_FLAGS="-g -Minform=inform -Mbounds -Mchkptr -Mchkstk -Meh_frame"
    ;;
  *nvfortran* )
    SYSFLAGS="-O1 -gopt -Mnoframe -Mdalign -Mbackslash -cpp"
    FUFLAGS="-O0 -g -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    NETCDFFLAGS="-DpgiFortran"
    def_compiler="-D_PGI"
    DEBUG_FLAGS="-g -Minform=inform -Mbounds -Mchkptr -Mchkstk -Meh_frame"
    ;;
  *gfortran*)
    SYSFLAGS="-O3 -g -mtune=native -fno-lto"
    FUFLAGS="-O0 -g -mtune=native"
    SLK_FC_FLAGS="-fallow-argument-mismatch"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    NETCDFFLAGS="-DgFortran"
    DEBUG_FLAGS="-Og -g -Wall -pedantic -fbounds-check -ffpe-trap=invalid,zero,overflow"
    ;;
  *g95*)
    SYSFLAGS="-O3 -g -fbackslash -fno-second-underscore"
    FUFLAGS="-O0 -g -fbackslash -fno-second-underscore"
    FCMFLAG=""
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check -ftrace=full"
    ;;
  *abf90*)
    SYSFLAGS="-B101 -YEXT_NAMES=LCS -YEXT_SFX=_"
    ;;
  *ifc*)
    SYSFLAGS="-O3 -g -w"
    FUFLAGS="-O0 -g -w"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    DEBUG_FLAGS=""
    ;;
  *ifort*)
    CPU_FLAG=""
    case "${INTELVERSION}" in
      *1*)
       CPU_FLAG="-mtune=itanium"
       ;;
      *)
       CPU_FLAG=""
       ;;
    esac
    SYSFLAGS="-assume bscc -O2 -g ${CPU_FLAG}"
    FUFLAGS="-assume bscc -O0 -g ${CPU_FLAG}"
    FCMFLAG="-nofor_main"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    DEBUG_FLAGS="-check all -CB -traceback -check bound"
    ;;
  *openf9*)
    SYSFLAGS="-O2 -fno-second-underscore"
    FUFLAGS="-O0 -fno-second-underscore"
    FCMFLAG=""
    ;;
  *pathf9*)
    SYSFLAGS="-O2 -fno-second-underscore"
    FUFLAGS="-O0 -fno-second-underscore"
    FCMFLAG=""
    OMPFLAGS=""
    DEBUG_FLAGS="-ffortran-bounds-check -C"
    ;;
  *)
    SYSFLAGS="-g -O"
    FUFLAGS="-O0"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-Df2cFortran"
  esac
  ;;
aarch*linux* | arm*linux* )
  case "${FC}" in
  *gfortran*)
    SYSFLAGS="-O3 -g -mtune=native -fno-lto"
    FUFLAGS="-O0 -g -mtune=native"
    SLK_FC_FLAGS="-fallow-argument-mismatch"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    NETCDFFLAGS="-DgFortran"
    DEBUG_FLAGS="-Og -g -Wall -pedantic -fbounds-check -ffpe-trap=invalid,zero,overflow"
    ;;
  *)
    SYSFLAGS="-g -O"
    FUFLAGS="-O0"
    NETCDFFLAGS="-Df2cFortran"
  esac
  ;;
*x86*64* )
  case "${FC}" in
  *ftn* )
    SYSFLAGS="-O1 -g -emf -eZ"
    FUFLAGS="-O0 -g -emf  -eZ"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    DEBUG_FLAGS="-g "
  ;;
  *pgf9* | *ftn* | *pgfortran* | *nvfortran* )
    SYSFLAGS="-O1 -gopt -Mnoframe -Mdalign -Mbackslash -cpp"
    #SYSFLAGS="-O2 -g -Munroll -Mnoframe -Mdalign -Mbackslash -cpp"
    FUFLAGS="-O0 -g -Mbackslash -cpp"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    def_compiler="-D_PGI"
    NETCDFFLAGS="-DpgiFortran"
    DEBUG_FLAGS="-g -Minform=inform -Mbounds -Mchkptr -Mchkstk -Meh_frame  -Mbackslash -cpp"
    ;;
  *gfortran*)
    SYSFLAGS="-O3 -g -mtune=native -fno-lto"
    FUFLAGS="-O0 -g -mtune=native"
    SLK_FC_FLAGS="-fallow-argument-mismatch"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    NETCDFFLAGS="-DgFortran"
    DEBUG_FLAGS="-Og -g -Wall -pedantic -fbounds-check -ffpe-trap=invalid,zero,overflow"
    ;;
  *g95*)
    SYSFLAGS="-O3 -g -fbackslash -fno-second-underscore"
    FUFLAGS="-O0 -g -fbackslash -fno-second-underscore"
    FCMFLAG=""
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check -ftrace=full"
    ;;
  *abf90*)
    SYSFLAGS="-B101 -YEXT_NAMES=LCS -YEXT_SFX=_"
    ;;
  *ifc*)
    SYSFLAGS="-O3 -g -w -tpp2"
    FUFLAGS="-O0 -g -w -tpp2"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    ;;
  *ifort* | *ifx* )
    OMPFLAGS="-openmp"
    CPU_FLAG=""
    FCMFLAG="-nofor_main"
    case "${INTELVERSION}" in
      *11* | *12* | *13* |*14* |*15* | *16* )
       #CPU_FLAG="-xHost"
       CPU_FLAG=" "
       ;;
      *2020* | *2021* | *2022* | *2023* | *2024* | *2025* )
       CPU_FLAG=" "
       OMPFLAGS="-qopenmp -parallel"
       FCMFLAG="-nofor-main"
       CFLAGS="-O2 -std=gnu99"
       ;;
      *19* )
       CPU_FLAG=" "
       OMPFLAGS="-qopenmp"
       CFLAGS="-O2 -std=gnu99 -no-multibyte-chars"
       ;;
      *17* | *18* )
       CPU_FLAG=" "
       OMPFLAGS="-qopenmp"
       CFLAGS="-O2 -std=gnu99"
       ;;
      *10*)
       CPU_FLAG="-xW"
       ;;
      *)
       CPU_FLAG=" "
       ;;
    esac
    SYSFLAGS="-assume bscc -O2 -g ${CPU_FLAG}"
    FUFLAGS="-assume bscc -O0 -g ${CPU_FLAG}"
    NETCDFFLAGS="-DpgiFortran"
    DEBUG_FLAGS="-CB -traceback -debug full"
    ;;
  *ifx*)
    OMPFLAGS="-qopenmp"
    CPU_FLAG=""
    FCMFLAG="-nofor-main"
    CFLAGS="-O2 -std=gnu99"
    SYSFLAGS="-O3 -g"
    FUFLAGS="-O0 -g"
    DEBUG_FLAGS="-CB -traceback -debug full"
    ;;
  *openf9*)
    SYSFLAGS="-O2 -fno-second-underscore"
    FUFLAGS="-O0 -fno-second-underscore"
    FCMFLAG=""
    ;;
  *pathf9*)
    SYSFLAGS="-O2 -fno-second-underscore"
    FUFLAGS="-O0 -fno-second-underscore"
    FCMFLAG=""
    OMPFLAGS=""
    DEBUG_FLAGS="-ffortran-bounds-check -C"
    ;;
  *)
    SYSFLAGS="-g -O"
    FUFLAGS="-O0"
    NETCDFFLAGS="-Df2cFortran"
  esac
  ;;
alphaev*)
  SYSFLAGS="-O3 -arch host -tune host"
  FUFLAGS="-O0"
  FCMFLAG="-nofor_main"
  ;;
powerpc64*linux* )
  case "${FC}" in
  *pgf9* | *ftn* | *pgfortran* )
    SYSFLAGS="-O1 -gopt -Mnoframe -Mdalign -Mbackslash -cpp"
    FUFLAGS="-O0 -g -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    def_compiler="-D_PGI"
    NETCDFFLAGS="-DpgiFortran"
    DEBUG_FLAGS="-g -Minform=inform -Mbounds -Mchkptr -Mchkstk -Meh_frame"
    ;;
  *nvfortran* )
    SYSFLAGS="-O1 -gopt -Mnoframe -Mdalign -Mbackslash -cpp"
    FUFLAGS="-O0 -g -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    def_compiler="-D_PGI"
    NETCDFFLAGS="-DpgiFortran"
    DEBUG_FLAGS="-g -Minform=inform -Mbounds -Mchkptr -Mchkstk -Meh_frame"
    ;;
  *gfortran*)
    SYSFLAGS="-O3 -g -mtune=native"
    FUFLAGS="-O0 -g -mtune=native"
    SLK_FC_FLAGS="-fallow-argument-mismatch"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    NETCDFFLAGS="-DgFortran"
    DEBUG_FLAGS="-g -Wall -pedantic -fbounds-check -ffpe-trap=invalid,zero,overflow"
    ;;
  *)
    CFLAGS="-q64 -O2 -g"
    SYSFLAGS="-q64 -O2 -g -qnoescape -qnostrict -qarch=ppc970 -qtune=ppc970"
    FUFLAGS="-q64 -O0 -g"
    OMPFLAGS=""
    def_compiler="-D_XLF"
  esac
  ;;
powerpc-ibm* )
  CFLAGS="-O -q64"
  SYSFLAGS="-O3 -g -q64 -qstrict -qarch=auto -qtune=auto -qmaxmem=-1"
  FUFLAGS="-q64"
  OMPFLAGS="-qthreaded"
  def_compiler="-D_XLF"
  ;;
mips-sgi-irix*)
  SYSFLAGS="-O3 -r10000 -mips4"
  ;;
*)
  SYSFLAGS="-O"
esac
#
if test "x$build_os" = "xaix" ; then NETCDFFLAGS="$NETCDFFLAGS -DIBMR2Fortran" ; fi
#
AC_MSG_CHECKING([for specific $FC flags])
if test -z "${FCFLAGS}"; then
 FCFLAGS="$SYSFLAGS"
 AC_MSG_RESULT([$FCFLAGS])
else
 AC_MSG_RESULT([(User-defined) $FCFLAGS])
fi
AC_MSG_CHECKING([for specific Open-MP flags])
AC_MSG_RESULT([$OMPFLAGS])
#
AC_MSG_CHECKING([for specific $CC flags])
AC_MSG_RESULT([$CFLAGS])

AC_MSG_CHECKING([for specific unoptimized flags])
if test -z "${UFLAGS}"; then
 FCUFLAGS="$FUFLAGS"
 AC_MSG_RESULT([$FCUFLAGS])
else
 AC_MSG_RESULT([(User-defined) $UFLAGS])
 FCUFLAGS="$UFLAGS"
 FUFLAGS="$UFLAGS"
fi 
#
if test x"$enable_debug_flags" = "xyes"; then 
 FCFLAGS="$DEBUG_FLAGS"  
 FCUFLAGS="$DEBUG_FLAGS"  
 HDF5_MODE="debug";
fi
#
AC_MSG_CHECKING([for specific NETCDF flags])
AC_MSG_RESULT([$NETCDFFLAGS])
#
AC_SUBST(CFLAGS)
AC_SUBST(FCFLAGS)
AC_SUBST(FCUFLAGS)
AC_SUBST(SLK_FC_FLAGS)
AC_SUBST(FUFLAGS)
AC_SUBST(FCMFLAG)
AC_SUBST(OMPFLAGS)
AC_SUBST(NETCDFFLAGS)
AC_SUBST(DEBUG_FLAGS)
AC_SUBST(HDF5_MODE)
AC_SUBST(def_compiler)
])
#
