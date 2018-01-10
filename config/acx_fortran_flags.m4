#
#        Copyright (C) 2000-2018 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM
#
# This file is distributed under the terms of the GNU
# General Public License. You can redistribute it and/or
# modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation;
# either version 2, or (at your option) any later version.
#
# This program is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
# MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
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
AC_ARG_ENABLE(debug-flags, AC_HELP_STRING([--enable-debug-flags],
              [Debug flags are set for compilation. Default is no.]))
if test x"$enable_debug_flags" = "x"; then enable_debug_flags="no"; fi
#
def_compiler=
#
case "${host}" in
i?86*linux*)
  case "${FC}" in
  *pgf9* | *ftn* | *pgfortran* )
    SYSFLAGS="-O2 -fast -Munroll -Mnoframe -Mdalign -Mbackslash"
    FUFLAGS="-O0 -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    NETCDFFLAGS="-DpgiFortran"
    def_compiler="-D_PGI"
    DEBUG_FLAGS=""
    ;;
  *abf90*)
    SYSFLAGS="-B101 -YEXT_NAMES=LCS -YEXT_SFX=_"
    ;;
  *ifc*)
    SYSFLAGS="-g -O3 -w -tpp7"
    FUFLAGS="-g -O0 -w -tpp7"
    FCMFLAG=""
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    DEBUG_FLAGS=""
    ;;
  *g95*)
    SYSFLAGS="-g -O3 -fbackslash -fno-second-underscore -mtune=pentium4"
    FUFLAGS="-g -O0 -fbackslash -fno-second-underscore"
    FCMFLAG=""
    OMPFLAGS=""
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check -ftrace=full"
    ;;
  *gfortran*)
    SYSFLAGS="-g -O3 -mtune=native"
    FUFLAGS="-g -O0 -mtune=native"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    NETCDFFLAGS="-DgFortran"
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check"
    ;;
  *ifort*)
    DEBUG_FLAGS="-CA -CB"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    CPU_FLAG=""
    case "${FCVERSION}" in
      *11* | *12* | *13* |*14* | *15* | *16* )
       #CPU_FLAG="-xHost"
       CPU_FLAG=" "
       ;;
      *17* | *18* | *19*)
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
    SYSFLAGS="-assume bscc -g -O3 -ip $CPU_FLAG"
    FUFLAGS="-assume bscc -g -O0 $CPU_FLAG"
    FCMFLAG="-nofor_main"
  ;;
  *pathf9*)
    SYSFLAGS="-g -O2 -fno-second-underscore"
    FUFLAGS="-g -O0 -fno-second-underscore"
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
*86*apple* )
  case "${FC}" in
  *pgf9* | *ftn* | *pgfortran* )
    SYSFLAGS="-O2 -fast -Munroll -Mnoframe -Mdalign -Mbackslash"
    FUFLAGS="-O0 -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    NETCDFFLAGS="-DpgiFortran"
    ;;
  *gfortran*)
    SYSFLAGS="-g -O3 -mtune=native"
    FUFLAGS="-g -O0 -mtune=native"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    NETCDFFLAGS="-DgFortran"
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check"
    ;;
  *g95*)
    SYSFLAGS="-g -O3 -fno-second-underscore -mtune=pentium4"
    FUFLAGS="-g -O0 -fno-second-underscore"
    FCMFLAG=""
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check -ftrace=full"
    ;;
  *ifort*)
    DEBUG_FLAGS="-CA -CB"
    CPU_FLAG=""
    case "${FCVERSION}" in
      *1*)
       CPU_FLAG="-mtune=pentium4"
       ;;
      *)
       CPU_FLAG="-mtune=pentium4"
       ;;
    esac
    SYSFLAGS="-assume bscc -g -O3 -ip ${CPU_FLAG}"
    FUFLAGS="-assume bscc -g -O0 ${CPU_FLAG}"
    FCMFLAG="-nofor_main"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    ;;
  *)
    SYSFLAGS="-O"
    FUFLAGS="-O0"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-Df2cFortran"
  esac
  ;;
ia64*linux* )
  case "${FC}" in
  *pgf9* | *ftn* | *pgfortran* )
    SYSFLAGS="-O2 -fast -Munroll -Mnoframe -Mdalign -Mbackslash"
    FUFLAGS="-O0 -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    NETCDFFLAGS="-DpgiFortran"
    def_compiler="-D_PGI"
    ;;
  *gfortran*)
    SYSFLAGS="-g -O3 -mtune=native"
    FUFLAGS="-g -O0 -mtune=native"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    NETCDFFLAGS="-DgFortran"
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check "
    ;;
  *g95*)
    SYSFLAGS="-g -O3 -fbackslash -fno-second-underscore"
    FUFLAGS="-g -O0 -fbackslash -fno-second-underscore"
    FCMFLAG=""
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check -ftrace=full"
    ;;
  *abf90*)
    SYSFLAGS="-B101 -YEXT_NAMES=LCS -YEXT_SFX=_"
    ;;
  *ifc*)
    SYSFLAGS="-g -O3 -w"
    FUFLAGS="-g -O0 -w"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    ;;
  *ifort*)
    DEBUG_FLAGS="-CA -CB"
    CPU_FLAG=""
    case "${FCVERSION}" in
      *1*)
       CPU_FLAG="-mtune=itanium"
       ;;
      *)
       CPU_FLAG=""
       ;;
    esac
    SYSFLAGS="-assume bscc -g -O3 -ip ${CPU_FLAG}"
    FUFLAGS="-assume bscc -g -O0 ${CPU_FLAG}"
    FCMFLAG="-nofor_main"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
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
*x86*64* )
  case "${FC}" in
  *pgf9* | *ftn* | *pgfortran* )
    SYSFLAGS="-O2 -fast -Munroll -Mnoframe -Mdalign -Mbackslash"
    FUFLAGS="-O0 -Mbackslash"
    FCMFLAG="-Mnomain"
    OMPFLAGS="-mp"
    def_compiler="-D_PGI"
    NETCDFFLAGS="-DpgiFortran"
    ;;
  *gfortran*)
    SYSFLAGS="-g -O3 -mtune=native"
    FUFLAGS="-g -O0 -mtune=native"
    FCMFLAG=""
    OMPFLAGS="-fopenmp"
    NETCDFFLAGS="-DgFortran"
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check "
    ;;
  *g95*)
    SYSFLAGS="-g -O3 -fbackslash -fno-second-underscore"
    FUFLAGS="-g -O0 -fbackslash -fno-second-underscore"
    FCMFLAG=""
    DEBUG_FLAGS="-Wall -pedantic -fbounds-check -ftrace=full"
    ;;
  *abf90*)
    SYSFLAGS="-B101 -YEXT_NAMES=LCS -YEXT_SFX=_"
    ;;
  *ifc*)
    SYSFLAGS="-g -O3 -w -tpp2"
    FUFLAGS="-g -O0 -w -tpp2"
    OMPFLAGS="-openmp"
    NETCDFFLAGS="-DpgiFortran"
    ;;
  *ifort*)
    DEBUG_FLAGS="-CA -CB"
    OMPFLAGS="-openmp"
    CPU_FLAG=""
    case "${FCVERSION}" in
      *11* | *12* | *13* |*14* |*15* | *16* )
       #CPU_FLAG="-xHost"
       CPU_FLAG=" "
       ;;
      *17* | *18* | *19*)
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
    SYSFLAGS="-assume bscc -g -O3 -ip ${CPU_FLAG}"
    FUFLAGS="-assume bscc -g -O0 ${CPU_FLAG}"
    FCMFLAG="-nofor_main"
    NETCDFFLAGS="-DpgiFortran"
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
  CFLAGS="-q64 -O2"
  SYSFLAGS="-q64 -O2 -qnoescape -qnostrict -qarch=ppc970 -qtune=ppc970"
  FUFLAGS="-q64 -O0"
  OMPFLAGS=""
  def_compiler="-D_XLF"
  ;;
powerpc-ibm* )
  CFLAGS="-O -q64"
  SYSFLAGS="-O3 -q64 -qstrict -qarch=auto -qtune=auto -qmaxmem=-1"
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
if test x"$enable_debug_flags" = "xyes"; then FCFLAGS="$FUFLAGS $DEBUG_FLAGS" ; fi
#
AC_MSG_CHECKING([for specific NETCDF flags])
AC_MSG_RESULT([$NETCDFFLAGS])
#
AC_SUBST(CFLAGS)
AC_SUBST(FCFLAGS)
AC_SUBST(FCUFLAGS)
AC_SUBST(FUFLAGS)
AC_SUBST(FCMFLAG)
AC_SUBST(OMPFLAGS)
AC_SUBST(NETCDFFLAGS)
AC_SUBST(DEBUG_FLAGS)
AC_SUBST(def_compiler)
])
#
