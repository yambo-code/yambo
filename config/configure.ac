#
# Copyright (C) 2000-2008 A. Marini and the YAMBO team
#              http://www.yambo-code.org
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
#
AC_INIT(Yambo, 3.2.1 r.458 , yambo@yambo-code.org)
SVERSION="3"
SPATCHLEVEL="2"
SSUBLEVEL="1"
SREVISION="458"
AC_SUBST(SVERSION)
AC_SUBST(SPATCHLEVEL)
AC_SUBST(SSUBLEVEL)
AC_SUBST(SREVISION)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR([driver/driver.c])
m4_include([config/acx_lapack.m4])
m4_include([config/acx_mpi.m4])
m4_include([config/acx_blas.m4])
m4_include([config/acx.m4])
m4_include([config/acx_gmake.m4])
m4_include([config/netcdf_f90.m4])
m4_include([config/fftw.m4])
m4_include([config/slk.m4])
m4_include([config/acx_report.m4])
m4_include([config/acx_get_fc_version.m4])
m4_include([config/acx_cpp.m4])

AC_PREFIX_DEFAULT($PWD)

# Checks for library functions.
AC_CHECK_HEADERS([malloc.h stdlib.h unistd.h string.h sys/time.h])
AC_FUNC_ALLOCA
AC_C_CONST
AC_C_INLINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_SETVBUF_REVERSED
AC_FUNC_STAT
AC_PROG_MAKE_SET

# ============================================================================
# Checks for programs.
AC_PROG_CC
#
AC_CANONICAL_HOST
hostname=`uname -n`
AC_SUBST(hostname)
# ============================================================================
# EDITOR
AC_ARG_WITH(EDITOR, AC_HELP_STRING([--with-editor=<exe>],[User-defined editor])) 
AC_CHECK_PROGS(EDITOR,[$with_editor vim vi pico],["none"])
AC_SUBST(EDITOR)

# ============================================================================
# GNU Make
MAKE="make"
CHECK_GNU_MAKE()
if test "$_cv_gnu_make_command" = ""; then
 ACX_CHECK_MAKE
 if ! test "$make_works" = "yes"; then
  AC_MSG_ERROR(Make does not accept function statments (use gmake if possible))
 fi
else
 MAKE=$_cv_gnu_make_command
fi
AC_SUBST(MAKE)

# ============================================================================
#AR 
#AC_DISABLE_SHARED
AC_CHECK_TOOL(AR, ar, false)
test -z "$AR" && AR=ar
test -z "$AR_FLAGS" && AR_FLAGS="-ru"

# ============================================================================
# DEBUG
AC_ARG_ENABLE(debug, AC_HELP_STRING([--enable-debug],[Objects are not removed but saved in appropriate directories. Default is yes.]))
if test x"$enable_debug" = "x"; then enable_debug="yes"; fi
AC_SUBST(enable_debug)
# ============================================================================
# DP
AC_ARG_ENABLE(dp, AC_HELP_STRING([--enable-dp], [Double-precision build. Default is no.]))
dp_cpp=""
if test x"$enable_dp" = "x"; then enable_dp="no"; fi
if test x"$enable_dp" = "xyes"; then dp_cpp="-D_DOUBLE"; fi
AC_SUBST(enable_dp)
AC_SUBST(dp_cpp)
# ============================================================================
# check for IOTK library 
#
AC_ARG_WITH(iotk, AC_HELP_STRING([--with-iotk=<path>],
 [Path of the IOTK library directory]))
compile_p2y="no"
iotk_dir=""
if ! test "$with_iotk" = "" ; then
 AC_MSG_CHECKING([$with_iotk IOTK library])
 if test -r $with_iotk/src/libiotk.a ; then
  compile_p2y="yes"
  iotk_dir="$with_iotk/src"
  AC_MSG_RESULT([yes])
  cp "$with_iotk/src/libiotk.a" lib/
 else
  AC_MSG_RESULT([no])
 fi
fi
AC_SUBST(compile_p2y)
AC_SUBST(iotk_dir)

# ============================================================================
# check for p2y versions
#
AC_ARG_WITH(p2y, AC_HELP_STRING([--with-p2y=<mode>],
 [Interface mode in PW 2 YAMBO : <export> <3.1> <3.1.1> <3.2> <4.0>]))

PW_VER="3.2"
PW_CPP="_P2Y_V32"
if test "$compile_p2y" = "yes"; then
 if test "$with_p2y" = "export"; then
  PW_VER="export"
  PW_CPP="_P2Y_EXPORT"
 fi
 if test "$with_p2y" = "3.1"; then
  PW_VER="3.1"
  PW_CPP="_P2Y_V31"
 fi
 if test "$with_p2y" = "3.1.1"; then
  PW_VER="3.1.1"
  PW_CPP="_P2Y_V311"
 fi
 if test "$with_p2y" = "3.2"; then
  PW_VER="3.2"
  PW_CPP="_P2Y_V32"
 fi
 if test "$with_p2y" = "4.0"; then
  PW_VER="4.0"
  PW_CPP="_P2Y_V40"
 fi
fi
AC_SUBST(PW_VER)
AC_SUBST(PW_CPP)

# ============================================================================
# check for ETSF library
AC_ARG_WITH(etsf, AC_HELP_STRING([--with-etsf=<path>],
 [Path of the ETSF I/O library directory]))
compile_e2y="no"
etsf_io_dir=""
if ! test "$with_etsf" = "" ; then
 AC_MSG_CHECKING([$with_etsf for the ETSF I/O library])
 if test -r $with_etsf/lib/libetsf_io.a ; then
  compile_e2y="yes"
  etsf_dir="$with_etsf/include/intel"
  AC_MSG_RESULT([yes])
  cp "$with_etsf/lib/libetsf_io.a" lib/
  cp "$with_etsf/lib/libetsf_io_low_level.a" lib/
  cp "$with_etsf/lib/libetsf_io_utils.a" lib/
 else
  AC_MSG_RESULT([no])
 fi
fi
AC_SUBST(compile_e2y)
AC_SUBST(etsf_dir)

# ============================================================================
#
# Verbose compilation
#
AC_ARG_ENABLE(msgs-comps, AC_HELP_STRING([--enable-msgs-comps],
              [Compress compilation messaging. Default is yes.]))
if test x"$enable_msgs_comps" = "x"; then enable_msgs_comps="yes"; fi
MKMF_PREFIX=" "
if test x"$enable_msgs_comps" = "xyes"; then MKMF_PREFIX="@"; fi
AC_SUBST(MKMF_PREFIX)
AC_SUBST(ECHO_N)
#
# ============================================================================
#
# C options specific for driver.c
#
AC_ARG_ENABLE(options_check, AC_HELP_STRING([--enable-options-check],
              [Enable the command line options check in driver.c. Default is yes.]))
if test x"$enable_options_check" = "x"; then enable_options_check="yes"; fi
if test x"$enable_options_check" = "xno"; then
 SAVE=$CFLAGS
 CFLAGS="${SAVE} -D_NO_OPTIONS_CHECK"
fi
#
# ============================================================================
#
# Fortran 90
#
acx_save_fcflags="$FCFLAGS"
AC_PROG_FC([],[90])
if test "$FC" = "" ; then
 AC_MSG_ERROR([could not find Fortran 90 compiler])
fi
# WIDESETUP affects the FCFLAGS/CFLAGS only if the variable is empty
ACX_WIDESETUP
AC_LANG_PUSH(Fortran)
#
# F90 extension
#
AC_ARG_WITH(f90ext, AC_HELP_STRING([--with-f90ext=<ext>],
[Use .<ext> Fortran 90 extension]))
if ! test -z "$with_f90ext"; then F90SUFFIX=".$with_f90ext"; fi
case $F90SUFFIX in
 .f90)
   AC_FC_SRCEXT(f90,[F90SUFFIX=".f90"; F90EXTFLAGS="$FCFLAGS_f90"],[])
   ;;
 .f)
   AC_FC_SRCEXT(f,[F90SUFFIX=".f"; F90EXTFLAGS="$FCFLAGS_f"],[])
   ;;
esac
AC_SUBST(F90SUFFIX)
FCFLAGS="${acx_save_fcflags}"
acx_save_fcflags="$FCFLAGS"
ACX_GET_FC_VERSION()
ACX_FCSETUP()
acx_save_fcflags="$FCFLAGS"
FCFLAGS="$acx_save_fcflags $F90EXTFLAGS"
ACX_EXTUS
AC_FC_FREEFORM
F90=$FC
AC_SUBST(F90)
AC_SUBST(AR_FLAGS)
AC_LANG_POP(Fortran)
#
# ============================================================================
# CPP check & flags
ACX_CPP
#
# ============================================================================
# Fortran 77: test if the FC works as F77 as well
#
AC_LANG_PUSH(Fortran 77)
if test -z "$F77"; then F77=$FC; fi
if test -z "$FFLAGS"; then FFLAGS="$acx_save_fcflags"; fi
AC_PROG_F77
AC_MSG_CHECKING([if Fortran 77 compiler $F77 works])
AC_COMPILE_IFELSE(AC_LANG_PROGRAM([],[       integer ierr]),
[acx_f77_ok="yes"; AC_MSG_RESULT(yes)],
[acx_f77_ok="no"; AC_MSG_RESULT(no);
AC_MSG_ERROR(Fortran 77 compiler does not work. Impossible to continue.)])
AC_LANG_POP(Fortran 77)
# ============================================================================
# check for BLAS/Lapack
compile_blas="no"
compile_lapack="no"
BLAS_LIBS=" "
LAPACK_LIBS=" "
if test "$with_blas" = "" || test "$with_lapack" = "" ; then
 compile_blas="yes"
 compile_lapack="yes"
else
 ACX_BLAS([], 
  compile_blas="yes";
  AC_MSG_NOTICE([Could not find blas. Using the built-in library]))
 if test "$compile_blas" = "no"; then
  ACX_LAPACK([], 
   compile_lapack="yes";
   AC_MSG_NOTICE([Could not find lapack. Using the built-in library]))
 else
  compile_lapack="yes"
 fi
fi
if test x"$compile_blas" = "xyes"; then BLAS_LIBS="-lblas"; fi
if test x"$compile_lapack" = "xyes"; then LAPACK_LIBS="-llapack"; fi
AC_SUBST(BLAS_LIBS)
AC_SUBST(LAPACK_LIBS)
AC_SUBST(compile_blas)
AC_SUBST(compile_lapack)
# ============================================================================
# check for local libs needed
# 
compile_local=no
LOCAL_LIBS=""
if test x"$acx_dlaran_ok" = xno; then
 compile_local=yes
 LOCAL_LIBS="-llocal"
fi
AC_SUBST(compile_local)
AC_SUBST(LOCAL_LIBS)
#
# ============================================================================
# MPI
mpibuild="yes" 
AC_ARG_WITH(mpi, AC_HELP_STRING([--with-mpi=<lib>],[Use MPI library <lib>]))
case $with_mpi in
  yes | "") ;;
  no ) 
    mpibuild="no" 
    ;;
  -* | */* | *.a | *.so | *.so.* | *.o)
    MPILIBS="$with_mpi"
    ;;
  *) 
    MPILIBS="-l$with_mpi"
    ;;
esac
if test "$mpibuild" = "yes"; then
# MPIF90
#
 mpi_cpp="-D_MPI"
 AC_LANG_PUSH(Fortran)
 ACX_MPI([], 
  AC_MSG_WARN([could not compile a FORTRAN mpi test program. YAMBO serial only.]))
 AC_LANG_POP(Fortran)
#
# MPICC
#
 if test "$mpibuild" = "yes" ; then
  AC_LANG_PUSH(C)
  ACX_MPI([], 
   AC_MSG_WARN([could not compile a C mpi test program. YAMBO serial only.]))
 fi
#
# OVERALL CHECK
#
 if test "$mpibuild" = "no" ; then 
  mpi_cpp=""
  MPILIBS=""
  PF90=""
  PF90FLAGS=""
  PCC=""
  PCCFLAGS=""
 fi
 AC_SUBST(mpi_cpp)
 AC_SUBST(MPILIBS)
 AC_SUBST(PF90)
 AC_SUBST(PF90FLAGS)
 AC_SUBST(PCC)
 AC_SUBST(PCCFLAGS)
 AC_SUBST(mpibuild)
fi
# ============================================================================
# FFT 
AC_LANG_PUSH(Fortran)
AC_HAVE_FFTW
# ============================================================================
# SCALAPACK/BLACS
SLK_SETUP
# ============================================================================
# NETCDF
AC_ARG_ENABLE(largedb, AC_HELP_STRING([--enable-largedb],
 [Enable NetCDF large file support. Default is no.]))
if test x"$enable_largedb" = "x"; then enable_largedb="no"; fi
AC_SUBST(enable_largedb)
KH_PATH_NETCDF_F90
# ============================================================================
# Prepare the REPORT file variables
ACX_REPORT()
# ============================================================================
AC_CONFIG_FILES([config/setup config/Makefile config/report
                 src/wf_and_fft/sgfft.F
                 sbin/make_makefile.sh sbin/make_message.pl sbin/objects_debug.sh
                 driver/codever.h driver/editor.h src/external_c/.objects])
AC_OUTPUT
chmod u+x sbin/*
cp config/Makefile .
if test "$mpibuild" = "no" ; then
 cat config/report | grep -v 'Parallel'
else
 cat config/report
fi
