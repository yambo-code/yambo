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
#
m4_include([config/version.m4])
#
CURRENT_DIR=`echo "$PWD" | sed -e "s/ /X/g"`
if ! test -d $CURRENT_DIR ; then 
 AC_MSG_ERROR(Make cannot work using paths which include spaces)
fi
#
AC_PREFIX_DEFAULT($PWD)
if test "$exec_prefix" = "NONE" ; then exec_prefix="$PWD" ; fi
#
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR([driver/driver.c])
#
# YAMBO SPECIFIC
#
m4_include([config/yambo_specific.m4])
# 
#  General
#
m4_include([config/acx_misc.m4])
m4_include([config/acx_gmake.m4])
m4_include([config/acx_cpp.m4])
m4_include([config/acx_fpp.m4])
m4_include([config/ax_f90_module_flag.m4])
m4_include([config/acx_get_fc_kind.m4])
m4_include([config/acx_get_mpi_kind.m4])
m4_include([config/acx_fortran_flags.m4])
m4_include([config/acx_mpi.m4])
m4_include([config/acx_branch.m4])
# 
#  Report
#
m4_include([config/acx_report.m4])
# ============================================================================
# Current branch
ACX_BRANCH
#
# Checks for library functions.
# ============================================================================
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
# check size of pointers to int - needed to decide the size of integer
# arrays in fortran holding C pointers for FFTW
ACX_POINTER_SIZE
SIZEOF_INT_P=$ac_pointersize
AC_SUBST(SIZEOF_INT_P)
# 
# ============================================================================
# Checks for programs.
AC_PROG_CC
#
AC_CANONICAL_HOST
hostname=`uname -n`
AC_SUBST(hostname)
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
AC_CHECK_TOOL(AR, ar, false)
test -z "$AR" && AR=ar
test -z "$AR_FLAGS" && AR_FLAGS="-ru"
AC_SUBST(AR_FLAGS)
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
AC_ARG_WITH(f90ext, AC_HELP_STRING([--with-f90ext=<flags>],
           [Use .<flags> Fortran 90 extension],[32]))
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
ACX_GET_FC_KIND()
#
# Fortran FLAGS
#
ACX_FCSETUP()
#
acx_save_fcflags="$FCFLAGS"
FCFLAGS="$acx_save_fcflags $F90EXTFLAGS"
#
# ============================================================================
#
# OpenMP
#
AC_ARG_ENABLE(open-mp, AC_HELP_STRING([--enable-open-mp],[Enable OpenMP support]))
if test x"$enable_open_mp" = "x"; then enable_open_mp="no"; fi
OPENMPLIBS=" "
def_openmp=" "
if test x"$enable_open_mp" = "xyes"; then 
  OPENMPLIBS="$OMPFLAGS"; 
  def_openmp="-D_OPENMP"
fi
AC_SUBST(def_openmp)
AC_SUBST(OPENMPLIBS)
#
# ============================================================================
#
# Underscore options in subroutine naming
#
ACX_EXTUS
#
# Commented by AM on August 28 because it was imposing an F77 different from the FC
# leading to linking conflicts
#
#AC_F77_WRAPPERS
#
# Free Form sepcific options
#
AC_FC_FREEFORM
#
AC_LANG_POP(Fortran)
#
#=============================================================================
# FLAGS TO INCLUDE MODULE
AX_F90_MODULE_FLAG
#F90_MODULE_FLAG="$ax_cv_f90_modflag"
IFLAG="$ax_cv_f90_modflag"
if test -z "$IFLAG" ; then IFLAG="-I" ; fi
#AC_SUBST([F90_MODULE_FLAG])
AC_SUBST(IFLAG)
#
# ============================================================================
# CPP check & flags
ACX_CPP
#
# ============================================================================
# FPP check
ACX_FPP
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
# 
# ============================================================================
#  Scripts
#
m4_include([config/scripts.m4])
#
# ============================================================================
# MPI
m4_include([config/mpi.m4])
m4_include([config/repos.list])
# 
# ============================================================================
#  Libs
#
m4_include([config/blas.m4])
m4_include([config/lapack.m4])
m4_include([config/netcdf_f90.m4])
m4_include([config/libxc.m4])
m4_include([config/fft.m4])
m4_include([config/iotk.m4])
m4_include([config/etsf_io.m4])
m4_include([config/scalapack.m4])
m4_include([config/petsc_slepc.m4])
#
AC_LANG_PUSH(Fortran)
# ============================================================================
# YDB/YAMBOpy
ACX_SCRIPTS
# ============================================================================
# BLAS/Lapack
ACX_BLAS 
ACX_LAPACK
# ============================================================================
# FFT 
AC_HAVE_FFT
# ============================================================================
# PETSC/SLEPC
AC_PETSC_SLEPC_SETUP
# ============================================================================
# SCALAPACK/BLACS
AC_SLK_SETUP
# ============================================================================
# NETCDF 
AC_HAVE_NETCDF_F90
# ============================================================================
# IOTK library 
AC_HAVE_IOTK
# ============================================================================
# ETSF_IO
AC_HAVE_ETSF_IO
# ============================================================================
# LIBXC
ACX_LIBXC
# ============================================================================
# Prepare the REPORT file variables
ACX_REPORT()
# ============================================================================
AC_CONFIG_HEADERS([include/headers/common/have_malloc.h:include/headers/common/have_malloc.h.in])
AC_CONFIG_FILES([config/setup config/Makefile config/report config/msg_ydb
                 src/wf_and_fft/sgfft.F
                 sbin/make_makefile.sh sbin/objects_store.sh sbin/generate_gpl.sh
                 driver/include/editor.h src/tools/.objects driver/include/version.h include/branch.inc])
#
AC_OUTPUT
#
# Folders
#
m4_include([config/yambo_folders.m4])
#
cat config/report
#
# Messages
#
if test "x$enable_ydb" = "xyes"; then
 cat config/msg_ydb
fi
