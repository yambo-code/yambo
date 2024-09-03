#
#        Copyright (C) 2000-2021 the YAMBO team
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
AC_INIT(Yambo Driver Library, yambo@yambo-code.org)
#
m4_include([config/version.m4])
#
AC_MSG_CHECKING(version)
AC_MSG_RESULT($YDRI_VERSION.$YDRI_SUBVERSION.$YDRI_PATCHLEVEL)
#
AC_CONFIG_AUX_DIR(config)
#
# General
#
m4_include([config/acx_misc.m4])
m4_include([config/acx_fpp.m4])
m4_include([config/acx_fortran_flags.m4])
# ============================================================================
# CC
AC_PROG_CC
# ============================================================================
# AR 
AC_CHECK_TOOL(AR, ar, false)
test -z "$AR" && AR=ar
test -z "$AR_FLAGS" && AR_FLAGS="-ru"
AC_SUBST(AR_FLAGS)
# ============================================================================
# Fortran 90
acx_save_fcflags="$FCFLAGS"
AC_PROG_FC([],[90])
if test "$FC" = "" ; then
 AC_MSG_ERROR([could not find Fortran 90 compiler])
fi
AC_LANG_PUSH(Fortran)
#
# F90 extension
#
F90SUFFIX=.f90
AC_ARG_WITH(f90ext, AC_HELP_STRING([--with-f90ext=<flags>],[Use .<flags> Fortran 90 extension],[32]))
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
#
# Fortran FLAGS
#
ACX_FCSETUP()
OPENMPLIBS=""
CUDA_FLAGS=""
AC_SUBST(OPENMPLIBS)
AC_SUBST(CUDA_FLAGS)
# ============================================================================
# Underscore options in subroutine naming
ACX_EXTUS
# ============================================================================
# FPP check
ACX_FPP
# ============================================================================
# Output
AC_LANG_PUSH(Fortran)
AC_CONFIG_FILES([config/defs.mk example/include/version.h])
AC_OUTPUT
