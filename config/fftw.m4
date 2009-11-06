#
# Copyright (C) 2000-2009 A. Marini and the YAMBO team
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
AC_DEFUN([AC_HAVE_FFTW],[
AC_ARG_WITH(fftw,AC_HELP_STRING([--with-fftw=<path>],
            [Path of the FFTW library directory]),[],[])
AC_MSG_CHECKING([for FFTW])
FFTW_LIBS="-lfftw3"
FFTW_PATH=""
case $with_fftw in
  no )
    FFTW_LIBS=""
    ;;
  "" ) ;;
  *)
    FFTW_PATH="-L$with_fftw"
    ;;
esac
save_ldflags="$LDFLAGS"
AS_IF([test "$FFTW_LIBS"], [LIBS="${FFTW_PATH} ${FFTW_LIBS}"])
AC_LINK_IFELSE([AC_LANG_CALL([], [dfftw_destroy_plan(1)])],
 [HAVE_FFTW="yes"; FFT_CPP="-D_FFTW";
 FFT_DESCRIPTION="FFTW Fast Fourier transform";LDFLAGS="$FFTW_PATH"],
 [HAVE_FFTW="no"; FFT_CPP=""; FFTW_LIBS="";LDFLAGS="$save_ldflags"])
AC_MSG_RESULT($HAVE_FFTW)
if test "$HAVE_FFTW" = "no" ; then
# SG FFT NCACHE 
 AC_ARG_WITH(fftfac, AC_HELP_STRING([--with-sgfftfac=<fac>],
                     [Change default Goedecker FFT cache factor]))
 fft_cfactor="0"
 case "${host}" in
  powerpc-ibm*)
  fft_cfactor="16"
  ;;
 esac
 if ! test x"$with_fftfac" = "x"; then
  fft_cfactor="$with_fftfac"
 fi
 FFT_DESCRIPTION="Goedecker Fast Fourier transform with $fft_cfactor cache"
 AC_SUBST(fft_cfactor)
fi
AC_SUBST(FFTW_LIBS)
AC_SUBST(FFT_CPP)
AC_SUBST(FFT_DESCRIPTION)])
