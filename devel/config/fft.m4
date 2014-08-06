#
#        Copyright (C) 2000-2014 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM, AF
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
AC_DEFUN([AC_HAVE_FFT],[
AC_ARG_WITH(fftw,AC_HELP_STRING([--with-fftw=<path>],
            [Path of the FFTW library directory]),[],[])
AC_ARG_WITH(fftw_lib,AC_HELP_STRING([--with-fftw-lib=<lib>],
            [FFTW library name]),[],[])
AC_ARG_WITH(essl_fft_lib,AC_HELP_STRING([--with-essl-fft-lib=<lib>],
            [ESSL-FFT library name]),[],[])
AC_ARG_WITH(internal_fft,AC_HELP_STRING([--with-internal-fft=<lib>],
            [Use internal FFT library]),[],[])
AC_ARG_ENABLE(3d_fft,AC_HELP_STRING([--enable-3d-fft],[Use 3D FFT]),
            [FFT3D_CPP="-D_USE_3D_FFT"])
AC_MSG_CHECKING([for FFT])
#
HAVE_FFT="no"

#
# FFTW3
#
FFTW_PATH=""
case $with_fftw in
  no )
    FFTW_LIBS=""
    ;;
  "" | * ) 
    FFTW_PATH="-L$with_fftw"
    ;;
esac
save_ldflags="$LDFLAGS"

case $with_fftw_lib in
  no | "" )
    if test x"$enable_open_mp" = "xyes"; then
        EXTERNAL_FFTW="-lfftw3 -lfftw3_omp"
    else
        EXTERNAL_FFTW="-lfftw3"
    fi
  ;;
  *)
    EXTERNAL_FFTW="$with_fftw_lib";
  ;;
esac

if test -d "$with_fftw" ; then
  for FFTW_LIBS in "$EXTERNAL_FFTW" ; do      
    AS_IF([test "$FFTW_LIBS"], [LIBS="${FFTW_PATH} ${FFTW_LIBS}"])
    AC_LINK_IFELSE([AC_LANG_CALL([], [dfftw_destroy_plan(1)])],
    [HAVE_FFTW="yes";],[HAVE_FFTW="no";])
    if test "$HAVE_FFTW" = "yes" ; then
      break;
    fi
  done
  if test "$HAVE_FFTW" = "yes" ; then
    FFT_CPP="-D_FFTW"
    if test "$FFTW_LIBS" = "-lfftw3" ; then
      FFT_DESCRIPTION="FFTW (v3) Fast Fourier transform";
      AC_MSG_RESULT(FFTW3)
    elif test "$FFTW_LIBS" = "-lfftw3 -lfftw3_omp" ; then
      FFT_DESCRIPTION="FFTW_OMP (v3) Fast Fourier transform";
      AC_MSG_RESULT(FFTW3_OMP)
    else 
      FFT_DESCRIPTION="External Fast Fourier transform";
    fi
    LDFLAGS="$FFTW_PATH";
  else
    FFT_CPP=""; 
    FFTW_LIBS="";
    LDFLAGS="$save_ldflags";
  fi
else
 HAVE_FFTW="no";
fi
if test "$HAVE_FFTW" = "yes" ; then 
   HAVE_FFT=yes 
else
   HAVE_FFT=no
fi


#
# ESSL FFT
#
if test -d "$essl_fft_lib" ; then
    #AC_SEARCH_LIBS(dcft, essl, have_essl_fft=1 fft_libs="$essl_fft_lib")
    AC_CHECK_LIB(essl, dcft, [have_essl_fft=1; fft_libs="$essl_fft_lib"], [have_essl_fft=0])
    if test "$have_essl_fft" = "1" ; then
       FFT_CPP="-D_FFTQE $FFT3D_CPP -D_ESSL"
       FFT_DESCRIPTION="ESSL Fast Fourier transform (FFTQE)";
       HAVE_FFT=yes
       compile_fftqe=yes
       AC_MSG_RESULT(ESSL FFT)
    fi
fi


#
# INTERNAL FFT
#
case $with_internal_fft in
  fftw )
    use_internal_fftw=yes
    compile_fftqe=yes
    ;;
  fftsg )
    use_internal_fftsg=yes
    ;;
#  "" | * ) 
#    use_internal_fftsg=yes
#    ;;
esac


#
# INTERNAL FFTW2
#
if test "$use_internal_fftw" = "yes" ; then
  FFT_CPP="-D_FFTQE $FFT3D_CPP -D_FFTW2"
  FFT_DESCRIPTION="Internal FFTW2 Fast Fourier transform (FFTQE)";
  FFTW_LIBS="";
  HAVE_FFTW=yes
  compile_fftqe=yes
  AC_MSG_RESULT(FFTW2)
fi
if test "$HAVE_FFTW" = "yes" ; then HAVE_FFT=yes ; fi



#
# INTERNAL GOEDECKER FFT
#
if test "$HAVE_FFT" = "no" -o "$use_internal_fftsg" = "yes" ; then
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
 FFT_CPP="-D_FFTSG"
 HAVE_FFTSG=yes
 AC_MSG_RESULT(FFTSG)
 AC_SUBST(fft_cfactor)
fi
if test "$HAVE_FFTSG" = "yes" ; then HAVE_FFT=yes ; fi

#
# finalize
#
if test "$compile_fftqe" = "yes" ; then 
    FFTQELIBS="-lfftqe" 
    AC_F77_WRAPPERS
    AC_DEFINE(_FFTQE)
    AC_CONFIG_HEADERS([lib/fftqe/c_defs.h:lib/fftqe/c_defs.h.in])
    AC_CONFIG_FILES([lib/fftqe/fftqe_defs.h:lib/fftqe/fftqe_defs.h.in])
fi

AC_SUBST(FFTW_LIBS)
AC_SUBST(FFT_CPP)
AC_SUBST(FFT_DESCRIPTION)
AC_SUBST(compile_fftqe)
AC_SUBST(FFTQELIBS)
])
