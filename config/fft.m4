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

#AC_ARG_WITH(fftw,AC_HELP_STRING([--with-fftw=<path>],
#            [Path of the FFTW library directory]),[],[])
#AC_ARG_WITH(fftw_lib,AC_HELP_STRING([--with-fftw-lib=<lib>],
#            [FFTW library name]),[],[])
#AC_ARG_WITH(essl_fft_lib,AC_HELP_STRING([--with-essl-fft-lib=<lib>],
#            [ESSL-FFT library name]),[],[])

AC_ARG_WITH(fft_path,AC_HELP_STRING([--with-fft-path=<path>],
            [Path to the FFTW install directory]),[],[])
AC_ARG_WITH(fft_libdir,AC_HELP_STRING([--with-fft-libdir=<path>],
            [Path to the FFTW lib directory]),[],[])
AC_ARG_WITH(fft_includedir,AC_HELP_STRING([--with-fft-includedir=<path>],
            [Path to the FFT include directory]),[],[])
AC_ARG_WITH(fft_libs,AC_HELP_STRING([--with-fft-libs=<libs>],
            [Link to FFT libraries]),[],[])
#
AC_ARG_ENABLE(internal_fftw,AC_HELP_STRING([--enable-internal-fftw],
            [Use internal FFTW library]),[],[])
AC_ARG_ENABLE(internal_fftsg,AC_HELP_STRING([--enable-internal-fftsg],
            [Use internal Goedecker FFT library]),[],[enable_internal_fftsg=yes])
AC_ARG_ENABLE(3d_fft,AC_HELP_STRING([--enable-3d-fft],[Use 3D FFT]),[],[])

if ! test x"$enable_3d_fft" = "xno" ; then FFT3D_CPP="-D_USE_3D_FFT" ; fi
#
HAVE_FFT="no"
save_ldflags="$LDFLAGS"

#
# first identifies lib and include dirs
#
libdir=
includedir=
#
if test -d "$with_fft_path/lib" && test -d "$with_fft_path/include" ; then
   AC_MSG_CHECKING([for FFT in $with_fft_path])
   libdir=$with_fft_path/lib
   includedir=$with_fft_path/include
elif  test -d "$with_fft_includedir" && test -d "$with_fft_libdir" ; then
   AC_MSG_CHECKING([for FFT in $with_fft_libdir])
   libdir=$with_fft_libdir
   includedir=$with_fft_includedir
else
   AC_MSG_CHECKING([for FFT])
fi

#
# check for FFTW/ESSL: init
#

if ! test x"$with_fft_libs" = "x" ; then
   EXTERNAL_LIB=$with_fft_libs
else
   EXTERNAL_LIB=
fi

#
# check for FFTW
#
if test -d "$libdir" && test -d "$includedir" ; then
   #
   if test x"$enable_open_mp" = "xyes"; then
       EXTERNAL_LIB="-lfftw3 -lfftw3_omp"
   else
       EXTERNAL_LIB="-lfftw3"
   fi
   #
fi
#
if ! test x"$EXTERNAL_LIB" = "x" ; then

  for FFTW_LIBS in "$EXTERNAL_LIB" ; do      
    if ! test x"$libdir" = "x" ; then FFTW_PATH="-L$libdir" ; fi
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
      desc=Other
      if ! test -z "`echo $FFTW_LIBS | grep -i mkl`" ; then desc="MKL" ; fi  
      FFT_DESCRIPTION="FFTW ($desc) Fast Fourier transform";
      AC_MSG_RESULT(FFTW ($desc) )
    fi
    LDFLAGS="$FFTW_PATH";    ## ??
  else
    FFT_CPP=""; 
    FFTW_LIBS="";
    LDFLAGS="$save_ldflags";
  fi
  if test x"$HAVE_FFTW" = "xyes" ; then HAVE_FFT=yes ; fi
else
  HAVE_FFTW=no
  HAVE_FFT=no
fi


#
# check for ESSL FFT
#
if test -d "$libdir" && test -d "$includedir" ; then
   EXTERNAL_LIB="-lessl"
fi
#
if ! test x"$EXTERNAL_LIB" = "x" && ! test "$HAVE_FFT" = "yes" ; then
    # 
    AC_CHECK_LIB(essl, dcft, [HAVE_ESSL=yes; fft_libs="$essl_fft_lib"], [HAVE_ESSL=no])
    if test "$HAVE_ESSL" = "yes" ; then
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
if ! test x"$HAVE_FFT" = "xyes" ; then
   #
   if test x"$enable_internal_fftw" = "xyes" ; then
      use_internal_fftw=yes
      use_internal_fftsg=no
      compile_fftqe=yes
   else
      use_internal_fftsg=yes
      compile_fftqe=no
   fi
fi

#
# INTERNAL FFTW2
#
if test "$use_internal_fftw" = "yes" ; then
  FFT_CPP="-D_FFTQE $FFT3D_CPP -D_FFTW2"
  FFT_DESCRIPTION="Internal FFTW2 Fast Fourier transform (FFTQE)";
  FFTW_LIBS="";
  HAVE_FFTW=yes
  compile_fftqe=yes
  AC_MSG_RESULT(Internal FFTW2 (FFTQE))
fi
if test "$HAVE_FFTW" = "yes" ; then HAVE_FFT=yes ; fi


#
# INTERNAL GOEDECKER FFT
#
if test "$use_internal_fftsg" = "yes" ; then
  # SG FFT NCACHE 
  AC_ARG_WITH(fftfac, AC_HELP_STRING([--with-fftsg-fac=<val>],
                      [Change default Goedecker FFT cache factor]))
  fft_cfactor="0"
  case "${host}" in
    powerpc-ibm*)
    fft_cfactor="16"
    ;;
  esac
  #
  if ! test x"$with_fftfac" = "x"; then
    fft_cfactor="$with_fftfac"
  fi
  #
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
if test x"$compile_fftqe" = "xyes" ; then 
    FFTQELIBS="-lfftqe" 
    AC_F77_WRAPPERS
    AC_DEFINE(_FFTQE)
    AC_CONFIG_HEADERS([lib/fftqe/c_defs.h:lib/fftqe/c_defs.h.in])
    AC_CONFIG_FILES([lib/fftqe/fftqe_defs.h:lib/fftqe/fftqe_defs.h.in])
    #
fi

AC_SUBST(FFTW_LIBS)
AC_SUBST(FFT_CPP)
AC_SUBST(FFT_DESCRIPTION)
AC_SUBST(compile_fftqe)
AC_SUBST(FFTQELIBS)
])
