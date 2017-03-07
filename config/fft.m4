#
#        Copyright (C) 2000-2016 the YAMBO team
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

AC_ARG_WITH(fft_libs,AC_HELP_STRING([--with-fft-libs=<libs>],
            [Link to FFT libraries],[32]),[],[])
AC_ARG_WITH(fft_path,AC_HELP_STRING([--with-fft-path=<path>],
            [Path to the FFT install directory],[32]),[],[])
AC_ARG_WITH(fft_libdir,AC_HELP_STRING([--with-fft-libdir=<path>],
            [Path to the FFT lib directory],[32]),[],[])
AC_ARG_WITH(fft_includedir,AC_HELP_STRING([--with-fft-includedir=<path>],
            [Path to the FFT include directory],[32]),[],[])
#
AC_ARG_ENABLE(internal_fftw,AC_HELP_STRING([--enable-internal-fftw],
            [Use internal FFTW library]),[],[])
AC_ARG_ENABLE(internal_fftsg,AC_HELP_STRING([--enable-internal-fftsg],
            [Use internal Goedecker FFT library]),[],[enable_internal_fftsg=yes])
AC_ARG_ENABLE(3d_fft,AC_HELP_STRING([--enable-3d-fft],[Use 3D FFT]),[],[])
#
AC_ARG_WITH(fftsg_fac, AC_HELP_STRING([--with-fftsg-fac=<val>],
            [Change default Goedecker FFT cache factor],[32]))

#
HAVE_FFT="no"
FFT_str="-"
save_ldflags="$LDFLAGS"
try_libs=

#
# F90 module flag
#
IFLAG=$ax_cv_f90_modflag
if test -z "$IFLAG" ; then IFLAG="-I" ; fi

#
# first identifies lib and include dirs
#
try_libdir=
try_incdir=
#
if test -d "$with_fft_path" || test -d "$with_fft_libdir" ; then
  #
  # external FFT
  #
  if test -d "$with_fft_path" ;   then AC_MSG_CHECKING([for FFT in $with_fft_path]) ; fi
  if test -d "$with_fft_libdir" ; then AC_MSG_CHECKING([for FFT in $with_fft_libdir]) ; fi
  #
  if test -d "$with_fft_path" ; then
    try_libdir=$with_fft_path/lib
    try_incdir=$with_fft_path/include
  fi
  if test -d "$with_fft_libdir"     ; then try_libdir=$with_fft_libdir ; fi
  if test -d "$with_fft_includedir" ; then try_incdir=$with_fft_includedir ; fi
  #
  if test -z "$try_libdir" ; then AC_MSG_ERROR([No lib-dir specified]) ; fi
  if test -z "$try_incdir" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
  #
elif test x"$with_fft_libs" != "x" ; then
  #
  # directly provided lib
  #
  AC_MSG_CHECKING([for FFT Library using $with_fft_libs])
  try_libs=$with_fft_libs
  #
else
  AC_MSG_CHECKING([for FFT])
fi
#
# check for FFTW
#
testprog="AC_LANG_PROGRAM([],[
    integer :: i
    call dfftw_destroy_plan(i)
])"
testprog_omp="AC_LANG_PROGRAM([],[
    integer :: i
    call dfftw_init_threads(i)  
    call dfftw_plan_with_nthreads(i)
    call dfftw_destroy_plan(i)
])"
#
if test -d "$try_libdir" && test -d "$try_incdir" ; then
   #
   if test x"$enable_open_mp" = "xyes" && test -e "$try_libdir/libfftw3_omp.a" ; then
       try_libs="-lfftw3 -lfftw3_omp"
   else
       try_libs="-lfftw3"
   fi
   #
fi
#
if ! test x"$try_libs" = "x" ; then
  #
  save_libs=$LIBS
  save_fcflags=$FCFLAGS
  #
  FFT_LIBS="$try_libs";
  FFT_INCS="";
  #
  if test x"$try_libdir" != "x" ; then FFT_LIBS="-L$try_libdir $try_libs" ; fi
  if test x"$try_incdir" != "x" ; then FFT_INCS="$IFLAG$try_incdir" ; fi
  #
  LIBS="$FFT_LIBS $save_libs";
  FCFLAGS="$FFT_INCS $save_fcflags";
  if test x"$enable_open_mp" = "xyes" ; then FCFLAGS="$FCFLAGS $OMPFLAGS" ; fi
  #
  AC_LINK_IFELSE($testprog,     [HAVE_FFTW="yes";],    [HAVE_FFTW="no";])
  AC_LINK_IFELSE($testprog_omp, [HAVE_FFTW_OMP="yes";],[HAVE_FFTW_OMP="no";])
  # 
  LIBS=$save_libs
  FCFLAGS=$save_fcflags
  #
  if test "$HAVE_FFTW" = "yes" ; then
    FFT_CPP="-D_FFTW"
    if test "$HAVE_FFTW_OMP" = "yes" ; then FFT_CPP="-D_FFTW -D_FFTW_OMP" ; fi
    #
    if test "$try_libs" = "-lfftw3" ; then
      FFT_DESCRIPTION="(FFTW v3)";
      AC_MSG_RESULT(FFTW3)
    elif test "$try_libs" = "-lfftw3 -lfftw3_omp" && test "$HAVE_FFTW_OMP" = "yes" ; then
      FFT_DESCRIPTION="(FFTW_OMP v3)";
      AC_MSG_RESULT(FFTW3_OMP)
    else 
      desc=Other
      if ! test -z "`echo $try_libs | grep -i mkl`" ; then desc="MKL" ; fi  
      FFT_DESCRIPTION="(FFTW $desc)";
      AC_MSG_RESULT(FFTW ($desc) )
    fi
    FFT_LIBS="${FFT_PATH} ${try_libs}"
  else
    FFT_CPP="" 
    FFT_LIBS=""
    LDFLAGS="$save_ldflags"
  fi
  if test x"$HAVE_FFTW" = "xyes" ; then
    HAVE_FFT=yes ;
    FFT_str="E" ;
  fi
else
  HAVE_FFTW=no
  HAVE_FFT=no
  FFT_str="-"
fi


#
# check for ESSL FFT
#
if test -d "$try_libdir" && test -d "$try_incdir" ; then
   try_libs="-L$try_libdir -lessl"
fi
#
if ! test x"$try_libs" = "x" && ! test "$HAVE_FFT" = "yes" ; then
  AC_MSG_RESULT(FFTW no)
  # 
  if ! test x"$try_libdir" = "x" ; then FFT_PATH="-L$try_libdir" ; fi
  #
  save_libs=$LIBS
  save_ldflags=$LDFLAGS
  save_fcflags=$FCFLAGS
  LIBS="$FFT_PATH $try_libs"
  if test x"$try_incdir" != "x" ; then FCFLAGS="$FCFLAGS $IFLAG$try_incdir" ; fi
  #
  AC_MSG_CHECKING([for dcft in $LIBS])
  AC_TRY_LINK_FUNC(dcft, [HAVE_ESSL=yes], [HAVE_ESSL=no])
  AC_MSG_RESULT($HAVE_ESSL)
  #
  LIBS=$save_libs
  LDFLAGS=$save_ldflags
  FCFLAGS=$save_fcflags
  #
  if test "$HAVE_ESSL" = "yes" ; then
    AC_MSG_CHECKING([for FFT])
    if ! test x"$enable_3d_fft" = "xno" ; then 
      FFT3D_CPP="-D_USE_3D_FFT"
      FFT_DESCRIPTION="(FFT ESSL (FFTQE) with 3D support)";
    else
      FFT_DESCRIPTION="(FFT ESSL (FFTQE))";
    fi
    FFT_CPP="-D_FFTQE $FFT3D_CPP -D_ESSL"
    FFT_str="E"
    FFT_LIBS="${FFT_PATH} $try_libs"
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
   AC_MSG_CHECKING([for FFT])
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
  if ! test x"$enable_3d_fft" = "xno" ; then 
    FFT3D_CPP="-D_USE_3D_FFT"
    FFT_DESCRIPTION="Internal FFTW2 (FFTQE) with 3D support";
  else
    FFT_DESCRIPTION="Internal FFTW2 (FFTQE)";
  fi
  FFT_CPP="-D_FFTQE $FFT3D_CPP -D_FFTW2"
  FFT_str="I"
  FFT_LIBS="";
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
  fft_cfactor="0"
  case "${host}" in
    powerpc-ibm*)
    fft_cfactor="16"
    ;;
  esac
  #
  if ! test x"$with_fftsg_fac" = "x"; then
    fft_cfactor="$with_fftsg_fac"
  fi
  #
  FFT_DESCRIPTION="Internal Goedecker FFT with $fft_cfactor cache"
  FFT_str="I"
  FFT_CPP="-D_FFTSG"
  FFT_LIBS=""
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
    FFT_str="E"
    AC_F77_WRAPPERS
    AC_DEFINE(_FFTQE)
    AC_CONFIG_HEADERS([lib/fftqe/c_defs.h:lib/fftqe/c_defs.h.in])
    AC_CONFIG_FILES([lib/fftqe/fftqe_defs.h:lib/fftqe/fftqe_defs.h.in])
    #
fi

AC_SUBST(FFT_str)
AC_SUBST(FFT_LIBS)
AC_SUBST(FFT_INCS)
AC_SUBST(FFT_CPP)
AC_SUBST(FFT_DESCRIPTION)
AC_SUBST(compile_fftqe)
AC_SUBST(FFTQELIBS)
])
