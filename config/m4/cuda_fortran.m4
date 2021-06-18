#
#        Copyright (C) 2000-2021 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AF
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
AC_DEFUN([AC_HAVE_CUDA_FORTRAN],[
#
AC_ARG_ENABLE(cuda,
        [AC_HELP_STRING([--enable-cuda], [Enable CUDA support @<:@default=no@:>@])],[],[enable_cuda="no"])
#
AC_ARG_WITH([cuda-cc],
   [AS_HELP_STRING([--with-cuda-cc=VAL],[GPU architecture (Kepler: 35, Pascal: 60, Volta: 70, Ampere: 80) @<:@default=70@:>@])],
   [],[with_cuda_cc=70])
#
# Available cc options
#    cc20            Compile for compute capability 2.0
#    cc30            Compile for compute capability 3.0
#    cc35            Compile for compute capability 3.5
#    cc50            Compile for compute capability 5.0
#    cc60            Compile for compute capability 6.0
#    cc70            Compile for compute capability 7.0
#    cc80            Compile for compute capability 8.0
#
# check your card at https://en.wikipedia.org/wiki/CUDA#GPUs_supported
#
# cc20  for Fermi cards
# cc30 / cc35  for Kepler cards (eg K20, K40, K80)
# cc50  for Maxwell cards
# cc60  for Pascal cards (eg P100)
# cc70  for Volta  cards (eg V100)
# 
AC_ARG_WITH([cuda-runtime],
   [AS_HELP_STRING([--with-cuda-runtime=VAL],[CUDA runtime (Pascal: 8+, Volta: 9+) @<:@default=10.1@:>@])],
   [],[with_cuda_runtime=10.1])
# 
AC_ARG_WITH([cuda-libs],
   [AS_HELP_STRING([--with-cuda-libs=VAL],[CUDA libraries () @<:@default=cufft,cublas,cusolver@:>@])],
   [],[with_cuda_libs=cufft,cublas,cusolver])
#
AC_ARG_ENABLE(nvtx,
        [AC_HELP_STRING([--enable-nvtx=<path>], [Enable NVTX support @<:@default=no@:>@])],[],[enable_nvtx="no"])
#
def_cuda=""
CUDA_FLAGS=""
CUDA_LIBS=""

if test x"$LIBCUDA_LIBS" = "x" ; then
  CUDA_LIBS="${with_cuda_libs}"
fi

if test x"$enable_cuda" != "xno" ; then
   case "${FCVERSION}" in
    *nvfortran*)
      CUDA_FLAGS="-cuda -gpu=cc${with_cuda_cc},cuda${with_cuda_runtime} -cudalib=$CUDA_LIBS"
      ;;
    *)
      CUDA_FLAGS="-Mcuda=cc${with_cuda_cc},cuda${with_cuda_runtime} -Mcudalib=$CUDA_LIBS"
   esac
   CUDA_LIBFLAGS="--with-cuda"
   if test "x$LIBCUDA_PATH" != "x" ; then CUDA_LIBFLAGS="--with-cuda=$LIBCUDA_PATH" ; fi
   CUDA_LIBFLAGS="$CUDA_LIBFLAGS --with-cuda-cc=${with_cuda_cc} --with-cuda-runtime=${with_cuda_runtime}"
   GPU_SUPPORT="cudaf"
   def_cuda="-D_CUDA"
else
  enable_nvtx=no
  def_cuda=""
  CUDA_FLAGS=""
  CUDA_LIBFLAGS=""
fi
#
if ! test x"$enable_nvtx" = "xno" ; then
  #
  if test x"$enable_nvtx" = "xyes" ; then
     def_cuda="$def_cuda -D_NVTX"
     CUDA_FLAGS="$CUDA_FLAGS -lnvToolsExt"
  elif ! test x"$enable_nvtx" = "x" ; then
     def_cuda="$def_cuda -D_NVTX"
     CUDA_FLAGS="$CUDA_FLAGS -L$enable_nvtx/lib64 -lnvToolsExt"
  fi
fi
#
AC_SUBST(enable_cuda)
AC_SUBST(def_cuda)
AC_SUBST(with_cuda_cc)
AC_SUBST(with_cuda_runtime)
AC_SUBST(with_cuda_libs)
AC_SUBST(CUDA_FLAGS)
AC_SUBST(CUDA_LIBFLAGS)
#
])
