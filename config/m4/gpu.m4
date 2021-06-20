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
AC_DEFUN([AC_SET_GPU],[
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
AC_ARG_WITH([cuda-int-libs],
   [AS_HELP_STRING([--with-cuda-int-libs=VAL],[CUDA internal libraries () @<:@default=cufft,cublas,cusolver@:>@])],
   [],[with_cuda_int_libs=cufft,cublas,cusolver])
#
AC_ARG_ENABLE(nvtx,
        [AC_HELP_STRING([--enable-nvtx=<path>], [Enable NVTX support @<:@default=no@:>@])],[],[enable_nvtx="no"])
#
use_int_cuda_libs="no"
enable_nvtx=no
INTCUDA_LIBS=""
DEVXLIBLIB_FLAGS=""
GPU_FLAGS=""
def_gpu=""

# Cuda Fortran
if test x"$enable_cuda_fortran" != "xno" ; then
   #
   def_gpu="-D_CUDA -D__CUDA -D__CUDAF"
   #
   # Flags to be passed to the devicexlib library
   #
   DEVXLIB_FLAGS="--with-cuda"
   if test "x$LIBCUDA_PATH" != "x" ; then
     DEVXLIB_FLAGS="--with-cuda=$LIBCUDA_PATH" ;
   fi
   DEVXLIB_FLAGS="$DEVXLIB_FLAGS --with-cuda-cc=${with_cuda_cc} --with-cuda-runtime=${with_cuda_runtime}"
   #
   # If not set via the configure use the cuda libs internal to the pgi/nvidia compiler
   if test x"$LIBCUDA_LIBS" = "x" -o x"$with_cuda_libs" = "x" ; then
     use_int_cuda_libs="yes" ;
   fi
   #
   case "${FCVERSION}" in
    *nvfortran*)
      if test x"$use_int_cuda_libs" = "xyes" ; then
        INTCUDA_LIBS="-cudalib=${with_cuda_int_libs}";
      fi
      GPU_FLAGS="-cuda -gpu=cc${with_cuda_cc},cuda${with_cuda_runtime} $INTCUDA_LIBS"
      ;;
    *)
      if test x"$use_int_cuda_libs" = "xyes" ; then
        INTCUDA_LIBS="-Mcudalib=${with_cuda_int_libs}";
      fi
      GPU_FLAGS="-Mcuda=cc${with_cuda_cc},cuda${with_cuda_runtime} $INTCUDA_LIBS"
   esac
   #
   if ! test x"$enable_nvtx" = "xno" ; then
     #
     if test x"$enable_nvtx" = "xyes" ; then
        def_gpu="$def_gpu -D_NVTX"
        GPU_FLAGS="$GPU_FLAGS -lnvToolsExt"
     elif ! test x"$enable_nvtx" = "x" ; then
        def_gpu="$def_gpu -D_NVTX"
        GPU_FLAGS="$GPU_FLAGS -L$enable_nvtx/lib64 -lnvToolsExt"
     fi
   fi
fi
#
# Openacc
#
if test x"$enable_openacc" != "xno" ; then
   #
   # Flags to be passed to the devicexlib library
   #
   DEVXLIB_FLAGS="--with-openacc --with-cuda-cc=${with_cuda_cc} --with-cuda-runtime=${with_cuda_runtime}"
   GPU_FLAGS="-fopenacc" # -gpu=cc${with_cuda_cc},cuda${with_cuda_runtime}"
   def_gpu="-D_OPENACC -D__CUDA -D__OPENACC"
   #
fi
#
# OpenMP5
#
if test x"$enable_openmp5" != "xno" ; then
   #
   # Flags to be passed to the devicexlib library
   #
   DEVXLIB_FLAGS="--with-opemp5 --with-cuda-cc=${with_cuda_cc} --with-cuda-runtime=${with_cuda_runtime}"
   GPU_FLAGS="-fopenmp" # -gpu=cc${with_cuda_cc},cuda${with_cuda_runtime}"
   def_gpu="-D_OPENMP5 -D__CUDA -D__OPENMP5"
   #
fi
#
AC_SUBST(with_cuda_cc)
AC_SUBST(with_cuda_runtime)
AC_SUBST(with_cuda_int_libs)
AC_SUBST(def_gpu)
AC_SUBST(GPU_FLAGS)
AC_SUBST(DEVXLIB_FLAGS)
#
])
