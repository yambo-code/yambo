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
# nvfortran options (nvfortran --help)
#
#-ta=host|multicore|tesla
#                    Choose target accelerator (supported only for OpenACC, DEPRECATED please refer to -acc and -gpu)
#    host            Compile for serial execution on the host CPU
#    multicore       Compile for parallel execution on the host CPU
#    tesla           Compile for parallel execution on a Tesla GPU
#
# -[no]acc[=gpu|host|multicore|[no]autopar|[no]routineseq|legacy|strict|verystrict|sync|[no]wait]
#                    Enable OpenACC directives
#    gpu             OpenACC directives are compiled for GPU execution only; please refer to -gpu for target specific options
#    host            Compile for serial execution on the host CPU
#    multicore       Compile for parallel execution on the host CPU
#    [no]autopar     Enable (default) or disable loop autoparallelization within acc parallel
#    [no]routineseq  Compile every routine for the device
#    legacy          Suppress warnings about deprecated PGI accelerator directives
#    strict          Issue warnings for non-OpenACC accelerator directives
#    verystrict      Fail with an error for any non-OpenACC accelerator directive
#    sync            Ignore async clauses
#    [no]wait        Wait for each device kernel to finish
#
#-gpu=cc35|cc50|cc60|cc62|cc70|cc72|cc75|cc80|ccall|cudaX.Y|fastmath|[no]flushz|[no]fma|keep|[no]lineinfo|llc|zeroinit|deepcopy|loadcache:{L1|L2}|maxregcount:<n>|pinned|[no]rdc|safecache|[no]unroll|managed|beta|autocompare|redundant
#                    Select specific options for GPU code generation
#    cc35            Compile for compute capability 3.5
#    cc50            Compile for compute capability 5.0
#    cc60            Compile for compute capability 6.0
#    cc62            Compile for compute capability 6.2
#    cc70            Compile for compute capability 7.0
#    cc72            Compile for compute capability 7.2
#    cc75            Compile for compute capability 7.5
#    cc80            Compile for compute capability 8.0
#    ccall           Compile for all supported compute capabilities
#    cudaX.Y         Use CUDA X.Y Toolkit compatibility, where installed
#    fastmath        Use fast math library
#    [no]flushz      Enable flush-to-zero mode on the GPU
#    [no]fma         Generate fused mul-add instructions (default at -O3)
#    keep            Keep kernel files
#    [no]lineinfo    Generate GPU line information
#    zeroinit        Initialize allocated device memory with zero
#    deepcopy        Enable Full Deepcopy support in OpenACC Fortran
#    loadcache       Choose what hardware level cache to use for global memory loads
#     L1             Use L1 cache
#     L2             Use L2 cache
#    maxregcount:<n> Set maximum number of registers to use on the GPU
#    pinned          Use CUDA Pinned Memory
#    [no]rdc         Generate relocatable device code
#    safecache       Allows variable-sized array sections in cache directives and assumes they fit into CUDA shared memory
#    [no]unroll      Enable automatic inner loop unrolling (default at -O3)
#    managed         Use CUDA Managed Memory
#    beta            Enable beta code generation features
#    autocompare     Automatically compare CPU/GPU results: implies redundant
#    redundant       Redundant CPU/GPU execution



AC_ARG_WITH([cuda-runtime],
   [AS_HELP_STRING([--with-cuda-runtime=VAL],[CUDA runtime (Pascal: 8+, Volta: 9+) @<:@default=10.1@:>@])],
   [],[with_cuda_runtime=10.1])
# 
AC_ARG_WITH([cuda-int-libs],
   [AS_HELP_STRING([--with-cuda-int-libs=VAL],[CUDA internal libraries () @<:@default=cuda,cufft,cublas,cusolver,cudart@:>@])],
   [],[with_cuda_int_libs=cufft,cublas,cusolver])
#
AC_ARG_ENABLE(nvtx,
        [AS_HELP_STRING([--enable-nvtx=<path>], [Enable NVTX support @<:@default=no@:>@])],[],[enable_nvtx="no"])
#
AC_ARG_WITH(gpu_libs, [AS_HELP_STRING([--with-gpu-libs=<libs>],
            [Use extra GPU-specific libraries <libs>],[32])])
AC_ARG_WITH(gpu_incs, [AS_HELP_STRING([--with-gpu-incs=<incs>],
            [Includes for extra GPU-specific libraries <incs>],[32])])
#
AC_ARG_WITH(rocm_libs, [AS_HELP_STRING([--with-rocm-libs=<libs>], 
            [Use librocm library <libs>],[32])])
AC_ARG_WITH(rocm_incs, [AS_HELP_STRING([--with-rocm-incs=<incs>], 
            [Use librocm include options <incs>],[32])])
AC_ARG_WITH(rocm_libdir, [AS_HELP_STRING([--with-rocm-libdir=<path>], 
            [Path to the rocm lib directory],[32])])
AC_ARG_WITH(rocm_includedir, [AS_HELP_STRING([--with-rocm-includedir=<path>], 
            [Path to the rocm include directory],[32])])
AC_ARG_WITH(rocm_path, [AS_HELP_STRING([--with-rocm-path=<path>], 
            [Path to rocm install directory],[32])])
#
AC_ARG_WITH(mklgpu_libs, [AS_HELP_STRING([--with-mklgpu-libs=<libs>], 
            [Use librocm library <libs>],[32])])

use_int_cuda_libs="no"
use_gpu_libs="no"
enable_nvtx=no
DEVXLIB_CUDALIBS=""
DEVXLIBLIB_FLAGS=""
GPU_LIBS=""
GPU_FLAGS=""
def_gpu=""


 #
 # If not set via the configure use the cuda libs internal to the pgi/nvidia compiler
 DEVXLIB_CUDALIBS="";
 if test x"$LIBCUDA_LIBS" = "x" -o x"$with_cuda_libs" = "x" ; then
   use_int_cuda_libs="yes" ;
   DEVXLIB_CUDALIBS="${with_cuda_int_libs}";
 fi

 #
 # GPU aux libraries
 GPU_LIBS="";
 if test x"$GPU_LIBS" = "x" -o x"$with_gpu_libs" = "x" ; then
     GPU_LIBS="$with_gpu_libs";
 fi
 GPU_INCS="";
 if test x"$GPU_INCS" = "x" -o x"$with_gpu_incs" = "x" ; then
     GPU_INCS="$with_gpu_incs";
 fi

#
dnl Heuristics to detect ROCm dir
#if test "x$with_rocm_path" = "x" ; then with_rocm_path="$ROCM_PATH" ; fi
#if test "x$with_rocm_path" = "x" ; then with_rocm_path="$ROCM_ROOT" ; fi
#if test "x$with_rocm_path" = "x" ; then with_rocm_path="$ROCM_HOME" ; fi

LIBROCM_PATH="$with_rocm_path"

if test x"$with_rocm_path" != x"" && test -d "$with_rocm_path" ; then
   librocm_incdir="$with_rocm_path/include"
   librocm_libdir="$with_rocm_path/lib"
   if ! test -d "$librocm_libdir" ; then librocm_libdir="$with_rocm_path/lib" ; fi
fi
if test x"$with_rocm_includedir" != x"" && test -d "$with_rocm_includedir" ; then
   librocm_incdir="$with_rocm_includedir"
   LIBROCM_LIBS="-L$librocm_libdir -lrocblas"
fi
if test  x"$with_rocm_libdir" != "$with_rocm_libdir" && test -d "$with_rocm_libdir" ; then
   librocm_libdir="$with_rocm_libdir"
   LIBROCM_INCS="-I$librocm_incdir"
fi
#
if test x"$with_rocm_libs" != x"" ; then  LIBROCM_LIBS="$with_rocm_libs" ; fi
if test x"$with_rocm_incs" != x"" ; then  LIBROCM_INCS="$with_rocm_incs" ; fi

# MKL-GPU
if test x"$with_mklgpu_libs" != x"" ; then MKLGPU_LIBS="$with_mklgpu_libs" ; fi

# Cuda Fortran
if test x"$enable_cuda_fortran" != "xno" ; then
   #
   def_gpu="-D_GPU -D_CUDA -D_CUDAF"
   #
   # Flags to be passed to the devicexlib library
   #
   DEVXLIB_FLAGS="--enable-openmp --enable-cuda-fortran --with-cuda-cc=${with_cuda_cc} --with-cuda-runtime=${with_cuda_runtime}"
   #
   case "${FCVERSION}" in
    *nvfortran*)
      GPU_FLAGS="-cuda -gpu=cc${with_cuda_cc},cuda${with_cuda_runtime}"
      if test x"$use_int_cuda_libs" = "xyes" ; then
        GPU_FLAGS+=" -cudalib=${with_cuda_int_libs}";
      fi
      ;;
    *)
      GPU_FLAGS="-Mcuda=cc${with_cuda_cc},cuda${with_cuda_runtime}"
      if test x"$use_int_cuda_libs" = "xyes" ; then
        GPU_FLAGS+=" -Mcudalib=${with_cuda_int_libs}"
      fi
   esac
   #
   # Check CUDA
   #
   AC_LANG_PUSH([Fortran])
   AC_FC_SRCEXT([f90])
   AX_CHECK_COMPILE_FLAG([$GPU_FLAGS], [have_cudafor=yes], [have_cudafor=no], [], [MODULE test; use cudafor; END MODULE])
   AC_LANG_POP([Fortran])
   if test "x$have_cudafor" != "xyes"; then
      AC_MSG_ERROR([You do not have the cudafor module. Are you using a PGI/NVIDIA compiler?])
   fi
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
   DEVXLIB_FLAGS="--enable-openacc --with-cuda-cc=${with_cuda_cc} --with-cuda-runtime=${with_cuda_runtime} "
   def_gpu="-D_GPU -D_CUDA -D_OPENACC"
   #
   case "${FCVERSION}" in
    *nvfortran*)
      DEVXLIB_FLAGS+="--enable-openmp"
      GPU_FLAGS="-acc=gpu,multicore -acclibs -gpu=cc${with_cuda_cc},cuda${with_cuda_runtime} " # -gpu=cc${with_cuda_cc},cuda${with_cuda_runtime}"
      if test x"$use_int_cuda_libs" = "xyes" ; then
        GPU_FLAGS+=" -cudalib=${with_cuda_int_libs}";
      fi
      ;;
    *pgfortran*)
      DEVXLIB_FLAGS+="--enable-openmp"
      GPU_FLAGS="-acc -acclibs -ta=tesla:cc${with_cuda_cc} "           # -gpu=cc${with_cuda_cc},cuda${with_cuda_runtime}"
      if test x"$use_int_cuda_libs" = "xyes" ; then
        GPU_FLAGS+=" -cudalib=${with_cuda_int_libs}";
      fi
      ;;
    *GNU* | *gnu*)
      GPU_FLAGS="-fopenacc -foffload=-lm  -foffload=-lgfortran" 
      # -foffload=nvptx-none
      # -foffload=amdgcn-amdhsa  -foffload=-march=gfx908 
      # -gpu=cc${with_cuda_cc},cuda${with_cuda_runtime}
   esac
   #
fi
#
# OpenMP5
#
if test x"$enable_openmp5" != "xno" ; then
   #
   # Flags to be passed to the devicexlib library
   #
   def_gpu="-D_GPU -D_OPENMP_GPU"
   GPU_FLAGS="-fopenmp" # -gpu=cc${with_cuda_cc},cuda${with_cuda_runtime}"
   DEVXLIB_FLAGS="--enable-openmp5" # --with-cuda-cc=${with_cuda_cc} --with-cuda-runtime=${with_cuda_runtime}"
   if test x"$LIBROCM_LIBS" != "x" ; then
     DEVXLIB_FLAGS+=" --enable-rocblas --with-rocm-libs=$LIBROCM_LIBS";
     def_gpu="$def_gpu -D_HIP"
   fi
   if test x"$MKLGPU_LIBS" != "x" ; then
     DEVXLIB_FLAGS+=" --enable-mkl-gpu" ;
     def_gpu="$def_gpu -D_MKLGPU"
     GPU_FLAGS="-qopenmp -fopenmp-targets=spir64"
   fi
   #
fi
#
AC_SUBST(with_cuda_cc)
AC_SUBST(with_cuda_runtime)
AC_SUBST(with_cuda_int_libs)
AC_SUBST(with_gpu_libs)
AC_SUBST(def_gpu)
AC_SUBST(GPU_FLAGS)
AC_SUBST(GPU_LIBS)
AC_SUBST(GPU_INCS)
AC_SUBST(DEVXLIB_FLAGS)
AC_SUBST(DEVXLIB_CUDALIBS)
AC_SUBST(LIBROCM_LIBS)
AC_SUBST(LIBROCM_INCS)
AC_SUBST(LIBROCM_PATH)
AC_SUBST(MKLGPU_LIBS)
#
])
