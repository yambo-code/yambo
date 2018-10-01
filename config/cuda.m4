#
#        Copyright (C) 2000-2016 the YAMBO team
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
AC_DEFUN([AC_HAVE_CUDA],[
#
AC_ARG_ENABLE(cuda,
        [AC_HELP_STRING([--enable-cuda=<opt>], [Enable CUDA support])],[],[])
#
AC_ARG_ENABLE(nvtx,
        [AC_HELP_STRING([--enable-nvtx=<path>], [Enable NVTX support])],[],[])
#
def_cuda=""
CUDA_FLAGS=""
CUDA_LIBS="-Mcudalib=cufft,cublas"

# Available cc options:
#    cc20            Compile for compute capability 2.0
#    cc30            Compile for compute capability 3.0
#    cc35            Compile for compute capability 3.5
#    cc50            Compile for compute capability 5.0
#    cc60            Compile for compute capability 6.0
#    cc70            Compile for compute capability 7.0
#
# check your card at https://en.wikipedia.org/wiki/CUDA#GPUs_supported
#
# cc20  for Fermi cards
# cc30 / cc35  for Kepler cards (eg K20, K40, K80)
# cc50  for Maxwell cards
# cc60  for Pascal cards (eg P100)
# cc70  for Volta  cards (eg V100)
#

if test x"$enable_cuda" = "xyes" ; then
   def_cuda="-D_CUDA"
   CUDA_FLAGS="-Mcuda=cuda9.0,cc70,nollvm $CUDA_LIBS"
elif ! test x"$enable_cuda" = "x" ; then
   def_cuda="-D_CUDA"
   CUDA_FLAGS="-Mcuda=$enable_cuda $CUDA_LIBS"
fi
#
if test x"$enable_cuda" = "x" -o x"$enable_cuda" = "xno" ; then
  enable_nvtx=no
fi
#
if test x"$enable_nvtx" = "xyes" ; then
   def_cuda="$def_cuda -D_NVTX"
   CUDA_FLAGS="$CUDA_FLAGS -lnvToolsExt"
elif ! test x"$enable_nvtx" = "x" ; then
   def_cuda="$def_cuda -D_NVTX"
   CUDA_FLAGS="$CUDA_FLAGS -L$enable_nvtx/lib64 -lnvToolsExt"
fi
#
AC_SUBST(def_cuda)
AC_SUBST(CUDA_FLAGS)
#
])
