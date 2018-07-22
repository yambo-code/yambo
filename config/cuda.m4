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
def_cuda="-D_CUDA"
CUDA_LIBS="-Mcudalib=cufft,cublas"

#
# cc35  for Kepler cards (eg K20, K40, K80)
# cc60  for Pascal cards (eg P100)
# cc70  for Volta  cards (eg V100)
#
if test x"$enable_cuda" = "xyes" ; then
   CUDA_FLAGS="-Mcuda=cuda8.0,cc35,nollvm $CUDA_LIBS"
elif ! test x"$enable_cuda" = "x" ; then
   CUDA_FLAGS="-Mcuda=$enable_cuda $CUDA_LIBS"
else
   def_cuda=
   CUDA_FLAGS=
fi
#
AC_SUBST(def_cuda)
AC_SUBST(CUDA_FLAGS)
#
])
