## Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.
##
## $Id: fcflags.m4 5257 2009-04-17 12:56:44Z marques $
##
################################################
# Get default FFLAGS
# ----------------------------------
# this function can certainly be improved on
AC_DEFUN([ACX_FCFLAGS],
[
AC_REQUIRE([AC_CANONICAL_HOST])

if test -z "${FCFLAGS}"; then
  case "${FC}" in
    gfortran*)
      FCFLAGS="-pipe -O3 -funroll-loops -ffast-math -ffree-line-length-none"
      ;;
    openf9*)
      FCFLAGS="-O3 -funroll-loops -ffast-math"
      ;;
    g95*)
      FCFLAGS="-pipe -O3 -funroll-loops -ffast-math"
      ;;
    efc*|ifc*|ifort*)
      case "${host}" in
        x86_64*)
          FCFLAGS="-u -fpp1 -nbs -pc80 -pad -align -unroll -O3 -ip -no-fp-port -mno-ieee-fp -vec-report0 -no-prec-div"
          ;;
        i?86*linux*)
          FCFLAGS="-u -fpp1 -nbs -pc80 -pad -align -unroll -O3 -ip -no-fp-port -mno-ieee-fp -vec-report0 -no-prec-div"
          a=`echo $host | sed "s/^i//" | sed "s/86.*//"`
          if test "$a" -gt 5 ; then
            FCFLAGS="$FCFLAGS -tpp7 -xW"
          fi
          ;;	
        ia64*)
          FCFLAGS="-O3 -ip -IPF_fp_relaxed -ftz -fpp -u -align all -pad"
         ;;
      esac
      ;;
    sun*)
      case "${host}" in
        i?86*linux*|x86_64*)
          FCFLAGS="-fast -xprefetch -xvector=simd"
          ;;
        sparc*)
          FCFLAGS="-fast"
          ;;
      esac
      ;;
    pathf9*)
      FCFLAGS="-O3 -march=auto -mcpu=auto -OPT:Ofast -fno-math-errno -LNO:simd=2 -OPT:align_unsafe=ON"
      ;;
    pgf90*)
      FCFLAGS="-O4 -fast -Munroll -Mnoframe -Mdalign"
      ;;
    abf90*)
      FCFLAGS="-O3 -YEXT_NAMES=LCS -YEXT_SFX=_"
      ;;
    xlf*)
      FCFLAGS="-O3 -qarch=auto -qtune=auto -qcache=auto -qxlf90=autodealloc"
      ;;
    f9*)
      case "${host}" in
        alphaev*)
          FCFLAGS="-align dcommons -fast -tune host -arch host -noautomatic"
          ;;
        mips*)
          FCFLAGS="-Ofast -O3"
          ;;
        sparc*)
          FCFLAGS="-fast"
          ;;
        *)
          FCFLAGS="-O3"
          ;;
      esac
      ;;
    *)
       FCFLAGS="-O3"
       ;;
  esac
fi
AC_MSG_NOTICE([Using FCFLAGS="$FCFLAGS"])
])
