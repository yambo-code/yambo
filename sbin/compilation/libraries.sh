#!/bin/bash
#
#        Copyright (C) 2000-2023 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM
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
libs=" "
for arg in $ARGS 
do
 case $arg in
  -l*)
   libs="$arg $libs"
   ;;
 esac
done
#
llocal="-lqe_pseudo -lmath77 -lslatec -llocal"
lPLA="\$(lscalapack) \$(lblacs) \$(llapack) \$(lblas)"
lSL="\$(lslepc) \$(lpetsc)"
lIO="\$(liotk) \$(lpnetcdf) \$(lnetcdff) \$(lnetcdf) \$(lhdf5)"
lextlibs="\$(llibxc) \$(lfft) \$(lfutile) \$(lyaml)"
#
case $target in
  yambo_nl)
   libs="$libs $llocal $lPLA $lIO $lextlibs -lm"
    ;;
  yambo*)
   libs="$libs $llocal $lSL $lPLA $lIO $lextlibs -lm"
    ;;
  a2y|elk2y|c2y)
   libs="-lint_modules $libs $llocal $lPLA $lIO $lextlibs -lm"
    ;;
  p2y*)
   libs="-lint_modules $libs $llocal $lPLA $lIO $lextlibs -lm"
    ;;
  e2y)
   libs="-lint_modules $libs $llocal $lPLA $lIO $lextlibs -lm"
    ;;
  ypp*)
   libs="$libs $llocal $lPLA $lIO $lextlibs -lm"
    ;;
  lib*)
    ;;
esac
#
libs="-L\$(libdir) $libs"
#
