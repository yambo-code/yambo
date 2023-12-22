#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2006 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
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
lIO="\$(liotk) \$(letsf) \$(lpnetcdf) \$(lnetcdff) \$(lnetcdf) \$(lhdf5)"
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
