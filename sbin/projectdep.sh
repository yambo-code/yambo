#!/bin/bash
#
#        Copyright (C) 2000-2021 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): HM
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
# moduledep.sh -- script that computes dependencies on Fortran 90 modules
# modified from the moduledep.sh distributed with Quantum ESPRESSO
#
sources=" "
if test `find $1 -maxdepth 1 -name '*.F' | wc -l` -ge 1 ; then
 sources+=`echo $1/*.F`
fi
sources+=" " 
if test `find $1 -maxdepth 1 -name '*.c' | wc -l` -ge 1 ; then
 sources+=`echo $1/*.c`
fi
pj=`echo $2`
for file in $sources
do
 if test `grep $pj $file | grep '#'| wc -l` -ge 1; then
   obj=`echo $file| sed 's/\.F/\.o/g'| sed 's/\.c/\.o/g'`
   sources_pj_dependent+=" ${obj} $pj"
 fi
done
echo $sources_pj_dependent >>  $1/project.dep
