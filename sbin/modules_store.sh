#! /bin/bash
#
#        Copyright (C) 2000-2021 the YAMBO team
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
if [ $# = 0 ] ; then exit 0; fi
target=$1
dir=$2
#
# SAVE the modules relative to the current project (if any)
#
if test `find . -maxdepth 1 -name '__*' | wc -l` -ge 1 ; then
 for file in __*
 do
  if test `find . -maxdepth 1 -name '*.o' | wc -l` -ge 1 ; then
   echo "Saving modules in .modules$file";
   if test ! -d .modules$file; then mkdir .modules$file; fi
   for mod in *.o
   do
     if test `grep $mod project.dep | wc -l` -ge 1; then
       mv $mod .modules"$file"/
     fi
   done
  fi
  rm -f $file
 done
fi
#
touch "$target"
#
# If the TARGET modules dir exists just copy the modules from there
#
if test -d .modules$target; then
 if test `find .modules$target/ -name '*.mod' | wc -l` -ge 1 ; then
  echo "Loading modules from $target";
  mv .modules$target/*.mod $modir
 fi
fi
