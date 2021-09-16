#!/bin/bash
#
#        Copyright (C) 2000-2021 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): HM AM
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
if test -f $compdir/config/stamps_and_lists/$goal.stamp; then
 candidates=`find $srcdir//$dir -type f  \( ! -iname ".o" \) -newer $compdir/config/stamps_and_lists/$goal.stamp`
fi
if test -f $compdir/config/stamps_and_lists/${target}.a.stamp; then
 candidates+=`find $srcdir//$dir -type f \( ! -iname ".o" \) -newer $compdir/config/stamps_and_lists/${target}.a.stamp`
fi
for file in $candidates
do
  file=`basename $file`
  obj=`echo $file | sed "s/\.o/\.X/"`
  obj=`echo $file | sed "s/\.F/\.o/" |  sed "s/\.c/\.o/" |  sed "s/\.f/\.o/"`
  if grep -q "$obj" $dir/.objects; then
   bdir=`basename $dir`
   rm -f config/stamps_and_lists/${goal}.stamp 
   rm -f config/stamps_and_lists/${target}.a.stamp 
   #echo "$file is NEW"
   #echo "rm -f config/stamps_and_lists/${goal}.stamp"
   #echo "rm -f config/stamps_and_lists/${target}.a.stamp"
  fi
  if grep -q "$obj" $compdir/config/stamps_and_lists/global_modules_dep.list; then
   mods1=`grep -w $obj $compdir/config/stamps_and_lists/global_modules_dep.list | awk '{print $1}'`
   for mod1 in $mods1
   do
    if test "$mod1" == "$obj"; then continue; fi
    first_level_dep+=" $mod1"
    mods2=`grep -w $mod1 $compdir/config/stamps_and_lists/global_modules_dep.list | awk '{print $1}'`
    for mod2 in $mods2
    do
     if test "$mod2" == "$mod1"; then continue; fi
     second_level_dep+=" $mod2"
     mods3=`grep -w $mod2 $compdir/config/stamps_and_lists/global_modules_dep.list | awk '{print $1}'`
     for mod3 in $mods3
     do
      if test "$mod3" == "$mod2"; then continue; fi
      third_level_dep+=" $mod3"
     done
    done
   done
   all_deps="$first_level_dep $second_level_dep $third_level_dep"
   for file in $all_deps
   do
    full_path=`find $compdir -name $file`
    if test -f "$full_path"; then
     ldir=`dirname $full_path`
     llib=`basename $ldir`
     rm -f $full_path
     rm -f config/stamps_and_lists/lib${llib}.a.stamp 
     #echo "rm -f  $full_path"
     #echo "rm -f config/stamps_and_lists/lib${llib}.a.stamp"
    fi 
   done
  fi
done
