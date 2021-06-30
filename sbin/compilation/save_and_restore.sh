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
lock_files=`find $cdir -name '*.lock'`
sorted_locks=$(echo "$lock_files"|tr " " "\n"|sort|uniq|tr "\n" " ")
sorted_precomps=$(echo "$precomp_flags"|tr " " "\n"|sort|uniq|tr "\n" " ")
#
# Directory where to save the objects
#
for lock in $sorted_locks
do
 lock=`echo $lock | sed "s/.lock//"`
 lock=`basename $lock`
 save_dir="${lock}_${save_dir}"
done
save_dir="${save_dir}.save"
#
# Directory to be restored
#
for flag in $sorted_precomps
do
 flag=`echo $flag | sed "s/\-D_//"`
 restore_dir="${flag}_${restore_dir}"
done
restore_dir="${restore_dir}.save"
#
# Search for objects dependent of PROJECTS not among the current ones
# and move them in a dedicated folder
#
for lock in $sorted_locks
do
 lock_file=$lock
 lock=`echo $lock | sed "s/.lock//"`
 lock=`basename $lock`
 needed="no"
 for flag in $sorted_precomps
 do
   flag=`echo $flag | sed "s/\-D_//"`
   if [ "$lock" == "$flag" ] ; then
     needed="yes"
   fi
 done
 if [ "$needed" == "no" ] ; then
  if ! test -d $save_dir; then mkdir -p $cdir/$save_dir; fi
  rm -f $lock_file
  if test -f "$cdir/${lock}_project.dep"; then
   deps=`cat $cdir/${lock}_project.dep`
   for object in $deps
   do
     if test -f "$cdir/$object"; then
      mv $cdir/$object $cdir/$save_dir
     fi
   done
  fi
 fi
done
#
# Restore the files of the project (if there are)
#
if test -d "$cdir/$restore_dir"; then
 for file in `ls $cdir/$restore_dir`
 do
  mv $cdir/$restore_dir/$file $cdir
 done
fi
