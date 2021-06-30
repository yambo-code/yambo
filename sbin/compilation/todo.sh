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
if ! test -f  $compdir/config/stamps_and_lists/active_directories.list; then
 exit 0
fi
#
dirs_to_check=`cat $compdir/config/stamps_and_lists/active_directories.list`
#
for dir in $dirs_to_check
 do
 if [[ ! "$dir" == *"$target"* ]]; then
  continue
 fi
 if test -f $compdir/config/stamps_and_lists/$target.stamp; then
  candidates=`find $dir -type f  \( ! -iname ".o" \) -newer $compdir/config/stamps_and_lists/$target.stamp`
 fi
 if test -f $compdir/config/stamps_and_lists/lib${target}.a.stamp; then
  candidates=`find $dir -type f \( ! -iname ".o" \) -newer $compdir/config/stamps_and_lists/lib${target}.a.stamp`
 fi
 for file in $candidates
 do
   file=`basename $file`
   obj=`echo $file | sed "s/.o/.X/"`
   obj=`echo $file | sed "s/.F/.o/" |  sed "s/.c/.o/" |  sed "s/.f/.o/"`
   if grep -q "$obj" $dir/.objects; then
    bdir=`basename $dir`
    if test -f config/stamps_and_lists/lib${bdir}.a.stamp; then rm -f config/stamps_and_lists/lib${bdir}.a.stamp; fi
   fi
 done
done
#

