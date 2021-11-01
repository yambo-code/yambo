#!/bin/bash
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
operate=$1
#
file_src=`echo $file | sed 's/.$/F/'`
full_path=`find $srcdir -name $file_src`
if [ -z "$full_path" ] ; then
 file_src=`echo $file | sed "s/.$/c/"`
 full_path=`find $srcdir -name $file_src`
 if [ -z "$full_path" ] ; then
   return
 fi
fi
#
file_obj=`echo $file | sed 's/.$/o/'`
full_path=`find $compdir -name $file_obj -not -path "/*.save/*"`
if [ -z "$full_path" ] ; then
  return
fi
#
ldir=`dirname $full_path`
llib=`basename $ldir`
#
source ./sbin/compilation/stamp_remove.sh "lib"
#
if [[ "$operate" == "remove" ]] && [[ -f "$full_path" ]]  ; then
  if [ "$VERB" == 1 ] ; then
   echo "rm -f  $full_path"
   echo "rm -f config/stamps_and_lists/lib${llib}.a.stamp"
  else
   rm -f $full_path
   rm -f config/stamps_and_lists/lib${llib}.a.stamp 
  fi
fi
if [[ "$operate" == "save" ]] && [[ -f "$full_path" ]]  ; then
  if [ "$VERB" == 1 ] ; then
   echo "mkdir -p $ldir/$save_dir"
   echo "mv $full_path $ldir/$save_dir"
  else
   mkdir -p $ldir/$save_dir
   mv $full_path $ldir/$save_dir
  fi
fi
if [[ "$operate" == "restore" ]] && [[ -f "$ldir/$restore_dir/$file" ]]  ; then
  if [ "$VERB" == 1 ] ; then
   echo " cp $ldir/$restore_dir/$file $ldir"
  else
   cp $ldir/$restore_dir/$file $ldir
  fi
fi 
