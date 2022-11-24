#!/bin/bash
#
#        Copyright (C) 2000-2022 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM DS
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
file_obj=`echo $file | sed 's/.$/o/'`
if [ "$file_obj" != "driver.o" ] ; then
 obj_path=`find $compdir -name $file_obj -not -path "/*.save/*"`;
fi
#
if [ "$file_obj" == "driver.o" ] ; then
 if [ "$VERB" == 1 ] ; then
   echo "WARNING driver.o case is problematic. "
   echo "Mode is $operate. Skipping it."
   echo "driver.o is created at the linking step. "
   echo "See config/mk/local/functions.mk, define_link"
 fi
 return
 #echo "Setting to external value $obj_path"
fi
#
if [ -z "$obj_path" ] ; then
  if [[ "$operate" == "remove_child"* ]]; then return ; fi
  # if the object does not exist yet, I set the path from the external loop
  if [ "$operate" == "remove" ]; then
    if [ "$VERB" == 1 ] ; then
      echo "Remove mode and $file_obj not found"
      echo "Setting to external value"
    fi
    obj_path=$compdir/$dir/$file_obj
  fi
fi
#
file_src=`echo $file | sed 's/.$/F/'`
source_path=`echo $obj_path | sed 's/.$/F/'`
if [[ "$compdir" != "$srcdir" ]] && [[ "$srcdir" != "." ]] ; then
 # replace compdir with srcdir in source_path
 source_path=${srcdir}${source_path/$compdir/}
fi
if [ ! -f "$source_path" ] ; then
 file_src=`echo $file | sed "s/.$/c/"`
 source_path=`echo $source_path | sed 's/.$/c/'`
 f90_file_src=""
 f90_source_path=""
else
 f90_file_src=`echo $file | sed 's/.$/f90/'`
 f90_source_path=`echo $obj_path | sed 's/.$/f90/'`
fi
if [ ! -f "$source_path" ] ; then
 file_src=`echo $file | sed "s/.$/f/"`
 source_path=`echo $source_path | sed 's/.$/f/'`
fi
#
if [ ! -f "$source_path" ] ; then
 echo "$file_src not found for src in $operate mode in $srcdir"
 echo "full path was set to $source_path"
fi
#
ldir=`dirname $obj_path`
llib=`basename $ldir`
#
if [ "$VERB" == 1 ] ; then
 echo "source path is $source_path"
 echo "f90 source path is $f90_source_path"
 echo "obj path is $obj_path"
 echo "dir is $dir"
 echo "ldir is $ldir"
fi
#
  if [ $VERB == 1 ] ; then echo "[WARNING] removing lib stamp" ; fi
  source ./sbin/compilation/stamp_remove.sh "lib"
  if [[ -f "$obj_path" ]]  ; then
    if [ "$VERB" == 1 ] ; then
     echo "remove object | rm -f  $obj_path"
     echo "remove f90 source | rm -f  $f90_source_path"
     echo "remove lib | rm -f config/stamps_and_lists/lib${llib}.a.stamp"
    fi
    rm $obj_path
    if [ -f "$f90_source_path" ] ; then rm -f $f90_source_path ; fi
  fi
#
