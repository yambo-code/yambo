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
 file_with_path_obj=`find $compdir -name $file_obj -not -path "/*objects.save/*"`;
else
 if [ "$VERB" == 1 ] ; then
   echo "WARNING driver.o case is problematic. "
   echo "Mode is $operate. Skipping it."
   echo "driver.o is created at the linking step. "
   echo "See config/mk/local/functions.mk, define_link"
 fi
 return
fi
#
if [ -z "$file_with_path_obj" ] ; then
  if [ "$operate" == "remove_child" ]; then return ; fi
  # if the object does not exist yet, I set the path from the external loop
  if [ "$operate" == "remove" ]; then
    if [ "$VERB" == 1 ] ; then
      echo "Remove mode and $file_obj not found"
      echo "Setting to external value"
    fi
    file_with_path_obj=$compdir/$dir/$file_obj
  fi
fi
#
path_obj=`dirname $file_with_path_obj`
#
file_src=`echo $file | sed 's/.$/F/'`
file_with_path_src=`echo $file_with_path_obj | sed 's/.$/F/'`
if [[ "$compdir" != "$srcdir" ]] && [[ "$srcdir" != "." ]] ; then
 # replace compdir with srcdir in file_with_path_src
 file_with_path_src=${srcdir}${file_with_path_src/$compdir/}
fi
if [ ! -f "$file_with_path_src" ] ; then
 file_src=`echo $file | sed "s/.$/c/"`
 file_with_path_src=`echo $file_with_path_src | sed 's/.$/c/'`
 file_f90=""
 file_with_path_f90=""
else
 file_f90=`echo $file | sed 's/.$/f90/'`
 file_with_path_f90=`echo $file_with_path_obj | sed 's/.$/f90/'`
fi
if [ ! -f "$file_with_path_src" ] ; then
 file_src=`echo $file | sed "s/.$/f/"`
 file_with_path_src=`echo $file_with_path_src | sed 's/.$/f/'`
fi
#
if [ ! -f "$file_with_path_src" ] ; then
 echo "$file_src not found for src in $operate mode in $srcdir"
 echo "full path was set to $file_with_path_src"
fi
#
ldir=`dirname $file_with_path_obj`
llib=`basename $ldir`
#
if [ "$VERB" == 1 ] ; then
 echo "source file name is $file_src"
 echo "source file with path is $file_with_path_src"
 echo "f90 file name is $file_f90"
 echo "f90 file with path is $file_with_path_f90"
 echo "obj file name is $file_obj"
 echo "obj file with path is $file_with_path_obj"
 echo "obj path is $path_obj"
 echo "dir is $dir"
 echo "ldir is $ldir"
 #echo "operate is $operate"
fi
#
if [[  "$path_obj" == *"${dir/\./}" ]] || [ "$2" == "locks" ]; then
 if [ $VERB == 1 ] ; then echo "remove lib | rm -f config/stamps_and_lists/lib${llib}.a.stamp" ; fi
 source ./sbin/compilation/stamp_remove.sh "lib"
fi
#
if [[ -f "$file_with_path_obj" ]]  ; then
  #
  if [ "$VERB" == 1 ] ; then
    if [[  "$path_obj" == *"${dir/\./}" ]]; then echo "remove object | rm -f  $file_with_path_obj"; fi
    echo "remove f90 source | rm -f  $file_with_path_f90"
  fi
  if [[  "$path_obj" == *"${dir/\./}" ]]; then rm $file_with_path_obj ; fi
  if [ -f "$file_with_path_f90" ] ; then rm -f $file_with_path_f90 ; fi
  #
  if [ "$2" == "locks" ] && [[  ! "$path_obj" == *"${dir/\./}" ]]; then
    if [ "$VERB" == 1 ] ; then echo "remove object | mv  $file_with_path_obj ${file_with_path_obj}_to_remove "; fi
    mv  $file_with_path_obj ${file_with_path_obj}_to_remove
  fi
  #
  # In sources mode remove all corresponding saved objects
  #
  if [ "$2" == "sources" ]; then
    #
    if [ "$VERB" == 1 ] ; then echo "touch file source | touch  $file_with_path_src" ; fi
    if [ -f "$file_with_path_src" ] ; then touch $file_with_path_src ; fi
    #
    if [ "$VERB" == 1 ] ; then
     echo "remove stored objects in .save folders | rm $path_obj/*objects.save/$file_obj"
     echo "remove .f90 files in .save folders | $path_obj/*objects.save/$file_f90"
     echo "remove .a libraries in | rm $path_obj/*objects.save/*.a"
    fi
    #
    count=`ls -1 $path_obj/*objects.save/*.a 2>/dev/null | wc -l`
    if [ $count != 0 ]; then rm -f $path_obj/*objects.save/*.a ; fi
    count=`ls -1 $path_obj/*objects.save/$file_obj 2>/dev/null | wc -l`
    if [ $count != 0 ] ; then
      if [ "$VERB" == 1 ] ; then echo "rm -f $path_obj/*objects.save/$file_obj" ; fi
      rm -f $path_obj/*objects.save/$file_obj
    fi
    if [ ! "$file_f90" == "" ]; then
      count=`ls -1 $path_obj/*objects.save/$file_f90 2>/dev/null | wc -l`
      if [ $count != 0 ] ; then
        if [ "$VERB" == 1 ] ; then echo "rm -f $path_obj/*objects.save/$file_f90" ; fi
        rm -f $path_obj/*objects.save/$file_f90
      fi
    fi
  fi
fi
#
# Remove corresponding mod files
#
if [[ -f "$file_with_path_src" ]]  ; then
  modfile=`grep -i module $file_with_path_src | grep -i -v end | grep -i -v use | grep -i -v procedure | grep -v !`
  for modname_tmp in $modfile; do
    # Global
    modname=`echo "$modname_tmp" | tr '[:upper:]' '[:lower:]'`
    if [ -f "$compdir/include/$modname.mod" ]; then
      if [ "$VERB" == 1 ] ; then echo "remove global module | rm -f $compdir/include/$modname.mod"; fi
      rm -f $compdir/include/$modname.mod
    fi
    # Local
    if [ -f "$path_obj/$modname.mod" ]; then
      #
      if [[  "$path_obj" == *"${dir/\./}" ]]; then
        if [ "$VERB" == 1 ]; then echo "remove local module | rm -f $path_obj/$modname.mod"; fi
        rm -f $path_obj/$modname.mod ;
      fi
      #
      if [ "$2" == "locks" ] && [[  ! "$path_obj" == *"${dir/\./}" ]]; then
        if [ "$VERB" == 1 ] ; then echo "remove local module | mv $path_obj/$modname.mod $path_obj/$modname.mod_to_remove "; fi
        mv $path_obj/$modname.mod $path_obj/$modname.mod_to_remove
      fi
      #
      # In sources mode remove all corresponding saved modules
      #
      if [ "$2" == "sources" ]; then
        count=`ls -1 $path_obj/*objects.save/$modname.mod 2>/dev/null | wc -l`
        if [ $count != 0 ]; then
          if [ "$VERB" == 1 ] ; then echo "remove module in .save folders | rm $path_obj/*objects.save/$modname.mod" ; fi
          rm $path_obj/*objects.save/$modname.mod
        fi
      fi
    fi
  done
  #
fi
