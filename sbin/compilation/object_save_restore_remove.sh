#!/bin/bash
#
#        Copyright (C) 2000-2022 the YAMBO team
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
source_path=`find $srcdir -name $file_src`
if [ -z "$source_path" ] ; then
 file_src=`echo $file | sed "s/.$/c/"`
 source_path=`find $srcdir -name $file_src`
 f90_file_src=""
 f90_source_path=""
else
 f90_file_src=`echo $file | sed 's/.$/f90/'`
 f90_source_path=`find $compdir -name $f90_file_src`
fi
if [ -z "$source_path" ] ; then
 file_src=`echo $file | sed "s/.$/f/"`
 source_path=`find $srcdir -name $file_src`
fi
if [ -z "$source_path" ] ; then
 if [ "$VERB" == 1 ] ; then
  echo "$file_src not found for src in $operate mode in $srcdir"
  echo "full path was set to $source_path $PWD"
 fi
 return
fi
#
file_obj=`echo $file | sed 's/.$/o/'`
obj_path=`echo $source_path | sed 's/.$/o/'`
if [ -z "$obj_path" ] ; then
  if [ "$VERB" == 1 ] ; then
    echo "$file_src not found for obj in $operate mode in $srcdir"
    echo "full path was set to $obj_path"
  fi
 return
fi
#
ldir=`dirname $obj_path`
llib=`basename $ldir`
fstamp=$compdir/config/stamps_and_lists/mods_${llib}_restored.stamp
rstamp=$compdir/config/stamps_and_lists/mods_${llib}_removed.stamp

if [ "$VERB" == 1 ] ; then
 echo "source path is $source_path"
 echo "obj path is $obj_path"
fi
#
if [[ "$operate" == "remove" ]] ; then
  if [ $VERB == 1 ] ; then echo "[WARNING] removing lib stamp" ; fi
  source ./sbin/compilation/stamp_remove.sh "lib"
  if [[ -f "$obj_path" ]]  ; then
    if [ "$VERB" == 1 ] ; then
     echo "remove object | rm -f  $obj_path"
     echo "remove f90 source | rm -f  $f90_source_path"
     echo "remove lib | rm -f config/stamps_and_lists/lib${llib}.a.stamp"
    fi
    if [ "$DRY_RUN" == 0 ] ; then
     rm -f $obj_path
     if [ -f "$f90_source_path" ] ; then rm -f $f90_source_path ; fi
     rm -f config/stamps_and_lists/lib${llib}.a.stamp 
    fi
  fi
fi
#
if [[ "$operate" == "save" ]] ; then
 if [[ -f "$obj_path" ]] ; then
  count_loc=`ls -1 $ldir/*.mod 2>/dev/null | wc -l`
  count_sav=`ls -1 $ldir/$save_dir/*.mod 2>/dev/null | wc -l`
  if [ "$VERB" == 1 ] ; then
   echo "mkdir -p $ldir/$save_dir"
   echo "saving $obj_path to $save_dir"
   echo "saving $f90_source_path to $save_dir"
   echo "mv $obj_path $ldir/$save_dir"
  fi
  if [ "$DRY_RUN" == 0 ] ; then
   mkdir -p $ldir/$save_dir
   if [   -f $ldir/$save_dir/$file_obj ] ; then  rm $obj_path ; fi
   if [ ! -f $ldir/$save_dir/$file_obj ] ; then  mv $obj_path $ldir/$save_dir ; fi
   back_dir=$PWD
   if [ -f "$f90_source_path" ] ; then 
    if [   -f $ldir/$save_dir/$f90_file_src ] ; then rm $f90_source_path ; fi
    if [ ! -f $ldir/$save_dir/$f90_file_src ] ; then mv $f90_source_path $ldir/$save_dir ; fi
   fi
   if [ "$count_loc" -gt "0" ] ; then
     rm -f $fstamp ;
     cd $ldir ;
     for mod_file in *.mod ; do
       mv $mod_file $save_dir ;
       if [ "$VERB" == 1 ] ; then echo "removing $mod_file found in $ldir from $compdir/include"; fi
       rm $compdir/include/$mod_file ;
     done
     cd $back_dir
     touch $rstamp ;
   fi
   if [ "$count_sav" -gt "0" ] && [ "$count_loc" == 0 ] && [ ! -f $rstamp ] ; then
     rm  -f $fstamp 
     cd $ldir/$save_dir/
     for mod_file in *.mod ; do
      if [ -f $compdir/include/$mod_file ] ; then
        if [ "$VERB" == 1 ] ; then echo "removing $mod_file found in $ldir/$save_dir from $compdir/include" ; fi
        rm $compdir/include/$mod_file ;
      fi
     done
     cd $back_dir
     touch $rstamp ;
   fi
  fi
 else
  if [ "$VERB" == 1 ] ; then echo "save mode | $obj_path not found"; fi
 fi
fi
#
if [[ "$operate" == "restore" ]] ; then
  if [[ -f "$ldir/$restore_dir/$file_obj" ]]  ; then
    count_sav=`ls -1 $ldir/$restore_dir/*.mod 2>/dev/null | wc -l`
    if [ "$VERB" == 1    ] ; then
      echo " cp $ldir/$restore_dir/$file_obj $ldir";
      if [ -f "$ldir/$restore_dir/$f90_file_src" ] ; then echo "cp $ldir/$restore_dir/$f90_file_src $ldir" ; fi
    fi
    if [ "$DRY_RUN" == 0 ] ; then
     cp $ldir/$restore_dir/$file_obj $ldir
     if [   -f "$ldir/$restore_dir/$f90_file_src" ] ; then cp $ldir/$restore_dir/$f90_file_src $ldir ; fi
     if [ ! -f $fstamp ] && [ "$count_sav" -gt "0" ] ; then
      #rm $ldir/*.mod ; 
      #cp $ldir/$restore_dir/*.mod $ldir ;
      cp $ldir/$restore_dir/*.mod $compdir/include/ ;
      rm $rstamp ; touch $fstamp  ;
     fi
    fi
  else
    if [ "$VERB" == 1    ] ; then
     echo "missing $file_obj into $restore_dir. Setting for compilation" ;
     echo "remove object if exists | rm $ldir/$file_obj"
    fi
    if [ "$DRY_RUN" == 0 ] ; then
     DIR_is_to_recompile=1
     rm -f $rstamp
     obj=$file_obj
     if [ -f "$ldir/$file_obj" ] ; then source ./sbin/compilation/check_object_childs.sh; fi
     #if [ -f $ldir/$file_obj ] ; then  rm $ldir/$file_obj; fi
     #source ./sbin/compilation/stamp_remove.sh "lib"
    fi
  fi
fi 
