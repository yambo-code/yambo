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
   echo "Mode is $operate. File $file. Skipping it."
   echo "The file is created at the linking step. See config/mk/local/functions.mk, define_link"
 fi
 return
 #echo "Setting to external value $obj_path"
fi
#
if [ -z "$obj_path" ] ; then
  if [ "$operate" == "remove_child" ]; then return ; fi
  # if the onject does not exist yet I set the path from the external loop
  # then I cannot be in the case "remove" or "restore"
  if [ "$operate" == "remove" ]; then
    if [ "$VERB" == 1 ] ; then
      echo "Remove mode and $file_obj not found"
      echo "Setting to external value"
    fi
    obj_path=$srcdir/$dir/$file_obj
  fi
  if [ "$operate" == "save" ]; then
    if [ "$VERB" == 1 ] ; then
      echo "Save mode and $file_obj not found"
      echo "Setting to external value"
    fi
    obj_path=$srcdir/$dir/$file_obj
  fi
  if [ "$operate" == "restore" ]; then
    if [ "$VERB" == 1 ] ; then
      echo "Restore mode and $file_obj not found"
      echo "Setting to external value"
    fi
    obj_path=$srcdir/$dir/$file_obj
  fi
fi
#
file_src=`echo $file | sed 's/.$/F/'`
source_path=`echo $obj_path | sed 's/.$/F/'`
if [ ! -f "$source_path" ] ; then
 file_src=`echo $file | sed "s/.$/c/"`
 source_path=`echo $obj_path | sed 's/.$/c/'`
 f90_file_src=""
 f90_source_path=""
else
 f90_file_src=`echo $file | sed 's/.$/f90/'`
 f90_source_path=`echo $obj_path | sed 's/.$/f90/'`
fi
if [ ! -f "$source_path" ] ; then
 file_src=`echo $file | sed "s/.$/f/"`
 source_path=`echo $obj_path | sed 's/.$/f/'`
fi
#
if [ ! -f "$source_path" ] ; then
 echo "********* ERROR ************"
 echo "$file_src not found for src in $operate mode in $srcdir"
 echo "full path was set to $source_path"
 #exit()
fi
#
ldir=`dirname $obj_path`
llib=`basename $ldir`
library="lib${llib}.a"
if [[ "$ldir" == *"ypp"* ]] ; then  library="lib${llib}.a" ; fi
if [[ "$ldir" == *"yambo/driver"* ]] ; then
 Ydrfolder=`basename $ldir`
 #Still not working since goal is not defined
 library="lib${goal}_Ydriver_${Ydrfolder}.a"
 #echo "library $library"
fi
if [[ "$goal" == "$target" ]] ; then
 if [ "$VERB" == 1 ] ; then echo "here I'm doing the driver folder, but there is no connected library"; fi
 library=""
fi
#
fstamp=$compdir/config/stamps_and_lists/mods_${llib}_restored.stamp
rstamp=$compdir/config/stamps_and_lists/mods_${llib}_saved_removed.stamp
lstamp=$compdir/config/stamps_and_lists/lib_${llib}_restored.stamp
sstamp=$compdir/config/stamps_and_lists/lib_${llib}_saved_removed.stamp
#
if [ "$VERB" == 1 ] ; then
 echo "source path is $source_path"
 echo "f90 source path is $f90_source_path"
 echo "obj path is $obj_path"
 echo "library is $library"
 echo "dir is $dir"
 echo "ldir is $ldir"
fi
#
if [[ "$operate" == *"remove"* ]] ; then
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
   echo "saving $obj_path to $save_dir"
   echo "saving $f90_source_path to $save_dir"
   echo "mv $obj_path $ldir/$save_dir"
  fi
  if [ "$DRY_RUN" == 0 ] ; then
   # save of library
   if [ -f $compdir/lib/$library ] ; then
    rm -f $lstamp 
    if [   -f $ldir/$save_dir/$library ] ; then rm $compdir/lib/$library ; fi
    if [ ! -f $ldir/$save_dir/$library ] ; then mv $compdir/lib/$library $ldir/$save_dir ; fi
   fi
   # save of objects
   if [   -f $ldir/$save_dir/$file_obj ] ; then  rm $obj_path ; fi
   if [ ! -f $ldir/$save_dir/$file_obj ] ; then  mv $obj_path $ldir/$save_dir ; fi
   back_dir=$PWD
   if [ -f "$f90_source_path" ] ; then 
    if [   -f $ldir/$save_dir/$f90_file_src ] ; then rm $f90_source_path ; fi
    if [ ! -f $ldir/$save_dir/$f90_file_src ] ; then mv $f90_source_path $ldir/$save_dir ; fi
   fi
   # save of modules
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
     # objects
     cp $ldir/$restore_dir/$file_obj $ldir
     #if [ -f "$ldir/$file_obj" ] && [ ! -f ] ; then source ./sbin/compilation/check_object_childs.sh "restore" ; fi
     #if [ -f $compdir/stamps_and_lists/check_object_loop.stamp] ; then rm $compdir/stamps_and_lists/check_object_loop.stamp ; fi
     if [   -f "$ldir/$restore_dir/$f90_file_src" ] ; then cp $ldir/$restore_dir/$f90_file_src $ldir ; fi
     # library
     if [ ! -f $lstamp ] && [ -f $ldir/$restore_dir/$library ] ; then
       cp $ldir/$restore_dir/$library $compdir/lib/
       touch $lstamp
     fi 
     # modules
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
     source ./sbin/compilation/check_object_childs.sh "remove" ;
     #source ./sbin/compilation/stamp_remove.sh "lib"
    fi
  fi
fi 
