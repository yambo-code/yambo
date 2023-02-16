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
path_back=$PWD
#
#defined library name
#
library="${target}.a"
if [[ "$goal" == "$target" ]] ; then
 if [ "$VERB" == 1 ] ; then
   echo "goal= $goal, target= $target";
   echo "here I'm doing the driver folder, but there is no connected library";
 fi
 library="NONE"
fi
if [ "$VERB" == 1 ] ; then echo "library is $library" ; fi
#
# Save files
#
if [ ! -f $dir/$save_dir/$library ] && [[ ! $dir == *"yambo/Ydriver"* ]] ; then
 if [ "$VERB" == 1 ] ; then echo "Saving files " ; fi
 if [ ! -d $dir/$save_dir ] ; then
  if [ "$VERB" == 1 ] ; then echo "mkdir -p $dir/$save_dir" ; fi
  mkdir -p $dir/$save_dir
 fi
 deps_rm=""
 count_obj_tr=`ls -1 $dir/*.o_to_remove 2>/dev/null | wc -l`
 count_mod_tr=`ls -1 $dir/*.mod_to_remove 2>/dev/null | wc -l`
 if [ $count_obj_tr != 0 ] || [ $count_mod_tr != 0 ] ; then
  if [ ! -f $dir/$save_dir/files.dep ] ; then
    cd $dir
    for file in *.o_to_remove; do
      [ -f "$file" ] || break
      deps_rm="$deps_rm ${file/_to_remove/}"
    done
    for mod in *.mod_to_remove; do
      [ -f "$mod" ] || break
      deps_rm="$deps_rm ${mod/_to_remove/}"
    done
    cd $path_back
  fi
  if [ $count_obj_tr != 0 ]; then rm $dir/*.o_to_remove ; fi
  if [ $count_mod_tr != 0 ]; then rm $dir/*.mod_to_remove ; fi
 fi
 count_obj=`ls -1 $dir/*.o 2>/dev/null | wc -l`
 count_mod=`ls -1 $dir/*.mod 2>/dev/null | wc -l`
 count_f90=`ls -1 $dir/*.f90 2>/dev/null | wc -l`
 if [ $count_obj != 0 ] || [ $count_mod != 0 ] || [ $count_obj_tr != 0 ] || [ $count_mod_tr != 0 ]  ; then
  if [ ! -f $dir/$save_dir/files.dep ] ; then
   cd $dir;
   deps=`ls *.o *.mod 2>/dev/null`;
   deps=$(echo $deps $deps_rm |tr " " "\n"|sort|uniq|tr "\n" " ")
   for file in $deps; do echo " $file" >> "$save_dir/files.dep"; done
   cd $path_back ;
   if [ $count_obj != 0 ] ; then cp $dir/*.o    $dir/$save_dir/ ; fi
   if [ $count_mod != 0 ] ; then cp $dir/*.mod  $dir/$save_dir/ ; fi
   if [ $count_f90 != 0 ] ; then cp $dir/*.f90  $dir/$save_dir/ ; fi
  else
   deps=`cat $dir/$save_dir/files.dep`
   cd $dir/$save_dir
   found=`ls *.o *.mod 2>/dev/null`
   cd $path_back
   missing_files=`comm -23 <(tr ' ' $'\n' <<< $deps | sort| uniq) <(tr ' ' $'\n' <<< $found | sort | uniq)`
   for file in $missing_files ; do
    if [ -f $dir/$file ] ; then cp $dir/$file $dir/$save_dir ; fi
    filef90=`echo $file | sed 's/.$/f90/'`
    if [ -f $dir/$filef90 ] ; then cp $dir/$filef90 $dir/$save_dir  ; fi
   done
  fi
 fi
 if [ -f lib/$library ] ; then
  cp $compdir/lib/$library $dir/$save_dir
 fi
fi
#
# Restore files
#
if [[ -d $dir/$restore_dir/ ]]  && [[ ! $dir == *"yambo/Ydriver"* ]] ; then
 # Restore operation starts
 # remove files from previous compilation
 count_obj=`ls -1 $dir/*.o 2>/dev/null | wc -l`
 count_mod=`ls -1 $dir/*.mod 2>/dev/null | wc -l`
 count_f90=`ls -1 $dir/*.f90 2>/dev/null | wc -l`
 if [ $count_obj != 0 ]; then
  if [ "$VERB" == 1 ] ; then echo "Removing $dir/*.o " ; fi
  rm $dir/*.o  ;
 fi
 if [ "$count_mod" -gt "0" ] ; then
  if [ "$VERB" == 1 ] ; then echo "Removing $dir/*.mod " ; fi
  cd $dir
  for mod in *.mod ; do
   [ -f "$mod" ] || break
   rm $compdir/include/$mod
  done
  rm *.mod
  cd $path_back
 fi
 if [ $count_f90 != 0 ]; then
  if [ "$VERB" == 1 ] ; then echo "Removing $dir/*.f90 " ; fi
  rm $dir/*.f90
 fi
 if [ ! "$library" == "NONE" ] && [ -f $compdir/lib/$library ]; then
  if [ "$VERB" == 1 ] ; then echo "Removing $compdir/lib/$library " ; fi
  rm -f $compdir/lib/$library
 fi
 # check for files
 if [ ! -f $dir/$restore_dir/files.dep ]; then
   deps=""
 else
  deps=`cat $dir/$restore_dir/files.dep`
 fi
 found=""
 missing_files=`comm -23 <(tr ' ' $'\n' <<< $deps | sort) <(tr ' ' $'\n' <<< $found | sort)`
 count_obj_res=`ls -1 $dir/$restore_dir/*.o   2>/dev/null | wc -l`
 count_mod_res=`ls -1 $dir/$restore_dir/*.mod 2>/dev/null | wc -l`
 count_f90_res=`ls -1 $dir/$restore_dir/*.f90 2>/dev/null | wc -l`
 if [ $count_obj_res != 0 ] || [ $count_mod_res != 0 ] ; then
  cd $dir/$restore_dir
  found=`ls *.o *.mod 2>/dev/null`
  cd $path_back
  missing_files=`comm -23 <(tr ' ' $'\n' <<< $deps | sort) <(tr ' ' $'\n' <<< $found | sort)`
  cd $dir
  if [ $count_obj_res != 0 ] ; then ln -s $restore_dir/*.o   ./ ; fi
  if [ $count_f90_res != 0 ] ; then ln -s $restore_dir/*.f90 ./ ; fi
  if [ $count_mod_res != 0 ] ; then
   # modules need to be linked also inside $compdir/include
   for mod in $restore_dir/*.mod ; do
    [ -f "$mod" ] || break
    ln -s $mod ./
    mod_filename=`basename $mod`
    cd $compdir/include
    if test -f $mod_filename ; then  rm $mod_filename; fi
    ln -s $compdir/$dir/$mod_filename ./
    cd $path_back/$dir
   done
  fi
  cd $path_back
 fi
 if [[ ! "$missing_files" == "" ]] && [ ! "$library" == "NONE" ]; then
  rm -f $dir/$restore_dir/$library
  DIR_is_to_recompile=1
 fi
 if [[ -f $dir/$restore_dir/$library ]] || [ "$library" == "NONE" ]; then
  if [[ -f $dir/$restore_dir/$library ]] ; then
   cd $compdir/lib
   ln -s $compdir/$dir/$restore_dir/$library ./
   cd $path_back
  fi
  if [[ "$missing_files" == "" ]] ; then
   FOLDER_OK=1
   source ./sbin/compilation/fix_locks.sh
  fi
 fi
fi
