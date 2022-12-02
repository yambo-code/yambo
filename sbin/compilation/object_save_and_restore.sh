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
 if [ "$VERB" == 1 ] ; then echo "here I'm doing the driver folder, but there is no connected library"; fi
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
 count=`ls -1 $dir/*.o 2>/dev/null | wc -l`
 if [ $count != 0 ] ; then
  if [ ! -f $dir/$save_dir/files.dep ] ; then
   cd $dir;
   deps=`ls *.o 2>/dev/null`;
   for file in $deps; do echo " $file" >> "$save_dir/files.dep"; done
   cd $path_back ;
   cp $dir/*.o $dir/$save_dir/
   count=`ls -1 $dir/*.f90 2>/dev/null | wc -l`
   if [ $count != 0 ]; then cp $dir/*.f90 $dir/$save_dir ; fi
  else
   deps=`cat $dir/$save_dir/files.dep`
   cd $dir/$save_dir
   found=`ls *.o 2>/dev/null`
   cd $path_back
   missing_files=`comm -23 <(tr ' ' $'\n' <<< $deps | sort) <(tr ' ' $'\n' <<< $found | sort)`
   for file in $missing_files ; do
    if [ -f $dir/$file ] ; then cp $dir/$file $dir/$save_dir ; fi
    filef90=`echo $file | sed 's/.$/f90/'`
    if [ -f $dir/$filef90 ] ; then cp $dir/$filef90 $dir/$save_dir  ; fi
   done
  fi
 fi
 if [ -f lib/$library ] ; then
  cp $compdir/lib/$library $dir/$save_dir
  count=`ls -1 $dir/*.mod 2>/dev/null | wc -l`
  if [ $count != 0 ]; then
    cd $dir
    for mod in *.mod ; do
      if [ ! -f $save_dir/$mod ] ; then cp $mod $save_dir/ ; fi
    done
    cd $path_back
  fi
 fi
fi
#
# Restore files
#
if [[ -d $dir/$restore_dir/ ]]  && [[ ! $dir == *"yambo/Ydriver"* ]] ; then
 if [ "$VERB" == 1 ] ; then echo "Restoring files " ; fi
 count=`ls -1 $dir/*.o 2>/dev/null | wc -l`
 if [ $count != 0 ]; then rm $dir/*.o  ; fi
 if [ ! -f $dir/$restore_dir/files.dep ]; then
   deps=""
 else
  deps=`cat $dir/$restore_dir/files.dep`
 fi
 count=`ls -1 $dir/$restore_dir/*.o 2>/dev/null | wc -l`
 if [ $count != 0 ]; then
  cd $dir/$restore_dir
  found=`ls *.o 2>/dev/null`
  cd $path_back
  missing_files=`comm -23 <(tr ' ' $'\n' <<< $deps | sort) <(tr ' ' $'\n' <<< $found | sort)`
  cd $dir
  ln -s $restore_dir/*.o ./ ;
  cd $path_back
 fi
 count_mod=`ls -1 $dir/*.mod 2>/dev/null | wc -l`
 if [ "$count_mod" -gt "0" ] ; then
  cd $dir
  for mod in *.mod ; do
   rm $compdir/include/$mod
  done
  rm *.mod
  cd $path_back
 fi
 count_mod=`ls -1 $dir/$restore_dir/*.mod 2>/dev/null | wc -l`
 if [ "$count_mod" -gt "0" ] ; then
  cd $dir
  for mod in $restore_dir/*.mod ; do
   ln -s $mod ./
   mod_filename=`basename $mod`
   cd $compdir/include
   if test -f $mod_filename ; then  rm $mod_filename; fi
   ln -s $compdir/$dir/$mod_filename ./
   cd $path_back/$dir
  done
  cd $path_back
 fi
 count_f90=`ls -1 $dir/*.f90 2>/dev/null | wc -l`
 if [ $count_f90 != 0 ]; then rm $dir/*.f90  ; fi
 count_f90=`ls -1 $dir/$restore_dir/*.f90 2>/dev/null | wc -l`
 if [ $count_f90 != 0 ]; then
  cd $dir
  ln -s $restore_dir/*.f90 ./
  cd $path_back
 fi
 if [[ -f $dir/$restore_dir/$library ]] || [ "$library" == "NONE" ]; then
  if [[ -f $dir/$restore_dir/$library ]] ; then
   cd $compdir/lib
   rm $library
   ln -s $compdir/$dir/$restore_dir/$library ./
   cd $path_back
  fi
  if [[ "$missing_files" == "" ]] ; then
   FOLDER_OK=1
   source ./sbin/compilation/fix_locks.sh
  fi
 fi
fi
