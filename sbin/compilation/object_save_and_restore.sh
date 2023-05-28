#!/bin/bash
#
#        Copyright (C) 2000-2023 the YAMBO team
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
path_back=$PWD
library="${target}.a"
#
if [[ "$goal" == "$target" ]]; then
 source $compdir/sbin/compilation/verbosity.sh "goal(=target)= $goal . Here I'm doing the driver folder, but there is no connected library"
 library="NONE"
fi
if [[ "$target" == *"_Ydriver_"* ]] ; then
 source $compdir/sbin/compilation/verbosity.sh "Ydriver => no library saving"
 library="NONE"
fi
source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh is running in $library using $dir/$restore_dir (restore) $dir/$save_dir (save)"
#
# Let's count
#
count_obj=`ls -1 $dir/*.o 2>/dev/null | wc -l`
count_mod=`ls -1 $dir/*.mod 2>/dev/null | wc -l`
count_f90=`ls -1 $dir/*.f90 2>/dev/null | wc -l`
count_obj_ts=`ls -1 $dir/*.o_to_save 2>/dev/null | wc -l`
count_mod_ts=`ls -1 $dir/*.mod_to_save 2>/dev/null | wc -l`
count_obj_res=`ls -1 $dir/$restore_dir/*.o 2>/dev/null | wc -l`
count_mod_res=`ls -1 $dir/$restore_dir/*.mod 2>/dev/null | wc -l`
count_f90_res=`ls -1 $dir/$restore_dir/*.f90 2>/dev/null | wc -l`
#
# Save files
#
if [ ! -d $dir/$save_dir ] ; then
 source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: mkdir $save_dir"
 mkdir -p $dir/$save_dir
fi
cd $dir
if [ $count_obj != 0 ]; then
 for tos in *.o; do
  source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: saving $tos"
  if ! test -L $tos; then cp -u $tos $save_dir; fi
 done
fi
if [ $count_mod != 0 ]; then
 for tos in *.mod; do
  source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: saving $tos"
  if ! test -L $tos; then cp -u $tos $save_dir; fi
 done
fi
if [ $count_f90 != 0 ]; then
 for tos in *.f90; do
  source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: saving $tos"
  if ! test -L $tos; then cp -u $tos $save_dir; fi
 done
fi
if [ $count_obj_ts != 0 ]; then
 for tos in *.o_to_save; do
  source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: saving and removing $tos"
  if ! test -L $tos; then mv $tos $save_dir/${tos/_to_save}; fi
 done
fi
if [ $count_mod_ts != 0 ]; then
 for tos in *.mod_to_save; do
  source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: saving and removing $tos"
  if ! test -L $tos; then mv $tos $save_dir/${tos/_to_save}; fi
  source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: removing" $compdir/include/${tos/_to_save}
  rm -f $compdir/include/${tos/_to_save}
 done
fi
cd $path_back
if [[ -f "$compdir/lib/$library" ]]; then
 source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: saving $compdir/lib/$library to $dir/$save_dir"
 if ! test -L $compdir/lib/$library; then cp -u $compdir/lib/$library $dir/$save_dir; fi
fi
#
# Restore files
#
if [[ -d $dir/$restore_dir ]] && [[ ! "$restore_dir" == "$save_dir" ]] ; then
 cd $dir
 if [ $count_obj_res != 0 ]; then
  for tos in $restore_dir/*.o; do
   tos_b=`basename $tos`
   source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: removing $tos_b in $dir"
   rm -f $tos_b
   source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: linking $tos to $dir"
   ln -s $tos .
  done
 fi
 if [ $count_mod_res != 0 ]; then
  for tos in $restore_dir/*.mod; do
   tos_b=`basename $tos`
   source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: removing $tos_b in $dir"
   rm -f $tos_b
   source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: linking $tos to $dir"
   ln -s $tos .
   source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: removing $tos_b in $compdir/include"
   cd $compdir/include
   rm -f $tos_b
   source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: linking $compdir/$dir/$tos to $compdir/include"
   ln -s $compdir/$dir/$tos ./
   cd $path_back
   cd $dir
  done
 fi
 if [ $count_f90_res != 0 ]; then
  for tos in $restore_dir/*.f90; do
   tos_b=`basename $tos`
   source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: removing $tos_b in $dir"
   rm -f $tos_b
   source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: linking $tos to $dir"
   ln -s $tos .
  done
 fi
 cd $path_back
 if [[ -f $dir/$restore_dir/$library ]] && [[ ! "$library" == "NONE" ]]; then
  source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: removing $library in $compdir/lib"
  cd $compdir/lib
  rm -f $library
  source $compdir/sbin/compilation/verbosity.sh "object_save_and_restore.sh: linking $compdir/$dir/$restore_dir/$library to $compdir/lib"
  ln -s $compdir/$dir/$restore_dir/$library ./
  DIR_restored="yes"
 else
  DIR_restored="no"
 fi
 cd $path_back
 source $compdir/sbin/compilation/fix_locks.sh
fi

