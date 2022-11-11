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
# Save all files
#
count=`ls -1 $dir/*.o 2>/dev/null | wc -l`
if [ ! -d $dir/$save_dir ] && [ "$count" -gt 0 ] ; then
 if [ "$DRY_RUN" == 0 ] ; then
  if [ "$VERB" == 1 ] ; then echo "mkdir -p $dir/$save_dir" ; fi
  mkdir -p $dir/$save_dir
  count=`ls -1 $dir/*.o 2>/dev/null | wc -l`
  cd $dir;
  deps=`ls *.o`;
  for file in $deps; do echo " $file" >> "$save_dir/files.dep"; done
  cd $path_back ;
  if [ $count != 0 ]; then cp $dir/*.o $dir/$save_dir ; fi
  if [ -f lib/$library ] ; then  cp $compdir/lib/$library $dir/$save_dir ; fi
  count=`ls -1 $dir/*.f90 2>/dev/null | wc -l`
  if [ $count != 0 ]; then cp $dir/*.f90 $dir/$save_dir ; fi
  count=`ls -1 $dir/*.mod 2>/dev/null | wc -l`
  if [ $count != 0 ]; then cp $dir/*.mod $dir/$save_dir ; fi
 fi
fi
#
# Restore all files
#
if [[ -f $dir/$restore_dir/$library ]] ; then
 count_mods=`ls -1 $dir/*.mod 2>/dev/null | wc -l`
 count_modr=`ls -1 $dir/$restore_dir/*.mod 2>/dev/null | wc -l`
 count_f90=`ls -1 $dir/$restore_dir/*.f90 2>/dev/null | wc -l`
 rm $dir/*.o
 cp $dir/$restore_dir/*.o $dir/ ;
 if [ "$count_mods" -gt "0" ] ; then
  cd $dir
  for mod in *.mod ; do
    rm $compdir/include/$mod
  done
  cd $path_back
  rm $dir/*.mod
 fi
 if [ "$count_modr" -gt "0" ] ; then
  cp $dir/$restore_dir/*.mod $dir/ ;
  cp $dir/*.mod include/ ;
 fi
 if [ $count_f90 != 0 ]; then
  cp $dir/$restore_dir/*.f90 $dir/ ;
 fi
 cp $dir/$restore_dir/$library lib/
 FOLDER_OK=1
 source ./sbin/compilation/fix_locks.sh 
fi
