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
lock_files=`find $compdir/config/stamps_and_lists/ -name '*.lock'`
sorted_locks=$(echo "$lock_files"|tr " " "\n"|sort|uniq|tr "\n" " ")
#
# Locks -> string
#
lock_string=""
single_save_dir=""
double_save_dir=""
for lock in $sorted_locks
do
 lock=`echo $lock | sed "s/.lock//"`
 lock=`basename $lock`
 lock_string="${lock} ${lock_string}"
 if [ $lock = "DOUBLE" ]; then continue; fi
 single_save_dir="${lock}_${single_save_dir}"
done
double_save_dir="DOUBLE_${single_save_dir}.save"
single_save_dir="SINGLE_${single_save_dir}.save"
#
# -D* -> string
#
flag_string=""
for flag in $sorted_precomps
do
 flag=`echo $flag | sed "s/\-D_//"`
 flag_string="${flag} ${flag_string}"
done
#
# No lock files? New compilation scheme!
#
if [[ -z $lock_files ]]; then
 for lock in $sorted_precomps
 do
  lock=`echo $lock | sed "s/\-D_//"`
  touch $compdir/config/stamps_and_lists/$lock.lock
 done
 return
fi
#
# Check for missing/new precomp flags
#
missing=`comm -23 <(tr ' ' $'\n' <<< $lock_string | sort) <(tr ' ' $'\n' <<< $flag_string | sort)`
new=`comm -23 <(tr ' ' $'\n' <<< $flag_string | sort) <(tr ' ' $'\n' <<< $lock_string | sort)`
#
SRC_IS_DOUBLE="no"
COMPILATION_IS_DOUBLE="no"
#
for lock in $missing
do
 if [ $lock == "DOUBLE" ]; then
   SRC_IS_DOUBLE="yes"
 else
  echo -e "\n Global compilation $flag is not in the new configure setup."
  echo -e " Use "
  echo -e "  >make clean_all "
  echo -e " and restart the configuration procedure."
  echo -e " Compilation aborted.\n"
  exit 1
 fi
done
#
for lock in $new
do
 if [ $lock == "DOUBLE" ]; then
   COMPILATION_IS_DOUBLE="yes"
 else
  echo -e "\n New global compilation $flag is not in the existing configure setup."
  echo -e " Use "
  echo -e "  >make clean_all "
  echo -e " and restart the configuration procedure."
  echo -e " Compilation aborted.\n"
  exit 1
 fi
done
#
candidates=`find $dir -type f  -name 'objects.mk'`
candidates+=" include/pars.mod"
#
# NEW source is in DOUBLE => SAVE the current state 
#
if [ $COMPILATION_IS_DOUBLE == "yes" ]; then
 echo -e "\t[DOUBLE compilation] Saving the current SINGLE source state"
 if test -d $dir/${double_save_dir} ; then
  echo -e "\t[SINGLE compilation] Restoring the previous DOUBLE compiled source"
 fi
 for file in $candidates
 do
   dir=`dirname $file`
   lib=`basename $dir`
   mkdir -p $dir/${single_save_dir}
   files_to_move=`find $dir  -maxdepth 1  -type f -name "*.o" -o -name "*.mod"` 
   N=`echo $files_to_move |wc -w`
   for el in $files_to_move
   do
     mv $el $dir/${single_save_dir}
   done
   rm -f $compdir/config/stamps_and_lists/lib${lib}.a.stamp
   rm -f $compdir/config/stamps_and_lists/lib_ypp_${lib}.a.stamp
   rm -f $compdir/config/stamps_and_lists/lib_Ydriver_${lib}.a.stamp
   touch $compdir/config/stamps_and_lists/DOUBLE.lock
   if test -d $dir/${double_save_dir} ; then
    files_to_move=`find $dir/${double_save_dir} -type f -name "*.o" -o -name "*.mod"` 
    for el in $files_to_move
    do
      mv $el $dir
    done
    rmdir $dir/${double_save_dir}
   fi
 done
fi
#
# OLD source is in DOUBLE => SAVE the current state 
#
if [ $SRC_IS_DOUBLE == "yes" ]; then
 echo -e "\t[SINGLE compilation] Saving the current DOUBLE source state"
 if test -d $dir/${single_save_dir} ; then
  echo -e "\t[SINGLE compilation] Restoring the previous SINGLE compiled source"
 fi
 for file in $candidates
 do
   dir=`dirname $file`
   lib=`basename $dir`
   mkdir -p $dir/${double_save_dir}
   files_to_move=`find $dir -maxdepth 1 -type f  -name "*.o" -o -name "*.mod"` 
   for el in $files_to_move
   do
     mv $el $dir/${double_save_dir}
   done
   rm -f $compdir/config/stamps_and_lists/lib${lib}.a.stamp
   rm -f $compdir/config/stamps_and_lists/lib_ypp_${lib}.a.stamp
   rm -f $compdir/config/stamps_and_lists/lib_Ydriver_${lib}.a.stamp
   rm -f $compdir/config/stamps_and_lists/DOUBLE.lock
   if test -d $dir/${single_save_dir} ; then
    files_to_move=`find $dir/${single_save_dir} -type f -name "*.o" -o -name "*.mod"` 
    for el in $files_to_move
    do
      mv $el $dir
    done
    rmdir $dir/${single_save_dir}
   fi
 done
fi
#
