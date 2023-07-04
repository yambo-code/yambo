#!/bin/bash
#
#        Copyright (C) 2000-2022 the YAMBO team
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
save_dir=""
for lock in $sorted_locks
do
 lock=`echo $lock | sed "s/.lock//"`
 lock=`basename $lock`
 lock_string="${lock} ${lock_string}"
 save_dir="${lock}_${single_save_dir}"
done
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
missing=`comm -23 <(tr ' ' $'\n' <<< $lock_string | sort| uniq) <(tr ' ' $'\n' <<< $flag_string | sort| uniq)`
new=`comm -23 <(tr ' ' $'\n' <<< $flag_string | sort| uniq) <(tr ' ' $'\n' <<< $lock_string | sort| uniq)`
#
for lock in $missing
do
 echo -e "\n Global compilation $flag is not in the new configure setup."
 echo -e " Use "
 echo -e "  >make clean_all "
 echo -e " and restart the configuration procedure."
 echo -e " Compilation aborted.\n"
 exit 1
done
#
for lock in $new
do
 echo -e "\n New global compilation $flag is not in the existing configure setup."
 echo -e " Use "
 echo -e "  >make clean_all "
 echo -e " and restart the configuration procedure."
 echo -e " Compilation aborted.\n"
 exit 1
done
#
