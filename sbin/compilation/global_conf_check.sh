#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2021 The Yambo Team
#
# Authors (see AUTHORS file for details): HM AM
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
