#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2021 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
lock_files=`find $dir -name '*.lock'`
sorted_locks=$(echo "$lock_files"|tr " " "\n"|sort|uniq|tr "\n" " ")
#
# Locks -> string
#
lock_string=""
save_dir="objects"
for lock in $sorted_locks
do
 lock=`echo $lock | sed "s/.lock//"`
 lock=`basename $lock`
 if [[ -f  "$dir/${lock}_project.dep" ]]; then
  lock_string="${lock} ${lock_string}"
  save_dir="${lock}_${save_dir}"
 fi
done
save_dir="${save_dir}.save"
#
# -D* -> string
#
flag_string=""
restore_dir="objects"
for flag in $sorted_precomps
do
 flag=`echo $flag | sed "s/\-D_//"`
 if [[ -f  "$dir/${flag}_project.dep" ]]; then
  flag_string="${flag} ${flag_string}"
  restore_dir="${flag}_${restore_dir}"
 fi
done
restore_dir="${restore_dir}.save"
#
# Check for missing/new precomp flags
#
missing=`comm -23 <(tr ' ' $'\n' <<< $lock_string | sort) <(tr ' ' $'\n' <<< $flag_string | sort)`
new=`comm -23 <(tr ' ' $'\n' <<< $flag_string | sort) <(tr ' ' $'\n' <<< $lock_string | sort)`
#
unmatched="$missing $new"
#
# Now new nor missing -> exit
#
if [[ -z $new ]] && [[ -z $missing ]]; then
 source ./sbin/compilation/verbosity.sh "check_updated_locks.sh: No new or missing projects"
 return
fi
#
# Ydriver is to be recompiled anyway as the library name depends on the executable and cannot be saved
if [[ "$target" == *"_Ydriver_"* ]] ; then DIR_is_to_recompile=1; fi
#
source ./sbin/compilation/verbosity.sh "locks"
#
# Save & Restore
source ./sbin/compilation/object_save_and_restore.sh
#
if [ "$DIR_saved" == "yes" ] ; then
 source ./sbin/compilation/verbosity.sh "check_updated_locks.sh: $dir has been saved"
fi
if [ "$DIR_restored" == "yes" ] ; then
 source ./sbin/compilation/verbosity.sh "check_updated_locks.sh: $dir has been restored"
fi
#
# tag new objects to be compiled
#
for lock in $unmatched
do
 #
 if test -f "$dir/${lock}_project.dep"; then
  deps=`cat $dir/${lock}_project.dep`
  for dep_file in $deps; do
   source ./sbin/compilation/verbosity.sh "check_updated_locks.sh: $dep_file must be recompiled"
   source ./sbin/compilation/name_me.sh $dir/$dep_file "no_search"
   if [ ! "$DIR_restored" == "yes" ] ; then
     DIR_is_to_recompile=1
   fi
   if [ "$lock" == "DOUBLE" ]; then
    source ./sbin/compilation/object_remove.sh "remove" "locks"
    continue;
   fi
   source ./sbin/compilation/check_object_childs.sh "locks"
  done
 fi
 #
done
#
source ./sbin/compilation/fix_locks.sh
