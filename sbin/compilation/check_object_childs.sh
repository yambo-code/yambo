#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2018 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
# This script:
# - tags the library to be compiled
# - remove the object file
#
# Check for OBJ childs (non zero only if OBJ is a module)...
#
source ./sbin/compilation/verbosity.sh "check_object_childs.sh on $file_src: calling object_remove.sh remove $1"
source ./sbin/compilation/object_remove.sh "remove" $1
#
if [ -z $file_o ] ; then return; fi
#
# Check for OBJ childs (non zero only if OBJ is a module)...
#
file_o_base=`basename $file_o`
#
first_level_dep=""
if grep -q "$file_o_base" $compdir/config/stamps_and_lists/global_modules_dep.list; then
 #
 deps=`grep -w $file_o_base $compdir/config/stamps_and_lists/global_modules_dep.list | awk '{print $1}'`
 for dep in $deps
 do
  if test "$dep" == "$file_o_base"; then continue; fi
  first_level_dep+=" $dep"
 done
 #
 for first_dep_file in $first_level_dep
 do
  source ./sbin/compilation/name_me.sh $first_dep_file "search"
  source ./sbin/compilation/verbosity.sh "check_object_childs.sh: on $file_o_src (child). calling object_remove.sh remove $1"
  source ./sbin/compilation/object_remove.sh "remove" $1
 done
fi
