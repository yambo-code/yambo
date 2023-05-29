#!/bin/bash
#
#        Copyright (C) 2000-2023 the YAMBO team
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
