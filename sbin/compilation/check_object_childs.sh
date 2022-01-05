#!/bin/bash
#
#        Copyright (C) 2000-2021 the YAMBO team
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
# Check for OBJ childs (non zero only if OBJ is a module)...
#
file=$obj
source ./sbin/compilation/object_save_restore_remove.sh "remove"
#
# Check for OBJ childs (non zero only if OBJ is a module)...
#
first_level_dep=
if grep -q "$obj" $compdir/config/stamps_and_lists/global_modules_dep.list; then
 #
 deps=`grep -w $obj $compdir/config/stamps_and_lists/global_modules_dep.list | awk '{print $1}'`
 for dep in $deps
 do
  if test "$dep" == "$obj"; then continue; fi
  first_level_dep+=" $dep"
 done
 #
 for file in $first_level_dep
 do
  source ./sbin/compilation/object_save_restore_remove.sh "remove"
 done
fi
