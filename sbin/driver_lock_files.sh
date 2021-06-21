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
objects_lock="_objects_lock"
modules_lock="_modules_lock"
for arg in $ARGS 
do
 case $arg in
  -D_yambo|-D_ypp)
   a=`echo $arg  | sed "s/-D_/_/"`
   objects_lock="$objects_lock$a"
   ;;
  -D_64BIT_OFFSET|-D_SLEPC_OFF|-D_DOUBLE|-D_yambo|-D_ypp)
   ;;
  -D_*) 
   a=`echo $arg  | sed "s/-D_/_/"`
   objects_lock="$objects_lock$a"
   modules_lock="$modules_lock$a"
   ;;
 esac
done

