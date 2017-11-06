#! /bin/sh
#
#        Copyright (C) 2000-2017 the YAMBO team
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
if [ $# = 0 ] ; then exit 0; fi
target=$1

if test ! -f "$target"; then 
 for file in __*
 do
  if test `find . -name '*.o' | wc -l` -ge 1 ; then
   if test ! -d .debug$file; then mkdir .debug$file; fi
   mv *.o .debug"$file"/
  fi
  rm -f $file
 done
 touch "$target" 
 if test -d .debug$target; then
  if test `find .debug$target/ -name '*.o' | wc -l` -ge 1 ; then
   mv .debug$target/*.o .
  fi
 fi
fi
