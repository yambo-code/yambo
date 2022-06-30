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
if [ $1 == "goal" ] ; then
 if [ "$VERB" == 1 ] ; then
   echo "rm -f $compdir/config/stamps_and_lists/${goal}.stamp"
 else
  rm -f $compdir/config/stamps_and_lists/${goal}.stamp 
 fi
fi
if [ $1 == "target.a" ] ; then
 if [ "$VERB" == 1 ] ; then
   echo "rm -f $compdir/config/stamps_and_lists/${target}.a.stamp"
 else
  rm -f $compdir/config/stamps_and_lists/${target}.a.stamp 
 fi
fi
if [ $1 == "lib" ] ; then
 if [ "$VERB" == 1 ] ; then
   echo "rm -f $compdir/config/stamps_and_lists/lib${llib}.a.stamp"
 else
   rm -f $compdir/config/stamps_and_lists/lib${llib}.a.stamp
 fi
fi
if [ $1 == "exe" ] ; then
  stamp=`find $compdir/config/stamps_and_lists/ -name "compiling*"  | sed "s/compiling_//"`
  rm -f $stamp
fi

