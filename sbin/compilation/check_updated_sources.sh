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
if test -f $compdir/config/stamps_and_lists/${goal}.stamp; then
 candidates=`find $srcdir/$dir -type f -name "*.F" -newer $compdir/config/stamps_and_lists/${goal}.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.f" -newer $compdir/config/stamps_and_lists/${goal}.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.c" -newer $compdir/config/stamps_and_lists/${goal}.stamp`
fi
if test -f $compdir/config/stamps_and_lists/${target}.stamp; then
 candidates=`find $srcdir/$dir -type f -name "*.F" -newer $compdir/config/stamps_and_lists/${target}.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.f" -newer $compdir/config/stamps_and_lists/${target}.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.c" -newer $compdir/config/stamps_and_lists/${target}.stamp`
fi
if test -f $compdir/config/stamps_and_lists/${target}.a.stamp; then
 candidates=`find $srcdir/$dir -type f -name "*.F" -newer $compdir/config/stamps_and_lists/${target}.a.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.f" -newer $compdir/config/stamps_and_lists/${target}.a.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.c" -newer $compdir/config/stamps_and_lists/${target}.a.stamp`
fi
if [ ! -z "$candidates" ]; then
 source ./sbin/compilation/verbosity.sh "check_updated_sources.sh: there are sources to check"
fi
for candidate in $candidates
do
  source ./sbin/compilation/verbosity.sh "check_updated_sources.sh: search for $candidate childs"
  source ./sbin/compilation/name_me.sh $candidate "no_search"
  DIR_is_to_recompile=1
  source ./sbin/compilation/check_object_childs.sh "sources"
done
