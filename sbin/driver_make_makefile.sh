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
if [ "$1" = "HEADER" ] ; then
cat config/setup >>$cdir/Makefile
cat << EOF >> $cdir/Makefile
libs=$libs
linclude=$lf90include
lf90libinclude=$lf90libinclude
lf90include=$lf90include
modinclude=$INCLUDEDIR//$modules_lock
mfiles=find . -maxdepth 1 -name '*.mod'
target=$target
precomp_mpi=$precomp_mpi
precomp_flags=$precomp_flags -D_\$(os)
objects_lock=$objects_lock
moduledep_file=$moduledep_file
modlist_file=$modlist_file
EOF
fi

if [ "$1" = "OBJECTS" ] ; then
cp $cdir/$ofile $cdir/$ofile.c
$cpp $cppflags $precomp_flags -D_$os -D_$target $cdir/$ofile.c >> $cdir/Makefile
rm -f $cdir/$ofile.c
fi

