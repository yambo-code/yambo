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
# make sure there is no locale setting creating unneeded differences.
#
LC_ALL=C
export LC_ALL
#
ARGS=$@;
source ./sbin/compilation/helper.inc.sh
#
# OPTIONS
source ./sbin/compilation/options.sh
#
# Dependencies?
if [ "$dep" == "yes" ]; then 
 source ./sbin/compilation/dependencies.sh
 exit 0
fi
#
# CHECK
if [ ! -f $cdir/$ofile ]; then exit 0; fi
#
# CLEAN
if [ -f $cdir/Makefile ] ; then rm -f $cdir/Makefile ;  fi
#
# DEFS (2)
pjdep_file="project.dep"
moduledep_file="modules.dep"
modlist_file="modfiles.list"
#
# Projects
source ./sbin/compilation/projects.sh
#
# Lock files
source ./sbin/compilation/lock_files.sh
#
# Libraries
source ./sbin/compilation/libraries.sh
#
#echo "helper.sh DIR: $cdir"
#echo "helper.sh TARG: $target"
#echo "helper.sh OFILE: $ofile"
#echo "helper.sh mode: $mode"
#echo "helper.sh LIBS: $libs"
#echo "helper.sh PRECOMP: $precomp_flags"
#echo "helper.sh M LOCKS: $modules_lock"
#echo "helper.sh O LOCKS: $objects_lock"
#
# Project dependencies
#if [ ! -f $cdir/project.dep ] ; then 
# for project in _SC _RT _ELPH _PHEL _NL _QED _YPP_ELPH _YPP_RT _YPP_NL _YPP_SC _DOUBLE
# do
#   @compdir@/sbin/projectdep.sh $cdir $project
# done
# for exe in _a2y _c2y _p2y _yambo _ypp
# do
#   @compdir@/sbin/projectdep.sh $cdir $exe
# done
#fi
#
# Makefile (I): variables
cat <<EOF > $cdir/dyn_variables.mk
srcdir =$srcdir
target =$target
wdir   =$cdir
EOF
#
# Makefile (II): OBJECTS list
cp $cdir/.objects $cdir/objects.mk
#
# Makefile (II): common vars
rm_command="@rm -f \$*\$(f90suffix)"
if [ "$KEEPSRC" == "yes" ]; then rm_command=" "; fi ;
#
cat <<EOF > sbin/compilation/mk/static_variables.mk
libs           =$libs
linclude       =$lf90include
lf90libinclude =$lf90libinclude
lf90include    =$lf90include
modinclude     =$INCLUDEDIR//$modules_lock
mfiles         =find . -maxdepth 1 -name '*.mod'
precomp_mpi    =$precomp_mpi
precomp_flags  =$precomp_flags -D_\$(os)
objects_lock   =$objects_lock
moduledep_file =$moduledep_file
modlist_file   =$modlist_file
rm_command     =$rm_command
EOF
#
# Makefile (III): copy makefile
cp sbin/compilation/mk/makefile $cdir/Makefile
#
# Makefile creation: (IV) operations 
#FC_NOOPT_SRC="mod_parser_m.o mod_logo.o"
#for arg in $@; do
# case $arg in
#  -D_PGI)
#   FC_NOOPT_SRC="$FC_NOOPT_SRC bz_samp_indexes.o" ;;
# esac
#done
#
