#!/bin/bash
#
#        Copyright (C) 2000-2022 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM, DS
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
VERB=0
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
# Projects
source ./sbin/compilation/projects.sh
#
if [ "$VERB" == 1 ] ; then
 echo "cdir is $cdir"
 echo "target is $target"
 echo "lib is $lib"
 echo "goal is $goal"
fi
#
if [ "$global" == "yes" ]  ; then
 source ./sbin/compilation/global_conf_check.sh
 exit 0
fi
#
if [ "$target" == "$goal" ] ; then
 if [ ! -f bin/$goal ] ; then
  source ./sbin/compilation/stamp_remove.sh "exe"
 fi
fi
#
# Check what has to be done
if [ "$new" == "yes" ]  && [[ -f $compdir/config/stamps_and_lists/active_directories.list ]] ; then 
 dirs_to_check=`cat $compdir/config/stamps_and_lists/active_directories.list`
 for dir in $dirs_to_check
 do
  if [[ "$dir" == "./$cdir" ]]; then
   DIR_is_to_recompile=0
   FOLDER_OK=0
   if [ ! "$mode" == "fast" ] ; then
     source ./sbin/compilation/check_updated_locks.sh
   fi
   source ./sbin/compilation/check_updated_sources.sh
   if [ "$DIR_is_to_recompile" == 1 ] ; then
     if [ $VERB = 1 ] ; then echo "$dir is to be recompiled $goal, $target" ; fi
     source ./sbin/compilation/stamp_remove.sh "goal"
     source ./sbin/compilation/stamp_remove.sh "target.a"
     source ./sbin/compilation/stamp_remove.sh "exe"
   fi
   # The driver library always needs to be recompiled since it is not copied in the save folders
   # due to the name which depends on the exectuable. This causes the exe to be relinked.
   # Without I could avoid to remove the "exe" stamp.
   # However in such case I should remove somehow the copiling stamp.
   if [[ "$dir" == *"yambo/Ydriver"* ]] && [ "$FOLDER_OK" == 0 ]; then
     source ./sbin/compilation/stamp_remove.sh "target.a"
     source ./sbin/compilation/stamp_remove.sh "exe"
   fi
 fi
 done
 exit 0
fi
#
# Dependencies?
if [ "$dep" == "yes" ] ; then
 source ./sbin/compilation/dependencies.sh
 source ./sbin/compilation/configure_generated_files.sh
 exit 0
fi
#
# CHECK
if [ ! -f $cdir/$ofile ]; then exit 0; fi
#
# CLEAN
if [ -f $cdir/Makefile ] ; then rm -f $cdir/Makefile ;  fi
#
# Pre-compiler flags
precomp_string=`echo $precomp_flags | sed "s/ /_/g" | sed "s/\-D_//g"`
#
# Libraries
source ./sbin/compilation/libraries.sh
#
# Lock the current projects
for flag in $precomp_flags
do
 flag=`echo $flag | sed "s/\-D_//"`
 touch $cdir/${flag}.lock
done
#
if [ "$VERB" == 1 ] ; then
 echo "libs are $libs"
 echo "precomp flags are $precomp_flags"
fi
#
# Makefile (I): variables
cat <<EOF > $cdir/dyn_variables.mk
compdir =$compdir
srcdir  =$srcdir
target  =$target
wdir    =$cdir
EOF
#
# Makefile (II): OBJECTS list
cp $cdir/.objects $cdir/objects.c
DTARG=`echo $target | sed "s/\.a//" | sed "s/\-//"`
$cpp $cppflags $precomp_flags -D_$DTARG $cdir/objects.c  > $cdir/objects.mk
rm -f $cdir/objects.c 
#
# Makefile (II): common vars
rm_command="@rm -f \$*\$(f90suffix)"
if [ "$KEEPSRC" == "yes" ]; then rm_command=" "; fi ;
#
# Makefile creation: (III) special sources 
source ./sbin/compilation/special_sources.sh
#
cat <<EOF > $compdir/config/mk/local/static_variables.mk
STDLOG         =$compdir/log/"compile_"$goal".log"
libs           =$libs
linclude       =$lf90include
lf90libinclude =$lf90libinclude
lf90include    =$lf90include
modinclude     =$INCLUDEDIR
mfiles         =find . -maxdepth 1 -name '*.mod'
precomp_mpi    =$precomp_mpi
precomp_flags  =$precomp_flags -D_\$(os)
rm_command     =$rm_command
F77_NOOPT_SRC  =$F77_NOOPT_SRC
FC_NOOPT_SRC   =$FC_NOOPT_SRC
FC_LOCAL_SRC   =$FC_LOCAL_SRC
EOF
#
# Makefile (III): copy makefile
cp config/mk/local/makefile $cdir/Makefile
#
