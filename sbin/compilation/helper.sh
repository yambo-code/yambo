#!/bin/bash
#
#        Copyright (C) 2000-2022 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM DS
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
# make sure there is no locale setting creating unneeded differences.
#
LC_ALL=C
export LC_ALL
#
BASE=$PWD
#
ARGS=$@;
source ./sbin/compilation/helper.inc.sh
#
# OPTIONS
source ./sbin/compilation/options.sh
#
# Verbosity
source ./sbin/compilation/verbosity.sh "init"
#
# Projects
source ./sbin/compilation/projects.sh
#
source ./sbin/compilation/verbosity.sh "options"
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
# IO(avoid)path
#
if [ "$IO_dir" == "io_serial"   ] ; then IO_no_dir="io_parallel"; fi
if [ "$IO_dir" == "io_parallel" ] ; then IO_no_dir="io_serial"; fi
#
# Check what has to be done
if [ "$new" == "yes" ]  && [[ -f $compdir/config/stamps_and_lists/active_directories.list ]] ; then 
 dirs_to_check=`cat $compdir/config/stamps_and_lists/active_directories.list`
 for dir in $dirs_to_check
 do
  if [[ "$dir" == "./$cdir" ]]; then
   #
   # Global logicals
   #
   DIR_saved=""
   DIR_restored=""
   DIR_is_to_recompile=0
   if [ ! "$mode" == "fast" ] ; then
    source ./sbin/compilation/verbosity.sh "helper.sh: call check_update_locks.sh"
    source ./sbin/compilation/check_updated_locks.sh
   fi
   source ./sbin/compilation/verbosity.sh "helper.sh: call check_update_sources.sh"
   source ./sbin/compilation/check_updated_sources.sh
   if [ "$DIR_is_to_recompile" == 1 ] ; then
     source ./sbin/compilation/verbosity.sh "helper.sh: locks force $dir to be recompiled"
     source ./sbin/compilation/stamp_remove.sh "goal"
     source ./sbin/compilation/stamp_remove.sh "target.a"
     source ./sbin/compilation/stamp_remove.sh "exe"
   else
     source ./sbin/compilation/verbosity.sh "helper.sh: $dir must NOT to be recompiled"
   fi
  fi
 done
 #
 # remove broken links
 #
 count_lib_files=`ls -1 lib/*.a 2>/dev/null | wc -l`
 if [ $count_lib_files -gt 0 ]; then
  for lib_file in lib/*.a ; do
   if [ ! -e $lib_file ]; then rm $lib_file ; fi
  done
 fi
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
if [ ! -f $cdir/$ofile ]; then 
 source ./sbin/compilation/verbosity.sh "WARNING helper.sh: $cdir/$ofile is not there. Exiting"
 exit 0 
fi
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
# Makefile (I): variables
cat <<EOF > $cdir/dyn_variables.mk
compdir =$compdir
srcdir  =$srcdir
target  =$target
wdir    =$cdir
EOF
#
# Makefile (II): OBJECTS list
source ./sbin/compilation/verbosity.sh "helper.sh. objects.mk=$cpp $cppflags $precomp_flags -D_$DTARG $cdir/objects.c"
cp $cdir/.objects $cdir/objects.c
DTARG=`echo $target | sed "s/\.a//" | sed "s/\-//"`
$cpp $cppflags $precomp_flags -D_$DTARG $cdir/objects.c  > $cdir/objects.mk
rm -f $cdir/objects.c 
#
# Makefile (III): common vars
rm_command="@rm -f \$*\$(f90suffix)"
if [ "$KEEPSRC" == "yes" ]; then rm_command=" "; fi ;
#
# Makefile creation: (IV) special sources 
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
# Makefile (V): copy makefile
cp config/mk/local/makefile $cdir/Makefile
#
# Makefile (VI): clean orphan _to_save files
#
# Some files scheduled to be moved can still be unmoved. This happens if the _to_save flag is added
# to a file that has no explicit dependence on the projects (see for example ypp/dipoles/DIPOLES_ypp_driver).
# In this case the file cannot be saved in a project dependent folder and needs to be removed.
#
if [ "$mode" == "x" ] ; then
 files_to_remove=`find $compdir -type f -name "*_to_save"`
 for file in $files_to_remove
 do
   source ./sbin/compilation/verbosity.sh "helper.sh: rm $file"
   rm -f $file
 done
fi

