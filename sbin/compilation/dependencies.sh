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
#
# dependencies.sh -- script that computes dependencies on Fortran 90 modules/projects
# modified from the moduledep.sh distributed with Quantum ESPRESSO and added the project part
#
# Directories to process
#========================
dot_files=`find . -name '.objects'`
for file in $dot_files
do
 if [[ "$file" == *"archive"* ]]; then continue ; fi
 directories+=" "
 directories+=`dirname $file`
done
#
Nd=`echo $directories | wc -w`
Nd=$((Nd-1))
#
BASE=$PWD
#
proj_dep_stamp=config/stamps_and_lists/project_dependencies.stamp
if [ ! -f $proj_dep_stamp ] ; then
idir=0
for CDIR in $directories
do
 echo -en "\t[SETUP] Projects dependencies [ "
 for ((i = 0 ; i <= $idir; i++)); do echo -n "#"; done
 for ((j = i ; j <= $Nd  ; j++)); do echo -n " "; done
 echo -n " ] $idir/$Nd " $'\r'
 ((i=i%N)); ((i++==0)) && wait
 idir=$((idir+1))
 source ./sbin/compilation/dependencies_project.sh  &
 #
done
touch $proj_dep_stamp
wait
echo
fi
#
idir=0
for CDIR in $directories
do
 echo -en "\t[SETUP] Modules detection [ "
 for ((i = 0 ; i <= $idir; i++)); do echo -n "#"; done
 for ((j = i ; j <= $Nd  ; j++)); do echo -n " "; done
 echo -n " ] $idir/$Nd " $'\r'
 ((i=i%N)); ((i++==0)) && wait
 idir=$((idir+1))
 source ./sbin/compilation/dependencies_element.sh  &
 #
done
wait
echo
#
idir=0
for CDIR in $directories
do
 cd $CDIR
 echo -en "\t[SETUP] Modules dependencies [ "
 for ((i = 0 ; i <= $idir; i++)); do echo -n "#"; done
 for ((j = i ; j <= $Nd  ; j++)); do echo -n " "; done
 echo -n " ] $idir/$Nd " $'\r'
 idir=$((idir+1))
 if [ ! -e modulesdep.list ]; then
  cd $BASE
  continue 
 fi
 # Modules. Step II: create a local list of modules dependencies 
 #===============================================================
 # replace module names with file names
 # by applying the file of substitution patterns just created
 sed -f $compdir/config/stamps_and_lists/modules.rules modulesdep.list |
 awk '{if ($1 != $3) print}' |         # remove self dependencies
 sort  | uniq |                        # remove duplicates
 sed 's/@.*@//' >> $compdir/config/stamps_and_lists/global_modules_dep.list
 if test `cat modules.rules | wc -l` -gt 0; then
  sed -f modules.rules modulesdep.list |
  awk '{if ($1 != $3) print}' |         # remove self dependencies
  sort  | uniq |                        # remove duplicates
  sed 's/@.*@//' > local_modules.dep
 fi
 cd $BASE
done
echo

