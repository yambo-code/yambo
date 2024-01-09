#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM DS
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

