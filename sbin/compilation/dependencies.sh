#!/bin/bash
#
#        Copyright (C) 2000-2021 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): HM AM
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
 directories+=" "
 directories+=`dirname $file`
done
#
Nd=`echo $directories | wc -w`
Nd=$((Nd-1))
#
BASE=$PWD
idir=0
for CDIR in $directories
do
 cd $CDIR
 echo -en "\t[SETUP] Modules detection [ "
 for ((i = 0 ; i <= $idir; i++)); do echo -n "#"; done
 for ((j = i ; j <= $Nd  ; j++)); do echo -n " "; done
 echo -n " ] $idir/$Nd " $'\r'
 idir=$((idir+1))
#
# Sources to process
#====================
sources=" "
if test `find . -maxdepth 1 -name '*.F' | wc -l` -ge 1 ; then
 sources+=`echo *.F`
fi
sources+=" " 
if test `find . -maxdepth 1 -name '*.c' | wc -l` -ge 1 ; then
 sources+=`echo *.c`
fi
if [ ${#sources} -eq 2 ]; then 
 cd $BASE
 continue 
fi
#
# Projects 
#==========
for PJ in _SC _RT _ELPH _PHEL _NL _QED _YPP_ELPH _YPP_RT _YPP_NL _YPP_SC _yambo _ypp
do
 sources_pj_dependent=" "
 for file in $sources
 do
  if test `grep $PJ $file | grep '#'| wc -l` -ge 1; then
    obj=`echo $file| sed 's/\.F/\.o/g'| sed 's/\.c/\.o/g'`
    sources_pj_dependent+=" ${obj} $PJ\n"
  fi
 done
 PREFIX=`echo $PJ | sed 's/_//g'`
 if [ ${#sources_pj_dependent} -gt 1 ]; then
  echo -e "$sources_pj_dependent" >>  ${PREFIX}_project.dep
 fi
done
#
# Modules. Step I: get the list of modules used and defined
#===========================================================
# create list of module dependencies
# each line is of the form:
# file_name.o : @module_name@
# cast all module names to lowercase because Fortran is case insensitive
egrep -H -i -e "include ?<memory.h>" -e "^ *use " $sources |  # look for "USE name"
sed 's/F:/o /
     s/,/ /;s/#include/ use /;s/<memory.h>/memory/' | # replace extension, insert space
#                                         #   and remove trailing comma
awk '{print $1 " : @" tolower($3) "@"}' | # create dependency entry
sort | uniq > modulesdep.list              # remove duplicates

# create list of available modules
# for each module, create a line of the form:
# s/@module_name@/file_name/g
egrep -H -i "^ *module " $sources |           # look for "MODULE name"
sed 's/F:/o /
     s/\//\\\//g' |                            # replace extension, insert
#                                              #   space and escape slashes
awk '{print "s/@" tolower($3) "@/" $1 "/" }' | # create substitution line
sort | uniq > modules.rules                    # remove duplicates

egrep -H -i "^ *module " $sources |           # look for "MODULE name"
sed 's/F:/o /
     s/\//\\\//g' |                            # replace extension, insert
#                                              #   space and escape slashes
awk '{print tolower($3) }' | # create substitution line
sort | uniq > modules.list                    # remove duplicates
#
# Add the local rules to the global file
cat modules.rules  >> $compdir/config/stamps_and_lists/modules.rules
#
cd $BASE
#
done
echo
#
idir=0
for CDIR in $directories
do
 cd $CDIR
 echo -en "\t[SETUP] Dependencies [ "
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
sed 's/@.*@//' > global_modules.dep
sed -f modules.rules modulesdep.list |
awk '{if ($1 != $3) print}' |         # remove self dependencies
sort  | uniq |                        # remove duplicates
sed 's/@.*@//' > local_modules.dep
cd $BASE
done
echo

