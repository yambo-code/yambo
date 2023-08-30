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
cd $srcdir/$CDIR
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
#
if [ ${#sources} -eq 2 ]; then
  cd $BASE
  continue 
fi
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
sort | uniq > $compdir/$CDIR/modulesdep.list              # remove duplicates

# create list of available modules
# for each module, create a line of the form:
# s/@module_name@/file_name/g
egrep -H -i "^ *module " $sources |           # look for "MODULE name"
sed 's/F:/o /
     s/\//\\\//g' |                            # replace extension, insert
#                                              #   space and escape slashes
awk '{print "s/@" tolower($3) "@/" $1 "/" }' | # create substitution line
sort | uniq > $compdir/$CDIR/modules.rules     # remove duplicates

egrep -H -i "^ *module " $sources |           # look for "MODULE name"
sed 's/F:/o /
     s/\//\\\//g' |                            # replace extension, insert
#                                              #   space and escape slashes
awk '{print tolower($3) }' | # create substitution line
sort | uniq > $compdir/$CDIR/modules.list      # remove duplicates
#
# Add the local rules to the global file
cat $compdir/$CDIR/modules.rules  >> $compdir/config/stamps_and_lists/modules.rules
#
cd $BASE
#
