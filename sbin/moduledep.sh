#!/bin/bash
#
#        Copyright (C) 2000-2017 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): HM
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
# moduledep.sh -- script that computes dependencies on Fortran 90 modules
# modified from the moduledep.sh distributed with Quantum ESPRESSO
#
# make sure there is no locale setting creating unneeded differences.
#
LC_ALL=C
export LC_ALL

# files whose dependencies must be computed
sources=`echo *.F | 
sed 's/\*\.F//g'|
sed 's/\.F//g'`        # remove the "*.F" that remains

objs=` echo $@ | 
sed 's/\.o//g '`

sources_new=" "
#iterate over the list of objects
for i in $objs
do
    # in the source is to be compiled i.e. it is in the objects list then we
    # will keep it to the next step. Otherwise we disregard it
    if [[ $sources =~ (^|[[:space:]])"$i"($|[[:space:]]) ]]; then
        sources_new+=" ${i}.F"
    fi
done

sources=`echo $sources_new`
if test "$sources" = "" ; then exit ; fi

# files that may contain modules
# extra directories can be specified on the command line
sources_all="$sources"
for dir in $*
do
  sources_all="$sources_all `echo $dir/*.F`"
done
sources_all=`echo $sources_all |
sed 's/[^ ]*\*\.F//g'`     # remove the "dir/*.F" that remain
#                            # when there are no such files

rm -f moduledep.tmp1 moduledep.tmp2 # destroy previous contents

# create list of module dependencies
# each line is of the form:
# file_name.o : @module_name@
# cast all module names to lowercase because Fortran is case insensitive
egrep -H -i -e "include ?<memory.h>" -e "^ *use " $sources |  # look for "USE name"
sed 's/F:/o /
     s/,/ /;s/#include/ use /;s/<memory.h>/memory/' | # replace extension, insert space
#                                         #   and remove trailing comma
awk '{print $1 " : @" tolower($3) "@"}' | # create dependency entry
sort | uniq > moduledep.tmp1              # remove duplicates

# create list of available modules
# for each module, create a line of the form:
# s/@module_name@/file_name/g
egrep -H -i "^ *module " $sources_all |           # look for "MODULE name"
sed 's/F:/o /
     s/\//\\\//g' |                            # replace extension, insert
#                                              #   space and escape slashes
awk '{print "s/@" tolower($3) "@/" $1 "/" }' | # create substitution line
sort | uniq > moduledep.tmp2                   # remove duplicates

# replace module names with file names
# by applying the file of substitution patterns just created
sed -f moduledep.tmp2 moduledep.tmp1 |
awk '{if ($1 != $3) print}' |          # remove self dependencies
sort  | uniq |                        # remove duplicates
sed 's/@.*@//'

rm -f moduledep.tmp1 moduledep.tmp2 # remove temporary files
