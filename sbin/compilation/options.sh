#!/bin/bash
#
#        Copyright (C) 2000-2022 the YAMBO team
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
while getopts ":d:t:o:m:nDGN:g:" opt; do
 case $opt in
  d) cdir=${OPTARG}
  ;;
  t) target=${OPTARG}
  ;;
  g) goal=${OPTARG}
  ;;
  o) ofile=${OPTARG}
  ;;
  m) mode=${OPTARG}
  ;;
  n) new="yes"
  ;;
  N) NARG=${OPTARG}
  ;;
  G) global="yes"
  ;;
  D) dep="yes"
  ;;
  ?) shift
  ;;
 esac 
done
N=1
if [ "${NARG:0:2}" == "-j" ]; then
 N=`echo ${N:0:4} | sed s/\-j//`
fi
if [[ "${NARG}" == "" ]] || [[ "${NARG}" == "-j" ]] ; then 
 N=`grep -c ^processor /proc/cpuinfo`
fi
shift $((OPTIND -1))
