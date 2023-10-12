#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2006 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
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
