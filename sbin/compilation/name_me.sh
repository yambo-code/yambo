#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2023 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
# Take the basaname
#
if [ "$2" == "search" ]; then
 file=`basename $1`
else
 file=$1
fi
#
# Different kinds
#
file_o=`echo $file | sed 's/.$/o/'`
file_F=`echo $file | sed 's/.$/F/'`
file_f=`echo $file | sed 's/.$/f/'`
file_f90=`echo $file | sed 's/.$/f90/'`
file_c=`echo $file | sed 's/.$/c/'`
file_src=""
#
if [[ ! "$compdir" == "$srcdir" ]] ; then  
 c_string=`echo $compdir | sed 's/\//_SLASH_/g' | sed 's/\./_DOT_/g'`
 s_string=`echo $srcdir  | sed 's/\//_SLASH_/g' | sed 's/\./_DOT_/g'`
fi
#
if [ "$2" == "search" ]; then
 file_F_src=`find $srcdir -name $file_F -not -path "/*objects.save/*" -not -path "*archive*" -not -path "*$IO_no_dir*"`
else
 file_F_src=$file_F
fi
if [ ! -z $file_F_src ]; then 
 file_src=$file_F_src
 file_o_src=`echo $file_F_src | sed 's/\.F/\.o/'`
 if [[ ! "$compdir" == "$srcdir" ]] ; then  
  file_o_src=`echo $file_o_src | sed 's/\//_SLASH_/g' | sed 's/\./_DOT_/g'`
  file_o_src=`echo $file_o_src | sed "s/$s_string/$c_string/g"`
  file_o_src=`echo $file_o_src | sed 's/_SLASH_/\//g' | sed 's/_DOT_/\./g'`
 fi
 if [ -z $file_o_src ] ; then file_o=""; fi
 file_f90_src=`echo $file_F_src | sed 's/.F/.f90/'`
 if [ -z $file_f90_src ] ; then file_f90=""; fi
 file_c=""
 file_c_src=""
 file_f=""
 file_f_src=""
 source ./sbin/compilation/verbosity.sh "filename"
 return
fi
#
if [ "$2" == "search" ]; then
 file_c_src=`find $srcdir -name $file_c -not -path "/*objects.save/*" -not -path "*archive*" -not -path "*$IO_no_dir*"`
else
 file_c_src=$file_c
fi
if [ ! -z $file_c_src ]; then 
 file_src=$file_c_src
 file_o_src=`echo $file_c_src | sed 's/\.c/\.o/'`
 if [[ ! "$compdir" == "$srcdir" ]] ; then  
  file_o_src=`echo $file_o_src | sed 's/\//_SLASH_/g' | sed 's/\./_DOT_/g'`
  file_o_src=`echo $file_o_src | sed "s/$s_string/$c_string/g"`
  file_o_src=`echo $file_o_src | sed 's/_SLASH_/\//g' | sed 's/_DOT_/\./g'`
 fi
 if [ -z $file_o_src ] ; then file_o=""; fi
 file_F=""
 file_F_src=""
 file_f90=""
 file_f90_src=""
 file_f=""
 file_f_src=""
 source ./sbin/compilation/verbosity.sh "filename"
 return
fi
#
if [ "$2" == "search" ]; then
 file_f_src=`find $srcdir -name $file_f -not -path "/*objects.save/*" -not -path "*archive*" -not -path "*$IO_no_dir*"`
else
 file_f_src=$file_f
fi
if [ ! -z $file_f_src ]; then 
 file_src=$file_f_src
 file_o_src=`echo $file_f_src | sed 's/\.f/\.o/'`
 if [[ ! "$compdir" == "$srcdir" ]] ; then  
  file_o_src=`echo $file_o_src | sed 's/\//_SLASH_/g' | sed 's/\./_DOT_/g'`
  file_o_src=`echo $file_o_src | sed "s/$s_string/$c_string/g"`
  file_o_src=`echo $file_o_src | sed 's/_SLASH_/\//g' | sed 's/_DOT_/\./g'`
 fi
 if [ -z $file_o_src ] ; then file_o=""; fi
 file_F=""
 file_F_src=""
 file_f90=""
 file_f90_src=""
 file_c=""
 file_c_src=""
 source ./sbin/compilation/verbosity.sh "filename"
 return
fi
#
