#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2006 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
for arg in $ARGS 
do
 case $arg in
  -D_MPI)
   precomp_mpi="yes" ;
   precomp_flags="$precomp_flags $arg"
   ;;
  -D_*) 
   precomp_flags="$precomp_flags $arg"
   ;;
 esac
done
#
sorted_precomps=$(echo "$precomp_flags"|tr " " "\n"|sort|uniq|tr "\n" " ")
