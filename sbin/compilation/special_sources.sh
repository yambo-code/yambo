#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2006 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
F77_NOOPT_SRC="xerbla.o slamch.o"
FC_LOCAL_SRC="sgfft.o"
FC_NOOPT_SRC="mod_parser_m.o mod_logo.o"
for arg in $ARGS 
do
 case $arg in
  -D_PGI)
   FC_NOOPT_SRC="$FC_NOOPT_SRC bz_samp_indexes.o" 
   ;;
 esac
done
