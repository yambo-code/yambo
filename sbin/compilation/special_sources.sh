#!/bin/bash
#
#        Copyright (C) 2000-2023 the YAMBO team
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
