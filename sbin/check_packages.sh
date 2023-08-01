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
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' 
#
repeat(){
 for i in {1..90}; do echo -en "$1"; done
 echo
}
#
for app in  install build-essential ca-certificates curl file \
make gcc g++ gfortran git gnupg2 iproute2 \
unzip m4 wget git zlib1g-dev ssh 
do
 res=`apt-cache policy $app |grep Installed|grep none`
 if [ ! -z "$res" ]; then
  MISS="$MISS $app"
 fi
done
if [ ! -z $MISS ] ; then
 repeat "="
 echo -e "${RED}Missing required packages:${NC}"
 repeat "="
 echo $MISS
 echo 
fi
#
for app in   libmkl-avx libmkl-avx2 libmkl-avx512 libmkl-core libmkl-def \
libmkl-dev libmkl-gf-ilp64 libmkl-gf-lp64 libmkl-gnu-thread
do
 res=`apt-cache policy $app |grep Installed|grep none`
 if [ ! -z "$res" ]; then
  MISS="$MISS $app"
 fi
done
repeat "="
echo -e "${BLUE}Missing optional packages:${NC}"
repeat "="
echo $MISS
echo 

