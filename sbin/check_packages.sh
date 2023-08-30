#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
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

