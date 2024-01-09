#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2018 The Yambo Team
#
# Authors (see AUTHORS file for details): AM DS
#
stamp="none"
if [ $1 == "goal" ] && [ ! -z $goal ] ; then stamp=$compdir/config/stamps_and_lists/${goal}.stamp ; fi
if [ $1 == "target.a" ] && [ ! -z $target ] ; then stamp=$compdir/config/stamps_and_lists/${target}.a.stamp ; fi
if [ $1 == "lib" ] && [ ! -z $llib ] ; then
 if [[ "$file_src" == *"/ypp/$llib"* ]]  ; then
  stamp=$compdir/config/stamps_and_lists/lib_ypp_${llib}.a.stamp;
 else
  stamp=$compdir/config/stamps_and_lists/lib${llib}.a.stamp;
 fi
fi
if [ $1 == "exe" ] ; then stamp=`find $compdir/config/stamps_and_lists/ -name "compiling*"  | sed "s/compiling_//"`; fi
source ./sbin/compilation/verbosity.sh "stamp_remove.sh (mode $1) running on $dir: remove $stamp"
rm -f $stamp
