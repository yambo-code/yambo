#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2018 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
if test -f $compdir/config/stamps_and_lists/${goal}.stamp; then
 candidates=`find $srcdir/$dir -type f -name "*.F" -newer $compdir/config/stamps_and_lists/${goal}.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.f" -newer $compdir/config/stamps_and_lists/${goal}.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.c" -newer $compdir/config/stamps_and_lists/${goal}.stamp`
fi
if test -f $compdir/config/stamps_and_lists/${target}.stamp; then
 candidates=`find $srcdir/$dir -type f -name "*.F" -newer $compdir/config/stamps_and_lists/${target}.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.f" -newer $compdir/config/stamps_and_lists/${target}.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.c" -newer $compdir/config/stamps_and_lists/${target}.stamp`
fi
if test -f $compdir/config/stamps_and_lists/${target}.a.stamp; then
 candidates=`find $srcdir/$dir -type f -name "*.F" -newer $compdir/config/stamps_and_lists/${target}.a.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.f" -newer $compdir/config/stamps_and_lists/${target}.a.stamp`
 candidates+=`find $srcdir/$dir -type f -name "*.c" -newer $compdir/config/stamps_and_lists/${target}.a.stamp`
fi
if [ ! -z "$candidates" ]; then
 source ./sbin/compilation/verbosity.sh "check_updated_sources.sh: there are sources to check"
fi
for candidate in $candidates
do
  source ./sbin/compilation/verbosity.sh "check_updated_sources.sh: search for $candidate childs"
  source ./sbin/compilation/name_me.sh $candidate "no_search"
  DIR_is_to_recompile=1
  source ./sbin/compilation/check_object_childs.sh "sources"
done
