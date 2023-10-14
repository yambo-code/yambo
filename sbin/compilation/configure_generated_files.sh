#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2018 The Yambo Team
#
# Authors (see AUTHORS file for details): HM AM
#
# dependencies.sh -- script that computes dependencies on Fortran 90 modules/projects
# modified from the moduledep.sh distributed with Quantum ESPRESSO and added the project part
#
rm -f $compdir/config/stamps_and_lists/autoconf_generated_files.list
in_files=`find . -name '*.in'`
for file in $in_files
do
 cfg_file=`echo $file |sed -E 's/(.*)\.in/\1/'`
 echo "$cfg_file" >> $compdir/config/stamps_and_lists/autoconf_generated_files.list
done
#
rm -f $compdir/config/stamps_and_lists/active_directories.list
for CDIR in $directories
do
 echo "$CDIR" >> $compdir/config/stamps_and_lists/active_directories.list
done
#
