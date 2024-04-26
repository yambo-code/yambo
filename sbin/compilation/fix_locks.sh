#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2006 The Yambo Team
#
# Authors (see AUTHORS file for details): DS
#
#
for lock in $missing ; do
 #
 # Remove old lock 
 #
 source ./sbin/compilation/verbosity.sh "fix_locks.sh: rm -f $dir/$lock.lock"
 rm -f $dir/$lock.lock
 #
done
#
for lock in $new ; do
 #
 # Add new lock 
 #
 source ./sbin/compilation/verbosity.sh "fix_locks.sh: touch  $dir/$lock.lock"
 touch $dir/$lock.lock
 #
done
#
