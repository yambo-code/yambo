#!/bin/bash 
#
# License-Identifier: GPL
#
# Copyright (C) 2017 The Yambo Team
#
# Authors (see AUTHORS file for details): AF
#
cat $1 | sed -e 's/NEWLINE/\
 /g' |  sed -e 's/\. *not *\./\.not\./g' | sed -e 's/QUOTES\ /\"/g' | sed -e 's/\ QUOTES/\"/g'  >  $1"_space"
rm $1
