#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2023 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
VERB=0
#
if [ "$VERB" == 0 ] ; then return; fi
#
# Log files
if [ "$1" == "init" ] ; then
 mkdir -p compilation_log
 vlog="$compdir/compilation_log/todo.log"
 if [ "$mode" == "l" ]; then vlog="$compdir/compilation_log/do.log"; fi
 return
fi
#
if [ "$1" == "options" ] ; then
 echo "####################################################################################" >> $vlog
 echo "${1}: current dir is $cdir" >> $vlog
 echo "${1}: mode is $mode" >> $vlog
 if [ ! -z $new ]; then echo "${1}: NEW procedure" >> $vlog; fi
 if [ ! -z $dep ]; then echo "${1}: Deps procedure" >> $vlog; fi
 if [ ! -z $target ]; then echo "${1}: target is $target" >> $vlog; fi
 if [ ! -z $goal ]; then echo "${1}: goal is $goal" >> $vlog; fi
 if [ ! -z $ofile ]; then echo "${1}: ofile is $ofile" >> $vlog; fi
 if [ ! -z $global ]; then echo "${1}: global compilation" $vlog; fi
 if [ ! -z $lib ]; then echo "${1}: lib is $lib" >> $vlog; fi
 if [ ! -z $NARG ]; then echo "${1}: ARG is $NARG" >> $vlog; fi
 return
fi
#
if [ "$1" == "locks" ] ; then
 echo "DIR        " $dir >> $vlog
 echo "LOCKS(old) " $lock_string >> $vlog
 echo "LOCKS(new) " $flag_string >> $vlog
 echo "MISSING    " $missing >> $vlog
 echo "NEW        " $new >> $vlog
 echo "SAVE       " $save_dir >> $vlog
 echo "RESTORE    " $restore_dir >> $vlog
 return
fi
#
if [ "$1" == "filename" ] ; then
 echo                              "file        " $file   >> $vlog
 if [ ! -z $file_o ];   then  echo ".o          " $file_o_src >> $vlog; fi
 if [ ! -z $file_c ];   then  echo ".c          " $file_c_src >> $vlog; fi
 if [ ! -z $file_f ];   then  echo ".f          " $file_f_src >> $vlog; fi
 if [ ! -z $file_f90 ]; then  echo ".f90        " $file_f90_src >> $vlog; fi
 if [ ! -z $file_F ];   then  echo ".F          " $file_F_src >> $vlog; fi
 if [ ! -z $file_src ]; then  echo "(file_src)  " $file_src >> $vlog; fi
 return
fi
#
echo "$1" >> $vlog
#
