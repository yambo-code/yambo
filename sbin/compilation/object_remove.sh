#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2022 The Yambo Team
#
# Authors (see AUTHORS file for details): AM DS
#
mode=$1
#
if [[ "$file_o" == "driver.o" ]] ; then
 source ./sbin/compilation/verbosity.sh "object_remove.sh: WARNING driver.o case is problematic."
 source ./sbin/compilation/verbosity.sh "object_remove.sh: mode is $mode. Skipping it"
 source ./sbin/compilation/verbosity.sh "object_remove.sh: driver.o is created at the linking step. See config/mk/local/functions.mk, define_link"
 return
fi
#
if [ ! -z "$file_o_src" ]  ; then
 file_o_path=`dirname $file_o_src`
 ldir=`dirname $file_o_src`
 llib=`basename $ldir`
else
 ldir=`dirname $file_src`
 llib=`basename $ldir`
 source ./sbin/compilation/stamp_remove.sh "lib"
 return
fi
#
if [[  "$file_o_path" == *"${dir/\./}" ]] || [ "$2" == "locks" ]; then
 source ./sbin/compilation/stamp_remove.sh "lib"
fi
#
if [[ -f "$file_o_src" ]]  ; then
  #
  if [[  "$file_o_path" == *"${dir/\./}" ]]; then 
   source ./sbin/compilation/verbosity.sh "object_remove.sh: remove $file_o_src"
   rm -f $file_o_src  
  fi
  if [[ -f "$file_f90_src" ]] ; then 
   source ./sbin/compilation/verbosity.sh "object_remove.sh: remove $file_f90_src"
   rm -f $file_f90_src 
  fi
  #
  if [[ "$2" == "locks" ]] && [[  ! "$file_o_path" == *"${dir/\./}" ]]; then
   source ./sbin/compilation/verbosity.sh "object_remove.sh: $file_o_src -> ${file_o_src}_to_save"
   mv $file_o_src ${file_o_src}_to_save
  fi
  #
  # In sources mode remove all corresponding saved objects
  #
  if [ "$2" == "sources" ]; then
    #
    if [[ -f "$file_src" ]] ; then
     source ./sbin/compilation/verbosity.sh "object_remove.sh: touch $file_src"
     touch $file_src
    fi
    #
    count=`ls -1 $file_o_path/*objects.save/*.a 2>/dev/null | wc -l`
    if [ $count != 0 ]; then 
     source ./sbin/compilation/verbosity.sh "object_remove.sh: remove objects.save .a files ($count)"
     rm -f $file_o_path/*objects.save/*.a 
    fi
    file_o_base=`basename $file_o`
    count=`ls -1 $file_o_path/*objects.save/$file_o_base 2>/dev/null | wc -l`
    if [ $count != 0 ] ; then
      source ./sbin/compilation/verbosity.sh "object_remove.sh: remove objects.save $file_o_base ($count)"
      rm -f $file_o_path/*objects.save/$file_o_base
    fi
    if [ ! "$file_f90" == "" ]; then
      file_f90_base=`basename $file_f90`
      count=`ls -1 $file_o_path/*objects.save/$file_f90_base 2>/dev/null | wc -l`
      if [ $count != 0 ] ; then
        source ./sbin/compilation/verbosity.sh "object_remove.sh: remove objects.save $file_f90_base ($count)"
        rm -f $file_o_path/*objects.save/$file_f90_base
      fi
    fi
  fi
fi
#
# Remove corresponding mod files
#
if [[ -f "$file_src" ]]  ; then
  modfile=`grep -i module $file_src | grep -i -v end | grep -i -v use | grep -i -v procedure | grep -v !`
  source ./sbin/compilation/verbosity.sh "object_remove.sh: modules to remove originated by $file_src: $modfile"
  for modname_tmp in $modfile; do
    # Global
    modname=`echo "$modname_tmp" | tr '[:upper:]' '[:lower:]'`
    if [[ -f "$compdir/include/$modname.mod" ]]; then
      source ./sbin/compilation/verbosity.sh "object_remove.sh: remove $compdir/include/$modname.mod"
      rm -f $compdir/include/$modname.mod
    fi
    # Local
    if [[ -f "$file_o_path/$modname.mod" ]]; then
      #
      if [[  "$file_o_path" == *"${dir/\./}" ]]; then
       source ./sbin/compilation/verbosity.sh "object_remove.sh: remove $file_o_path/$modname.mod"
       rm -f $file_o_path/$modname.mod ;
      fi
      #
      if [ "$2" == "locks" ] && [[  ! "$file_o_path" == *"${dir/\./}" ]]; then
       source ./sbin/compilation/verbosity.sh "object_remove.sh: $file_o_path/$modname.mod -> $file_o_path/$modname.mod_to_save"
       mv $file_o_path/$modname.mod $file_o_path/$modname.mod_to_save
      fi
      #
      # In sources mode remove all corresponding saved modules
      #
      if [ "$2" == "sources" ]; then
        count=`ls -1 $file_o_path/*objects.save/$modname.mod 2>/dev/null | wc -l`
        if [ $count != 0 ]; then
         source ./sbin/compilation/verbosity.sh "object_remove.sh: remove $file_o_path/*objects.save/$modname.mod"
         rm $file_o_path/*objects.save/$modname.mod
        fi
      fi
    fi
  done
  #
fi
