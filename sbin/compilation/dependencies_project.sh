#!/bin/bash
#
#        Copyright (C) 2000-2022 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM DS
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
# dependencies.sh -- script that computes dependencies on Fortran 90 modules/projects
# modified from the moduledep.sh distributed with Quantum ESPRESSO and added the project part
#
cd $srcdir/$CDIR
#
# Sources to process
#====================
sources=" "
if test `find . -maxdepth 1 -name '*.F' | wc -l` -ge 1 ; then
 sources+=`echo *.F`
fi
sources+=" "
if test `find . -maxdepth 1 -name '*.c' | wc -l` -ge 1 ; then
 sources+=`echo *.c`
fi
#
sources=$sources
if test `find . -maxdepth 1 -name '*.f' | wc -l` -ge 1 ; then
 sources+=`echo *.f`
fi
#
if [ ${#sources} -eq 2 ]; then
  cd $BASE
  continue 
fi
#
# Projects 
#==========
for PJ in _SC _RT _RT_SCATT _ELPH _PHEL _NL _QED _YPP_ELPH _YPP_RT _YPP_NL _YPP_SC _yambo _ypp _a2y _p2y _c2y _DOUBLE
do
 sources_pj_dependent=""
 for file in $sources
 do
  if [ "$PJ" == "_DOUBLE" ] ; then
    obj=`echo $file| sed 's/\.F/\.o/g'| sed 's/\.c/\.o/g'| sed 's/\.f/\.o/g'`
    sources_pj_dependent+=" ${obj}\n"
  elif [ "$PJ" == "_RT"  ] ; then
    if test `grep $PJ $file | grep '#' | grep -v _RT_SCATT | wc -l` -ge 1 ; then
      obj=`echo $file| sed 's/\.F/\.o/g'| sed 's/\.c/\.o/g'| sed 's/\.f/\.o/g'`
      sources_pj_dependent+=" ${obj}\n"
    fi
  elif [ "$PJ" == "_ELPH"  ] ; then
    if test `grep $PJ $file | grep '#' | grep -v _ELPH_ITERATIVE | wc -l` -ge 1 ; then
      obj=`echo $file| sed 's/\.F/\.o/g'| sed 's/\.c/\.o/g'| sed 's/\.f/\.o/g'`
      sources_pj_dependent+=" ${obj}\n"
    fi
  elif [ "$PJ" == "_SC"  ] ; then
    if test `grep $PJ $file | grep '#' | grep -v _SCALAPACK | wc -l` -ge 1 ; then
      obj=`echo $file| sed 's/\.F/\.o/g'| sed 's/\.c/\.o/g'| sed 's/\.f/\.o/g'`
      sources_pj_dependent+=" ${obj}\n"
    fi
  elif [ "$PJ" == "_RT_SCATT"  ] ; then
    test1=`grep _RT_SCATT $file | grep '#' | wc -l`
    test2=`grep _ELPH_ITERATIVE $file | grep '#' | wc -l`
    if [ "$test1" -ge 1 ] || [ "$test2" -ge 1  ] ; then
      obj=`echo $file| sed 's/\.F/\.o/g'| sed 's/\.c/\.o/g'| sed 's/\.f/\.o/g'`
      sources_pj_dependent+=" ${obj}\n"
    fi
  elif test `grep $PJ $file | grep '#' | wc -l` -ge 1 ; then
    obj=`echo $file| sed 's/\.F/\.o/g'| sed 's/\.c/\.o/g'| sed 's/\.f/\.o/g'`
    sources_pj_dependent+=" ${obj}\n"
  fi
 done
 PREFIX=`echo $PJ | sed 's/_//'`
 if [ ${#sources_pj_dependent} -gt 1 ]; then
  echo -e "$sources_pj_dependent" >>  $compdir/$CDIR/${PREFIX}_project.dep
 fi
done
#
cd $BASE
#
