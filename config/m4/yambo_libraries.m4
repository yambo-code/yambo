#
#        Copyright (C) 2000-2021 the YAMBO team
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
#
AC_DEFUN([AC_YAMBO_LIBRARIES],[

DRIVER_INCS="-I$PWD/lib/yambo/driver/include/ -I$PWD/include/driver"
AC_SUBST(DRIVER_INCS)

if [[ "$compdir" != "$srcdir" ]] && [[ "$srcdir" != "." ]] ; then
 if test ! -d "$compdir/lib/" ;      then mkdir  $compdir/lib/                   ; fi
 if test ! -d "$compdir/lib/yambo" ; then cp -r  $srcdir/lib/yambo $compdir/lib/ ; fi
fi

if test -f ".git" || test -d ".git"; then
  #
  # git procedure
  #
  AC_MSG_CHECKING([the yambo-libraries git repository])
  cd lib/
  if ! test -d "yambo/driver/src"; then
    git clone git@github.com:yambo-code/yambo-libraries.git yambo >& /dev/null
  else
    cd yambo
    git checkout master >& /dev/null
    git pull >& /dev/null
    cd ../
  fi
  m4_include([lib/yambo/driver/config/version.m4])
  AC_MSG_RESULT([@ version $YDRI_VERSION.$YDRI_SUBVERSION.$YDRI_PATCHLEVEL])
  #AC_MSG_CHECKING([the libraries archive ])
  #cp yambo/external/*.gz  $compdir/lib/archive
  cd ../
  #AC_MSG_RESULT(populated)
  #
else
  #
  # direct download procedure
  #
  make download
  TARBALL=`find lib/archive -name 'Ydriver*'`
  AC_MSG_NOTICE([Extracting yambo internal librar(ies)])
  if ! test -d "lib/yambo/driver/src"; then
    AC_MSG_CHECKING([the internal library ${TARBALL}])
    tar -xzf $TARBALL
    rm   -rf lib/yambo
    mkdir -p lib/yambo
    mv yambo-libraries-*/* lib/yambo
    rm -rf yambo-libraries-*
    cd lib/yambo
    m4_include([lib/yambo/driver/config/version.m4])
    AC_MSG_RESULT([@ version $YDRI_VERSION.$YDRI_SUBVERSION.$YDRI_PATCHLEVEL])
    cd ../../
  fi 
  #
fi


])
