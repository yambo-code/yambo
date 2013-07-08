#
# autoconf macro for detecting LibXc module file
#
# Copyright (C) 2010 C. Attaccalite and the YAMBO team
#              http://www.yambo-code.org
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
AC_DEFUN([ACX_LIBXC],[

have_configured="no"

AC_MSG_CHECKING([the configuration of the LIBXC internal library])
cd lib/libxc
if test -f Makefile; then 
have_configured="yes"
else
./configure FC=$F90 CC=$CC --prefix=$PWD/../../ >&/dev/null
if test -f Makefile; then have_configured="yes";fi
fi
cd ../../
AC_MSG_RESULT($have_configured)

if test "x$have_configured" != xyes; then AC_MSG_ERROR([can't configure LIBXC ]); fi

])
