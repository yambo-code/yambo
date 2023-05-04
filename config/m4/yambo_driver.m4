#
#        Copyright (C) 2000-2023 the YAMBO team
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
AC_DEFUN([AC_YDRIVER],[

DRIVER_INCS="-I$PWD/lib/yambo/Ydriver/include/ -I$PWD/include/driver"
#
AC_ARG_WITH(ydriver-branch,[AS_HELP_STRING([--with-ydriver-branch=<branch>],[Use the <branch> of the ydriver repository.],[32])],,[with_ydriver_branch=none])
#
if test x"$with_ydriver_branch" = "xnone"; then
 Ydriver_check="D"; 
 Ydriver_LIB="Download"
else
 Ydriver_check="G"; 
 Ydriver_LIB="GIT, branch $with_ydriver_branch"
fi
AC_SUBST(Ydriver_LIB)
AC_SUBST(Ydriver_check)
AC_SUBST(DRIVER_INCS)
AC_SUBST(with_ydriver_branch)
])
