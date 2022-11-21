#
#        Copyright (C) 2000-2022 the YAMBO team
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
AC_DEFUN([AC_YAMBO_LIBRARIES],[

DRIVER_INCS="-I$PWD/lib/yambo/driver/include/ -I$PWD/include/driver"
#
AC_ARG_WITH(yambo-libs-branch,[AS_HELP_STRING([--with-yambo-libs-branch=<branch>],[Use the <branch> of the yambo-libraries repository.],[32])],,[with_yambo_libs_branch=master])
#
if test x"$with_yambo_libs_branch" = "xnone"; then
 Ydriver_check="D"; 
 Ydriver_LIB="Download"
else
 Ydriver_check="G"; 
 Ydriver_LIB="GIT, branch $with_yambo_libs_branch"
fi
AC_SUBST(Ydriver_LIB)
AC_SUBST(Ydriver_check)
AC_SUBST(DRIVER_INCS)
AC_SUBST(with_yambo_libs_branch)
])
