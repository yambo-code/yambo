#
#        Copyright (C) 2000-2017 the YAMBO team
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
AC_DEFUN([AC_PETSC_SLEPC_SETUP],[
#
AC_ARG_WITH(petsc_libs,
        [AC_HELP_STRING([--with-petsc-libs=<libs>], [Use PETSC libraries <libs> or leave empty to use internal lib],[32])])
AC_ARG_WITH(slepc_libs,
        [AC_HELP_STRING([--with-slepc-libs=<libs>], [Use SLEPC libraries <libs> or leave empty to use internal lib],[32])])
#
PETSC_LIBS=""
SLEPC_LIBS=""
dslepc=""
enable_slepc="no"
enable_petsc="no"
compile_petsc="no"
compile_slepc="no"
#
case $with_petsc_libs in
        yes ) compile_petsc="yes";;
        no) acx_petsc_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) PETSC_LIBS="$with_petsc_libs" ;;
        *) PETSC_LIBS="$with_petsc_libs" ;;
esac
#
case $with_slepc_libs in
        yes ) compile_slepc="yes";;
        no) acx_slepc_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) SLEPC_LIBS="$with_slepc_libs" ;;
        *)  PETSC_LIBS="$with_petsc_libs" ;;
esac
#
if test "$compile_petsc" = "yes" &&  test "$compile_slepc" = "yes"  ; then
  enable_slepc="yes"
  enable_petsc="yes"
  dslepc="-D_PETSC"
else 
 if ! test "x$PETSC_LIBS" = "x" &&  ! test "x$SLEPC_LIBS" = "x" ; then
  enable_slepc="yes"
  enable_petsc="yes"
  dslepc="-D_PETSC"
 fi
fi
#
AC_SUBST(PETSC_LIBS)
AC_SUBST(PETSC_INCS)
AC_SUBST(SLEPC_LIBS)
AC_SUBST(SLEPC_INCS)
AC_SUBST(enable_slepc)
AC_SUBST(compile_slepc)
AC_SUBST(compile_petsc)
#
])
