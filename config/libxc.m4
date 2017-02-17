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
AC_DEFUN([KH_PATH_LIBXC_F90],[

AC_ARG_WITH(libxc_include,AC_HELP_STRING([--with-libxc-include=<path>],
                                  [Path of the LibXC include directory]))
AC_ARG_WITH(libxc_lib,AC_HELP_STRING([--with-libxc-lib=<path>],
                                  [Path of the LibXC lib directory]))
libxc="no"
dlibxc=""
XCLIBS=""
if test -d "$with_libxc_include" && test -d "$with_libxc_lib" ; then
 AC_MSG_CHECKING([for LibXC in $with_libxc_lib])
 AC_LANG([Fortran])
 save_fcflags="$FCFLAGS"
 for flag in "-I" "-M" "-p"; do
    FCFLAGS="$flag$with_libxc_include $save_fcflags"
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [use xc_f90_lib_m]),
     [libxc=yes 
      for file in `find $with_libxc_include \( -name 'libxc*mod' -o -name 'xc_*mod' \) `; do
       cp $file include/ 
      done
      for file in `find $with_libxc_lib -name 'libxc*.a'`; do
       cp $file lib/ 
      done
     ], [libxc=no])
    FCFLAGS="$save_fcflags"
    if test "x$libxc" = xyes; then
     AC_MSG_RESULT([yes])
     XCLIBS="-lxc"
     dlibxc="-D_LIBXC"
     break
    fi
 done
 if test "x$libxc" = xno; then
  AC_MSG_RESULT([no])
 fi
fi
AC_SUBST(XCLIBS)
AC_SUBST(libxc)
AC_SUBST(dlibxc)
])
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
AC_DEFUN([KH_PATH_LIBXC_F90],[

AC_ARG_WITH(libxc_include,AC_HELP_STRING([--with-libxc-include=<path>],
                                  [Path of the LibXC include directory]))
AC_ARG_WITH(libxc_lib,AC_HELP_STRING([--with-libxc-lib=<path>],
                                  [Path of the LibXC lib directory]))
libxc="no"
dlibxc=""
XCLIBS=""
if test -d "$with_libxc_include" && test -d "$with_libxc_lib" ; then
 AC_MSG_CHECKING([for LibXC in $with_libxc_lib])
 AC_LANG([Fortran])
 save_fcflags="$FCFLAGS"
 for flag in "-I" "-M" "-p"; do
    FCFLAGS="$flag$with_libxc_include $save_fcflags"
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [use xc_f90_lib_m]),
     [libxc=yes 
      for file in `find $with_libxc_include \( -name 'libxc*mod' -o -name 'xc_*mod' \) `; do
       cp $file include/ 
      done
      for file in `find $with_libxc_lib -name 'libxc*.a'`; do
       cp $file lib/ 
      done
     ], [libxc=no])
    FCFLAGS="$save_fcflags"
    if test "x$libxc" = xyes; then
     AC_MSG_RESULT([yes])
     XCLIBS="-lxc"
     dlibxc="-D_LIBXC"
     break
    fi
 done
 if test "x$libxc" = xno; then
  AC_MSG_RESULT([no])
 fi
fi
AC_SUBST(XCLIBS)
AC_SUBST(libxc)
AC_SUBST(dlibxc)
])
