#
#        Copyright (C) 2000-2014 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AF
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
AC_DEFUN([AC_HAVE_IOTK],[

AC_ARG_WITH(iotk, AC_HELP_STRING([--with-iotk=<yes/no>],
            [Whether to use the IOTK library]),[],[with_iotk="yes"])
AC_ARG_WITH(iotk_path, AC_HELP_STRING([--with-iotk-path=<path>],
            [Path of the IOTK library]),[],[])
AC_ARG_WITH(iotk_include, AC_HELP_STRING([--with-iotk-include=<path>],
            [Inlcude path for the IOTK library]),[],[])


compile_p2y="no"
compile_iotk="no"
iotk_idir=" "
IOTK_LIBS=" "

if test "x$with_iotk" = "xyes" ; then
  #
  if ! test "x$with_iotk_path" = "x" ; then
    AC_MSG_CHECKING([for IOTK in $with_iotk_path])
    # external iotk
    if test -r $with_iotk_path/src/libiotk.a ; then
      compile_p2y="yes"
      compile_iotk="no"
      if ! test "x$with_iotk_include" = "x" ; then
         iotk_idir="-I$with_iotk_include"
      else
         iotk_idir="-I$with_iotk_path/src"
      fi
      #
      IOTK_LIBS="-liotk"
      cp "$with_iotk_path/src/libiotk.a" lib/
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no])
    fi
  else
    # internal iotk
    compile_iotk="yes"
  fi
  #
  # internal iotk
  if test "x$compile_iotk" = "xyes" ; then
    AC_MSG_CHECKING([for IOTK library])
    compile_p2y="yes"
    iotk_idir=" "
    IOTK_LIBS="-liotk"
    AC_MSG_RESULT(Internal)
  fi
else
  AC_MSG_CHECKING([for IOTK library])
  AC_MSG_RESULT([no])
fi
#
AC_SUBST(compile_p2y)
AC_SUBST(compile_iotk)
AC_SUBST(iotk_idir)
AC_SUBST(IOTK_LIBS)
AC_CONFIG_FILES([lib/install/make_iotk.inc])

])
