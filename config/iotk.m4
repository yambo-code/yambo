#
#        Copyright (C) 2000-2016 the YAMBO team
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

AC_ARG_ENABLE(iotk, AC_HELP_STRING([--enable-iotk],
            [Activate the IOTK support]),[],[enable_iotk="yes"])
AC_ARG_WITH(iotk_libs, AC_HELP_STRING([--with-iotk-libs=<libs>],
            [Use the IOTK library in <libs>],[32]),[],[])
AC_ARG_WITH(iotk_path, AC_HELP_STRING([--with-iotk-path=<path>],
            [Path to the IOTK install directory],[32]),[],[])
AC_ARG_WITH(iotk_libdir, AC_HELP_STRING([--with-iotk-libdir=<path>],
            [Path to the IOTK lib directory],[32]))
AC_ARG_WITH(iotk_includedir, AC_HELP_STRING([--with-iotk-includedir=<path>],
            [Path to the IOTK include directory],[32]),[],[])

compile_p2y="no"
compile_iotk="no"
iotk_idir=" "
IOTK_LIBS=" "

if test -d "$with_iotk_path"  ;  then enable_iotk=yes ; fi
if test -d "$with_iotk_libdir" ; then enable_iotk=yes ; fi
if test  x"$with_iotk_libs" != "x" ;  then enable_iotk=yes ; fi
#
# F90 module flag
#
IFLAG=$ax_cv_f90_modflag
if test -z "$IFLAG" ; then IFLAG="-I" ; fi

if test "x$enable_iotk" = "xyes" ; then
  #
  if test -d "$with_iotk_path" || test -d "$with_iotk_libdir" ; then
    #
    # external IOTK
    #
    if test -d "$with_iotk_path" ;   then AC_MSG_CHECKING([for IOTK in $with_iotk_path]) ; fi
    if test -d "$with_iotk_libdir" ; then AC_MSG_CHECKING([for IOTK in $with_iotk_libdir]) ; fi
    #
    if test -d "$with_iotk_path" ; then
        try_libdir=$with_iotk_path/src
        try_incdir=$with_iotk_path/src
    fi
    if test -d "$with_iotk_libdir"  ;    then try_libdir=$with_iotk_libdir ; fi
    if test -d "$with_iotk_includedir" ; then try_incdir=$with_iotk_includedir ; fi
    #
    if test -z "$try_libdir" ; then AC_MSG_ERROR([No lib-dir specified]) ; fi
    if test -z "$try_incdir" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
    #
    # 
    if test -r $try_libdir/libiotk.a ; then
      compile_p2y="yes"
      compile_iotk="no"
      iotk_idir="$IFLAG$try_incdir"
      IOTK_LIBS="$try_libdir/libiotk.a"
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no])
    fi
  elif test x"$with_iotk_libs" != "x" ; then
    #
    # directly provided lib
    #
    AC_MSG_CHECKING([for IOTK Library using $with_iotk_libs])
    compile_p2y="yes"
    compile_iotk="no"
    if test -d "$with_iotk_includedir" ; then iotk_idir="$IFLAG$with_iotk_includedir" ; fi
    IOTK_LIBS="$with_iotk_libs"
    AC_MSG_RESULT(yes)
  else
    #
    # internal IOTK
    #
    AC_MSG_CHECKING([for IOTK library])
    compile_iotk="yes"
    compile_p2y="yes"
    iotk_idir=" "
    IOTK_LIBS="-liotk"
    if test ! -d lib ; then mkdir lib ; fi
    AC_MSG_RESULT(Internal)
    AC_CONFIG_FILES([lib/install/make_iotk.inc])
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
#AC_CONFIG_FILES([lib/install/make_iotk.inc])

])
