#
#        Copyright (C) 2000-2018 the YAMBO team
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
AC_DEFUN([AC_HAVE_ETSF_IO],[

AC_ARG_ENABLE(etsf_io, AC_HELP_STRING([--enable-etsf-io],
   [Activate the ETSF_IO support]),[],[enable_etsf_io="no"])
AC_ARG_WITH(etsf_io_libs, AC_HELP_STRING([--with-etsf-io-libs=<libs>],
   [Use the ETSF_IO libraries in <libs>],[32]))
AC_ARG_WITH(etsf_io_path, AC_HELP_STRING([--with-etsf-io-path=<path>],
   [Path to the ETSF_IO install directory],[32]))
AC_ARG_WITH(etsf_io_libdir, AC_HELP_STRING([--with-etsf-io-libdir=<path>],
   [Path to the ETSF_IO lib directory],[32]))
AC_ARG_WITH(etsf_io_includedir, AC_HELP_STRING([--with-etsf-io-includedir=<path>],
   [Path to the ETSF_IO include directory],[32]))

compile_e2y="no"
compile_etsf="no"
internal_etsf="no"
etsf_libdir=" "
ETSF_INCS=" "
ETSF_LIBS=" "

if test -d "$with_etsf_io_path"  ;  then enable_etsf_io=yes ; fi
if test -d "$with_etsf_io_libdir" ; then enable_etsf_io=yes ; fi
if test x"$with_etsf_io_libs" != "x" ;   then enable_etsf_io=yes ; fi
#
if test x"$netcdf" != "xyes" ; then enable_etsf_io=no ; fi

#
# main search
#
if test "x$enable_etsf_io" = "xyes" ; then
  #
  if test -d "$with_etsf_io_path" || test -d "$with_etsf_io_libdir" ; then
    #
    # external ETSF_IO
    #
    if test -d "$with_etsf_io_path" ;   then AC_MSG_CHECKING([for ETSF_IO in $with_etsf_io_path]) ; fi
    if test -d "$with_etsf_io_libdir" ; then AC_MSG_CHECKING([for ETSF_IO in $with_etsf_io_libdir]) ; fi
    #
    if test -d "$with_etsf_io_path" ; then
        try_libdir=$with_etsf_io_path/lib
        try_incdir=$with_etsf_io_path/include
    fi
    if test -d "$with_etsf_io_libdir"     ; then try_libdir=$with_etsf_io_libdir ; fi
    if test -d "$with_etsf_io_includedir" ; then try_incdir=$with_etsf_io_includedir ; fi
    #
    if test -z "$try_libdir" ; then AC_MSG_ERROR([No lib-dir specified]) ; fi
    if test -z "$try_incdir" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
    #
    #
    if test -r $try_libdir/libetsf_io.a ; then
      compile_e2y="yes"
      internal_etsf="no"
      ETSF_LIBS="$try_libdir/libetsf_io.a"
      ETSF_INCS="$IFLAG$try_incdir"
      #
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no])
    fi
  elif test x"$with_etsf_io_libs" != "x" ; then
    #
    # directly provided lib
    #
    AC_MSG_CHECKING([for ETSF_IO Library using $with_etsf_io_libs])
    internal_etsf="no"
    compile_e2y="yes"
    if test -d "$with_etsf_io_includedir" ; then ETSF_INCS="$IFLAG$with_etsf_io_includedir" ; fi
    ETSF_LIBS="$with_etsf_io_libs"
    AC_MSG_RESULT(yes)
  else
    #
    # internal ETSF_IO
    #
    AC_MSG_CHECKING([for internal ETSF_IO Library])
    internal_etsf="yes"
    compile_e2y="yes"
    ETSF_INCS="${IFLAG}${extlibs_path}/${FCKIND}/${FC}/include"
    ETSF_LIBS="${extlibs_path}/${FCKIND}/${FC}/lib/libetsf_io.a"
    if test -e "${extlibs_path}/${FCKIND}/${FC}/lib/libetsf_io.a"; then
      compile_etsf="no"
      AC_MSG_RESULT(found already compiled)
    else
      compile_etsf="yes"
      AC_MSG_RESULT(to be compiled)
    fi
  fi
  #
else
  AC_MSG_CHECKING([for ETSF_IO Library])
  AC_MSG_RESULT([no])
fi
#
AC_SUBST(compile_e2y)
AC_SUBST(compile_etsf)
AC_SUBST(internal_etsf)
AC_SUBST(ETSF_LIBS)
AC_SUBST(ETSF_INCS)

])
