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
AC_DEFUN([AC_HAVE_ETSF_IO],[

AC_ARG_ENABLE(etsf_io, AC_HELP_STRING([--enable-etsf-io],
   [Activate the ETSF_IO support]),[],[enable_etsf_io="no"])
AC_ARG_WITH(etsf_io_path, AC_HELP_STRING([--with-etsf-io-path=<path>],
   [Path to the ETSF_IO install directory]))
AC_ARG_WITH(etsf_io_libdir, AC_HELP_STRING([--with-etsf-io-libdir=<path>],
   [Path to the ETSF_IO lib directory]))
AC_ARG_WITH(etsf_io_includedir, AC_HELP_STRING([--with-etsf-io-includedir=<path>],
   [Path to the ETSF_IO include directory]))

compile_e2y="no"
compile_etsf="no"
etsf_libdir=" "
etsf_idir=" "
ETSF_LIBS=" "

if ! test "x$with_etsf_io_path" = "x" ;    then enable_etsf_io=yes ; fi
if test -d "$with_etsf_io_includedir" && test -d "$with_etsf_io_libdir" ; then
        enable_etsf_io=yes 
fi
#
# main search
#
if test "x$enable_etsf_io" = "xyes" ; then
  #
  if ! test "x$with_etsf_io_path" = "x" ; then
    AC_MSG_CHECKING([for ETSF_IO in $with_etsf_io_path])
    # check for external lib
    #
    if test -r $with_etsf_io_path/lib/libetsf_io.a ; then
      compile_e2y="yes"
      compile_etsf="no"
      etsf_idir="-I$with_etsf_path/include"
      ETSF_LIBS="-letsf_io"
      #
      for file in `find $with_etsf_io_path/include \( -name '*etsf_io*' -o -name '*typesizes*' \) `; do
         cp $file include/
      done
      for file in `find $with_etsf_io_path/lib -name '*etsf_io*.a'`; do
         cp $file lib/
      done
      #
      AC_MSG_RESULT([yes])
    #else
    #  compile_etsf="yes"
    fi
  elif test -d "$with_etsf_io_includedir" && test -d "$with_etsf_io_libdir" ; then
    # check for external lib (second option)
    AC_MSG_CHECKING([for ETSF_IO in $with_etsf_io_libdir])
    if test "$netcdf" = "yes"; then
       if test -r $with_etsf_io_libdir/libetsf_io.a ; then
          #
          compile_e2y="yes"
          compile_etsf=no
          etsf_idir="-I$with_etsf_io_libdir"
          ETSF_LIBS="-letsf_io"
          #
          for file in `find $with_etsf_io_includedir \( -name '*etsf_io*' \) `; do
             cp $file include/
          done
          for file in `find $with_etsf_io_libdir -name '*etsf_io*.a'`; do
             cp $file lib/
          done
          #
          AC_MSG_RESULT([yes])
       else
          AC_MSG_RESULT([no])
       fi
    else
      AC_MSG_RESULT([no])
      AC_MSG_WARN([ETSF_IO requires NETCDF. ETSF_IO support removed.])
    fi
  else
    AC_MSG_CHECKING([for ETSF_IO Library])
    # internal etsf_io
    compile_etsf="yes"
  fi

  #
  # internal etsf_io
  if test "x$compile_etsf" = "xyes" ; then
    compile_e2y="yes"
    etsf_idir=" "
    ETSF_LIBS="-letsf_io"
    AC_MSG_RESULT(Internal)
  fi
  #
else
  AC_MSG_CHECKING([for ETSF_IO Library])
  AC_MSG_RESULT([no])
fi
#
AC_SUBST(compile_e2y)
AC_SUBST(compile_etsf)
AC_SUBST(etsf_idir)
AC_SUBST(ETSF_LIBS)

])
