#
#
#        Copyright (C) 2000-2021 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): DS
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
AC_DEFUN([ACX_DEVXLIB], [


dnl Check if the library was given in the command line
dnl if not, use environment variables or defaults
AC_ARG_WITH(devxlib_libs, [AS_HELP_STRING([--with-devxlib-libs=<libs>], 
            [Use devxlib libraries <libs>],[32])])
AC_ARG_WITH(devxlib_path, [AS_HELP_STRING([--with-devxlib-path=<path>], 
            [Path to devxlib install directory],[32])])
AC_ARG_WITH(devxlib_libdir, [AS_HELP_STRING([--with-devxlib-libdir=<path>], 
            [Path to the devxlib lib directory],[32])])
AC_ARG_WITH(devxlib_includedir, [AS_HELP_STRING([--with-devxlib-includedir=<path>], 
            [Path to the devxlib include directory],[32])])

AC_ARG_WITH(devxlib-branch,[AS_HELP_STRING([--with-devxlib-branch=<branch>],[Use the <branch> of the devxlib repository.],[32])],,[with_devxlib_branch=none])


acx_devxlib_ok="no"
internal_devxlib="no"
compile_devxlib="no"

DEVXLIB_info=""

if test -d "$with_devxlib_path"; then
   devxlib_incdir="$with_devxlib_path/include"
   devxlib_libdir="$with_devxlib_path/lib"
fi
if test -d "$with_devxlib_includedir"; then devxlib_incdir="$with_devxlib_includedir" ; fi
if test -d "$with_devxlib_libdir";     then devxlib_libdir="$with_devxlib_libdir" ; fi

DEVXLIB_INCS="$IFLAG$devxlib_incdir"

dnl Backup LIBS and FCFLAGS
acx_devxlib_save_LIBS="$LIBS"
acx_devxlib_save_FCFLAGS="$FCFLAGS"

#This is fake, it is always going to fail
dnl The tests
AC_MSG_CHECKING([for devxlib])

dnl The following program should work with all version of devxlib
testprog="AC_LANG_PROGRAM([],[
    use devxlib
    implicit none

    integer :: i
])"

FCFLAGS="$GPU_FLAGS $LIBCUDA_INCS $LIBROCM_INCS $DEVXLIB_INCS $acx_devxlib_save_FCFLAGS"

# set from environment variable, if not blank
if test ! -z "$DEVXLIB_LIBS"; then
  LIBS="$DEVXLIB_LIBS $LIBCUDA_LIBS $LIBROCM_LIBS"
dnl $acx_devxlib_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_devxlib_ok=yes], [])
fi

# set from --with-devxlib-libs flag
if test x"$acx_devxlib_ok" = xno && test ! -z "$with_devxlib_libs" ; then
  DEVXLIB_LIBS="$with_devxlib_libs"
  LIBS="$DEVXLIB_LIBS $LIBCUDA_LIBS $LIBROCM_LIBS"
dnl $acx_devxlib_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_devxlib_ok=yes], [])
fi

# dynamic linkage, separate Fortran interface
if test x"$acx_devxlib_ok" = xno; then
  DEVXLIB_LIBS="-L$devxlib_libdir -ldevXlib"
  LIBS="$DEVXLIB_LIBS $LIBCUDA_LIBS $LIBROCM_LIBS"
dnl $acx_devxlib_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_devxlib_ok=yes], [])
fi

# static linkage, separate Fortran interface
if test x"$acx_devxlib_ok" = xno; then
  DEVXLIB_LIBS="$devxlib_libdir/libdevXlib.a"
  LIBS="$DEVXLIB_LIBS $LIBCUDA_LIBS $LIBROCM_LIBS"
dnl $acx_devxlib_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_devxlib_ok=yes], [])
fi

# static linkage, combined Fortran interface (devxlib pre-r10730)
if test x"$acx_devxlib_ok" = xno; then
  DEVXLIB_LIBS="$devxlib_libdir/libdevXlib.a"
  LIBS="$DEVXLIB_LIBS $LIBCUDA_LIBS $LIBROCM_LIBS"
dnl $acx_devxlib_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_devxlib_ok=yes], [])
fi

# dynamic linkage, combined Fortran interface (devxlib pre-r10730)
if test x"$acx_devxlib_ok" = xno; then
  DEVXLIB_LIBS="-L$devxlib_libdir -ldevXlib"
  LIBS="$DEVXLIB_LIBS $LIBCUDA_LIBS $LIBROCM_LIBS"
dnl $acx_devxlib_save_LIBS"
  AC_LINK_IFELSE($testprog, [acx_devxlib_ok=yes], [])
fi


dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_devxlib_ok" = xyes; then
  compile_devxlib=no
  internal_devxlib=no
  #
  AC_DEFINE(HAVE_DEVXLIB, 1, [Defined if you have the DEVXLIB library.])
fi

if test x"$acx_devxlib_ok" = xno; then
  internal_devxlib="yes"
  #DEVXLIB_LIBS="${extlibs_path}/${FCKIND}/${FC}/${GPU_SUPPORT}/lib/libdevXlib.a"
  DEVXLIB_LIBS="-L${extlibs_path}/${FCKIND}/${FC}/${GPU_SUPPORT}/lib -ldevXlib"
  DEVXLIB_INCS="$IFLAG${extlibs_path}/${FCKIND}/${FC}/${GPU_SUPPORT}/include"
  if test -e "${extlibs_path}/${FCKIND}/${FC}/${GPU_SUPPORT}/lib/libdevXlib.a"; then
    compile_devxlib="no"
    AC_MSG_RESULT([Compatible external DevXlib not found/specified. Found internal already compiled.])
  else
    if test x"$with_devxlib_branch" = "xnone"; then
      DEVXLIB_info="(devxlib tarball)"
    else
      DEVXLIB_info="(devxlib $with_devxlib_branch branch)"
    fi
    compile_devxlib="yes"
    AC_MSG_RESULT([Compatible external DevXlib not found/specified. Internal to be compiled.])
  fi
fi 
FCFLAGS="$acx_devxlib_save_FCFLAGS"
LIBS="$acx_devxlib_save_LIBS"

AC_SUBST(DEVXLIB_LIBS)
AC_SUBST(DEVXLIB_INCS)
AC_SUBST(compile_devxlib)
AC_SUBST(internal_devxlib)

AC_SUBST(DEVXLIB_info)
AC_SUBST(with_devxlib_branch)


])
