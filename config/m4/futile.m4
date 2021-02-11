#
#        Copyright (C) 2000-2020 the YAMBO team
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
AC_DEFUN([AC_HAVE_FUTILE],[

AC_ARG_ENABLE(yaml_output, AC_HELP_STRING([--enable-yaml-output],
            [Activate the YAML output support. Default is no],[32]))

AC_ARG_WITH(yaml_libs, AC_HELP_STRING([--with-yaml-libs=<libs>],
            [Use the FUTILE library in <libs>],[32]),[],[])
AC_ARG_WITH(yaml_libdir, AC_HELP_STRING([--with-yaml-libdir=<path>],
            [Path to the FUTILE lib directory],[32]))
AC_ARG_WITH(yaml_libdir, AC_HELP_STRING([--with-yaml-libdir=<path>],
            [Path to the FUTILE lib directory],[32]))
AC_ARG_WITH(yaml_includedir, AC_HELP_STRING([--with-yaml-includedir=<path>],
            [Path to the FUTILE include directory],[32]),[],[])



AC_ARG_WITH(futile_libs, AC_HELP_STRING([--with-futile-libs=<libs>],
            [Use the FUTILE library in <libs>],[32]),[],[])
AC_ARG_WITH(futile_libdir, AC_HELP_STRING([--with-futile-libdir=<path>],
            [Path to the FUTILE lib directory],[32]))
AC_ARG_WITH(futile_libdir, AC_HELP_STRING([--with-futile-libdir=<path>],
            [Path to the FUTILE lib directory],[32]))
AC_ARG_WITH(futile_includedir, AC_HELP_STRING([--with-futile-includedir=<path>],
            [Path to the FUTILE include directory],[32]),[],[])

def_yaml=""
enable_yaml="no"
compile_yaml="no"
internal_yaml="no"
YAML_LIBS=" "
YAML_INCS=" "

if test x"$enable_yaml_output" = "xyes"; then enable_yaml=yes ; fi
if test -d "$with_yaml_path"  ;  then enable_yaml=yes ; fi
if test -d "$with_yaml_libdir" ; then enable_yaml=yes ; fi
if test  x"$with_yaml_libs" != "x" ;  then enable_yaml=yes ; fi

if test "x$enable_yaml" = "xyes" ; then
  #
  if test -d "$with_yaml_path" || test -d "$with_yaml_libdir" ; then
    #
    # external YAML
    #
    if test -d "$with_yaml_path" ;   then AC_MSG_CHECKING([for YAML in $with_yaml_path]) ; fi
    if test -d "$with_yaml_libdir" ; then AC_MSG_CHECKING([for YAML in $with_yaml_libdir]) ; fi
    #
    if test -d "$with_yaml_path" ; then
        try_yaml_libdir_src=$with_yaml_path/src
        try_yaml_incdir_src=$with_yaml_path/src
        try_yaml_libdir=$with_yaml_path/lib
        try_yaml_incdir=$with_yaml_path/include
    fi
    if test -d "$with_yaml_libdir"  ;    then try_yaml_libdir=$with_yaml_libdir ; fi
    if test -d "$with_yaml_includedir" ; then try_yaml_incdir=$with_yaml_includedir ; fi
    #
    if test -z "$try_yaml_libdir" ; then AC_MSG_ERROR([No lib-dir specified]) ; fi
    if test -z "$try_yaml_incdir" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
    #
    # 
    if test -r $try_yaml_libdir_src/libyaml.a && test -e $try_yaml_incdir_src/yaml_module.mod ; then
      compile_yaml="no"
      YAML_INCS="$IFLAG$try_yaml_incdir_src"
      YAML_LIBS="$try_yaml_libdir_src/libyaml.a"
      AC_MSG_RESULT([yes])
    elif test -r $try_yaml_libdir/libyaml.a  && test -e $try_yaml_incdir/yaml_module.mod ; then
      compile_yaml="no"
      YAML_INCS="$IFLAG$try_yaml_incdir"
      YAML_LIBS="$try_yaml_libdir/libyaml.a"
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no. Fallback to internal library.])
    fi
  elif test x"$with_yaml_libs" != "x" ; then
    #
    # directly provided lib
    #
    AC_MSG_CHECKING([for YAML Library using $with_yaml_libs])
    compile_yaml="no"
    if test -d "$with_yaml_includedir" ; then YAML_INCS="$IFLAG$with_yaml_includedir" ; fi
    YAML_LIBS="$with_yaml_libs"
    AC_MSG_RESULT(yes)
  fi
  if test "$YAML_LIBS" = " "; then
    #
    # internal YAML
    #
    AC_MSG_CHECKING([for internal YAML library])
    internal_yaml="yes"
    YAML_INCS="${IFLAG}${extlibs_path}/${FCKIND}/${FC}/include/"
    YAML_LIBS="${extlibs_path}/${FCKIND}/${FC}/lib/libyaml.a"
    if ! test -e "${extlibs_path}/${FCKIND}/${FC}/lib/libyaml.a" || ! test -e "${extlibs_path}/${FCKIND}/${FC}/include/yaml_parse.mod" || ! test -e "${extlibs_path}/${FCKIND}/${FC}/include/yaml.h"; then
      compile_yaml="yes"
      if test ! -d lib ; then mkdir lib ; fi
      AC_MSG_RESULT(to be compiled)
    else
      compile_yaml="no"
      AC_MSG_RESULT(already compiled)
    fi
  fi
else
  AC_MSG_CHECKING([for YAML library])
  AC_MSG_RESULT([no])
fi



enable_futile="no"
compile_futile="no"
internal_futile="no"
FUTILE_LIBS=" "
FUTILE_INCS=" "

if test x"$enable_yaml" = "xyes"; then enable_futile=yes ; fi

if test -d "$with_futile_path"  ;  then enable_futile=yes ; fi
if test -d "$with_futile_libdir" ; then enable_futile=yes ; fi
if test  x"$with_futile_libs" != "x" ;  then enable_futile=yes ; fi


if test "x$enable_futile" = "xyes" ; then
  #
  if test -d "$with_futile_path" || test -d "$with_futile_libdir" ; then
    #
    # external FUTILE
    #
    if test -d "$with_futile_path" ;   then AC_MSG_CHECKING([for FUTILE in $with_futile_path]) ; fi
    if test -d "$with_futile_libdir" ; then AC_MSG_CHECKING([for FUTILE in $with_futile_libdir]) ; fi
    #
    if test -d "$with_futile_path" ; then
        try_futile_libdir_src=$with_futile_path/src
        try_futile_incdir_src=$with_futile_path/src
        try_futile_libdir=$with_futile_path/lib
        try_futile_incdir=$with_futile_path/include
    fi
    if test -d "$with_futile_libdir"  ;    then try_futile_libdir=$with_futile_libdir ; fi
    if test -d "$with_futile_includedir" ; then try_futile_incdir=$with_futile_includedir ; fi
    #
    if test -z "$try_futile_libdir" ; then AC_MSG_ERROR([No lib-dir specified]) ; fi
    if test -z "$try_futile_incdir" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
    #
    # 
    if test -r $try_futile_libdir_src/libfutile-1.a && test -e $try_futile_incdir_src/futile_module.mod ; then
      compile_futile="no"
      FUTILE_INCS="$IFLAG$try_futile_incdir_src"
      FUTILE_LIBS="$try_futile_libdir_src/libfutile-1.a"
      AC_MSG_RESULT([yes])
    elif test -r $try_futile_libdir/libfutile-1.a  && test -e $try_futile_incdir/futile_module.mod ; then
      compile_futile="no"
      FUTILE_INCS="$IFLAG$try_futile_incdir"
      FUTILE_LIBS="$try_futile_libdir/libfutile-1.a"
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no. Fallback to internal library.])
    fi
  elif test x"$with_futile_libs" != "x" ; then
    #
    # directly provided lib
    #
    AC_MSG_CHECKING([for FUTILE Library using $with_futile_libs])
    compile_futile="no"
    if test -d "$with_futile_includedir" ; then FUTILE_INCS="$IFLAG$with_futile_includedir" ; fi
    FUTILE_LIBS="$with_futile_libs"
    AC_MSG_RESULT(yes)
  fi
  if test "$FUTILE_LIBS" = " "; then
    #
    # internal FUTILE
    #
    AC_MSG_CHECKING([for internal FUTILE library])
    internal_futile="yes"
    FUTILE_INCS="${IFLAG}${extlibs_path}/${FCKIND}/${FC}/include/"
    FUTILE_LIBS="${extlibs_path}/${FCKIND}/${FC}/lib/libfutile-1.a"
    if ! test -e "${extlibs_path}/${FCKIND}/${FC}/lib/libfutile-1.a" || ! test -e "${extlibs_path}/${FCKIND}/${FC}/include/futile.mod" || ! test -e "${extlibs_path}/${FCKIND}/${FC}/include/futile.h"; then
      compile_futile="yes"
      if test ! -d lib ; then mkdir lib ; fi
      AC_MSG_RESULT(to be compiled)
    else
      compile_futile="no"
      AC_MSG_RESULT(already compiled)
    fi
  fi
else
  AC_MSG_CHECKING([for FUTILE library])
  AC_MSG_RESULT([no])
fi
#
if test $enable_yaml = "yes" && test $enable_futile = "yes" ; then
  def_yaml="-D_YAML_OUTPUT"
fi
#
AC_SUBST(def_yaml)
AC_SUBST(enable_yaml)
AC_SUBST(compile_yaml)
AC_SUBST(internal_yaml)
AC_SUBST(YAML_INCS)
AC_SUBST(YAML_LIBS)
#
AC_SUBST(enable_futile)
AC_SUBST(compile_futile)
AC_SUBST(internal_futile)
AC_SUBST(FUTILE_INCS)
AC_SUBST(FUTILE_LIBS)
#
])
