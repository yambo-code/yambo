#
#        Copyright (C) 2000-2020 the YAMBO team
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
AC_DEFUN([ACX_SCRIPTS],[
#
AC_ARG_ENABLE(ydb, AC_HELP_STRING([--enable-ydb],[Activate the YDB support]))
#
enable_yambopy="no"
#
# AC_ARG_ENABLE(yambopy, AC_HELP_STRING([--enable-yambopy],[Activate the Yambo PY project]))
#
if test "x$enable_ydb" = "xyes"; then
 #
 AC_CHECK_TOOL(GIT, git, false)
 #
 if test  "$GIT" = "false"; then
  enable_ydb="no"
  AC_MSG_WARN([Git not found. Impossible to install YDB.])
 fi
 #
 if test "x$enable_ydb" = "xyes" && ! test -d "scripts/ydb"; then
  #
  AC_MSG_CHECKING([YDB from $url_ydb])
  mkdir -p scripts
  git clone $url_ydb scripts/ydb
  #
  if test -d "scripts/ydb"; then 
   cd $exec_prefix/bin
   ln -s ../scripts/ydb/ydb.pl .
   cd $srcdir
  else
   enable_ydb="no"
   AC_MSG_WARN([Impossible to install YDB.])
  fi
 fi
 #
 YDB_dir="$PWD/scripts/ydb"
 AC_SUBST(YDB_dir)
fi
])
