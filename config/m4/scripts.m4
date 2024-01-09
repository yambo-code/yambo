#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([ACX_SCRIPTS],[
#
AC_ARG_ENABLE(ydb, AS_HELP_STRING([--enable-ydb],[Activate the YDB support]))
#
enable_yambopy="no"
#
# AC_ARG_ENABLE(yambopy, AS_HELP_STRING([--enable-yambopy],[Activate the Yambo PY project]))
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
