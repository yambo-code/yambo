#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([ACX_BRANCH],
[
Y_BRANCH="unknown"
AC_CHECK_PROG(GIT_CHECK,git,yes)
if test x"$GIT_CHECK" = x"yes" && test -f $srcdir/.gitignore; then
 cd $srcdir
 Y_BRANCH=`git branch | grep \* | cut -d ' ' -f2`
 cd $compdir
fi
AC_SUBST(Y_BRANCH)
])
