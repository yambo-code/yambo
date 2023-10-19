#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([AC_YDRIVER],[

DRIVER_INCS="-I$PWD/lib/yambo/Ydriver/include/ -I$PWD/include/driver"
#
AC_ARG_WITH(ydriver-branch,[AS_HELP_STRING([--with-ydriver-branch=<branch>],[Use the <branch> of the ydriver repository.],[32])],,[with_ydriver_branch=none])
#
if test x"$with_ydriver_branch" = "xnone"; then
 Ydriver_check="D"; 
 Ydriver_LIB="Download"
else
 Ydriver_check="G"; 
 Ydriver_LIB="GIT, branch $with_ydriver_branch"
fi
AC_SUBST(Ydriver_LIB)
AC_SUBST(Ydriver_check)
AC_SUBST(DRIVER_INCS)
AC_SUBST(with_ydriver_branch)
])
