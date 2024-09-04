#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([AC_YDIAGO],[

#
AC_ARG_WITH(ydiago-branch,[AS_HELP_STRING([--with-ydiago-branch=<branch>],[Use the <branch> of the ydiago repository.],[32])],,[with_ydiago_branch=none])
#
AC_CONFIG_FILES([lib/ydiago/make_ydiago.inc:lib/ydiago/make_ydiago.inc.in])
if test x"$with_ydiago_branch" = "xnone"; then
 ydiago_check="D"; 
else
 ydiago_check="G"; 
fi
#YDIAGO_LIBS="${extlibs_path}/${FCKIND}/${FC}/lib/libydiago.a"
#YDIAGO_INCS="$IFLAG${extlibs_path}/${FCKIND}/${FC}/include/"
YDIAGO_LIBS="${compdir}/lib/libydiago.a"
YDIAGO_INCS="$IFLAG${compdir}/include/"

AC_SUBST(YDIAGO_LIBS)
AC_SUBST(YDIAGO_INCS)

AC_SUBST(ydiago_check)
AC_SUBST(with_ydiago_branch)

])
