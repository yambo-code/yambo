#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([AC_LDIAGO],[

#
AC_ARG_WITH(ldiago-branch,[AS_HELP_STRING([--with-ldiago-branch=<branch>],[Use the <branch> of the ldiago repository.],[32])],,[with_ldiago_branch=none])
#
AC_CONFIG_FILES([lib/ldiago/make_ldiago.inc:lib/ldiago/make_ldiago.inc.in])

# GPU flags are passed to ldiago compilation only if elpa library is available
if test ! x"$def_gpu" = "x" && test "$def_elpa" = "-D_ELPA" ; then
  ldiago_opt="$def_gpu $def_elpa"
  LDIAGO_GPU_SUPPORT="$GPU_SUPPORT"
else
  ldiago_opt="$def_elpa"
  LDIAGO_GPU_SUPPORT="no_gpu"
fi

LDIAGO_LIBS="${extlibs_path}/${FCKIND}/${FC}/diago/${LDIAGO_GPU_SUPPORT}/lib/libldiago.a"
LDIAGO_INCS="$IFLAG${extlibs_path}/${FCKIND}/${FC}/diago/${LDIAGO_GPU_SUPPORT}/include/"
#LDIAGO_LIBS="${compdir}/lib/libldiago.a"
#LDIAGO_INCS="$IFLAG${compdir}/include/"

AC_SUBST(LDIAGO_LIBS)
AC_SUBST(LDIAGO_INCS)
AC_SUBST(LDIAGO_GPU_SUPPORT)

AC_SUBST(ldiago_opt)
AC_SUBST(with_ldiago_branch)

])
