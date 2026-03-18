#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([AC_YDIAGO],[

#
AC_ARG_ENABLE(ydiago,   AS_HELP_STRING([--enable-ydiago],[Use ydiago. Default is yes]),[],[enable_ydiago="yes"])
AC_ARG_WITH(ydiago-branch,[AS_HELP_STRING([--with-ydiago-branch=<branch>],[Use the <branch> of the ydiago repository.],[32])],,[with_ydiago_branch=none])
#
AC_CONFIG_FILES([lib/ydiago/make_ydiago.inc:lib/ydiago/make_ydiago.inc.in])

compile_ydiago="no"
def_ydiago=""

if test "$def_scalapack" = "-D_SCALAPACK" && test "x$def_chase" = "x" &&
   test "$enable_ydiago" = "yes" ; then

  compile_ydiago="yes"
  def_ydiago="-D_YDIAGO"
  
  # GPU flags are passed to ydiago compilation only if elpa library is available
  if test ! x"$def_gpu" = "x" && test "$def_elpa" = "-D_ELPA" ; then
    ydiago_opt="$def_gpu $def_elpa"
    YDIAGO_GPU_SUPPORT="$GPU_SUPPORT"
  else
    ydiago_opt="$def_elpa"
    YDIAGO_GPU_SUPPORT="no_gpu"
  fi

  #YDIAGO_LIBS="${extlibs_path}/${FCKIND}/${FC}/${YDIAGO_GPU_SUPPORT}/lib/libydiago.a"
  #YDIAGO_INCS="$IFLAG${extlibs_path}/${FCKIND}/${FC}/${YDIAGO_GPU_SUPPORT}/include/"
  YDIAGO_LIBS="${extlibs_path}/${FCKIND}/${FC}/lib/libydiago.a"
  YDIAGO_INCS="$IFLAG${extlibs_path}/${FCKIND}/${FC}/include/"
  #YDIAGO_LIBS="${compdir}/lib/libydiago.a"
  #YDIAGO_INCS="$IFLAG${compdir}/include/"

else

  compile_ydiago="no"
  def_ydiago=""
  ydiago_opt=""
  YDIAGO_LIBS=""
  YDIAGO_INCS=""

fi

AC_SUBST(YDIAGO_LIBS)
AC_SUBST(YDIAGO_INCS)
AC_SUBST(YDIAGO_GPU_SUPPORT)

AC_SUBST(compile_ydiago)
AC_SUBST(def_ydiago)
AC_SUBST(ydiago_opt)
AC_SUBST(with_ydiago_branch)

])
