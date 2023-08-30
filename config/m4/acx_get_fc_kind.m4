#
# License-Identifier: GPL
#
# Copyright (C) 2007 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([ACX_GET_FC_KIND],
[
INTELVERSION="unknown"
FCKIND="unknown"
case "${FC}" in
    *ftn*)
      FCVERSION=`$FC --version`
      ;;
    *abf90*)
      FCVERSION=`$FC --version`
      ;;
    *pgf*)
      FCKIND="pgi"
      FCVERSION=`$FC --version`
      ;;
    *nvfortran*)
      FCKIND="nvfortran"
      FCVERSION=`$FC --version`
      ;;
    *gfortran*)
      FCKIND="gfortran" 
      FCVERSION=`$FC --version`
      ;;
    *g95*)
      FCKIND="g95"
      ;;
    *ifc*)
      FCKIND="intel"
      FCVERSION=`$FC -v 2>&1`
      ;;
    *ifort*)
      FCKIND="intel"
      FCVERSION=`$FC -v 2>&1`
      $FC -v >& ver_
      VER_8=`grep 8. ver_   | wc -l`
      VER_9=`grep 9. ver_   | wc -l`
      VER_10=`grep 10. ver_ | wc -l`
      VER_11=`grep 11. ver_ | wc -l`
      VER_12=`grep 12. ver_ | wc -l`
      VER_13=`grep 13. ver_ | wc -l`
      VER_14=`grep 14. ver_ | wc -l`
      VER_15=`grep 15. ver_ | wc -l`
      VER_16=`grep 16. ver_ | wc -l`
      VER_17=`grep 17. ver_ | wc -l`
      VER_18=`grep 18. ver_ | wc -l`
      VER_19=`grep 19. ver_ | wc -l`
      VER_2021=`grep 2021. ver_ | wc -l`
      if ! test "$VER_8" = "0";  then INTELVERSION="8"  ; fi
      if ! test "$VER_9" = "0";  then INTELVERSION="9"  ; fi
      if ! test "$VER_10" = "0"; then INTELVERSION="10" ; fi
      if ! test "$VER_11" = "0"; then INTELVERSION="11" ; fi
      if ! test "$VER_12" = "0"; then INTELVERSION="12" ; fi
      if ! test "$VER_13" = "0"; then INTELVERSION="13" ; fi
      if ! test "$VER_14" = "0"; then INTELVERSION="14" ; fi
      if ! test "$VER_15" = "0"; then INTELVERSION="15" ; fi
      if ! test "$VER_16" = "0"; then INTELVERSION="16" ; fi
      if ! test "$VER_17" = "0"; then INTELVERSION="17" ; fi
      if ! test "$VER_18" = "0"; then INTELVERSION="18" ; fi
      if ! test "$VER_19" = "0"; then INTELVERSION="19" ; fi
      if ! test "$VER_2021" = "0"; then INTELVERSION="2021" ; fi
      rm -f ver_
      ;;
    *)
esac
#
FCVERSION=`echo "$FCVERSION" | sed "/^\s*$/d" | head -n 1`
#
AC_MSG_CHECKING([for $FC kind and version])
AC_MSG_RESULT([$FCKIND $FCVERSION $INTELVERSION])

AC_SUBST(FCKIND)
AC_SUBST(FCVERSION)
AC_SUBST(INTELVERSION)

])
