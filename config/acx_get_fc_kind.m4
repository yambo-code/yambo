#
#        Copyright (C) 2000-2017 the YAMBO team
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
AC_DEFUN([ACX_GET_FC_KIND],
[
FCVERSION="unknown"
FCKIND="unknown"
case "${FC}" in
    *ftn*)
      ;;
    *pgf9*)
      FCKIND="pgi"
      ;;
    *abf90*)
      ;;
    *ifc*)
      FCKIND="intel"
      ;;
    *gfortran*)
      FCKIND="gfortran" 
      ;;
    *g95*)
      FCKIND="g95"
      ;;
    *ifort*)
      FCKIND="intel"
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
      if ! test "$VER_8" = "0";  then FCVERSION="8"  ; fi
      if ! test "$VER_9" = "0";  then FCVERSION="9"  ; fi
      if ! test "$VER_10" = "0"; then FCVERSION="10" ; fi
      if ! test "$VER_11" = "0"; then FCVERSION="11" ; fi
      if ! test "$VER_12" = "0"; then FCVERSION="12" ; fi
      if ! test "$VER_13" = "0"; then FCVERSION="13" ; fi
      if ! test "$VER_14" = "0"; then FCVERSION="14" ; fi
      if ! test "$VER_15" = "0"; then FCVERSION="15" ; fi
      if ! test "$VER_16" = "0"; then FCVERSION="16" ; fi
      if ! test "$VER_17" = "0"; then FCVERSION="17" ; fi
      if ! test "$VER_18" = "0"; then FCVERSION="18" ; fi
      if ! test "$VER_19" = "0"; then FCVERSION="19" ; fi
      rm -f ver_
      ;;
    *)
esac
AC_MSG_CHECKING([for $FC kind and version])
AC_MSG_RESULT([$FCKIND $FCVERSION])

AC_SUBST(FCKIND)
AC_SUBST(FCVERSION)

])
