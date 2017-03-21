#
# Original version Available from the GNU Autoconf Macro Archive at:
# http://autoconf-archive.cryp.to/macros-by-category.html
#
#        Copyright (C) 2000-2017 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM, DS
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

mpibuild="yes" 
AC_ARG_ENABLE(mpi, AC_HELP_STRING([--enable-mpi], [Enable mpi parallelization . Default is yes.]))
if test "$enable_mpi" = "no"; then mpibuild="no"; fi
#
if test "$mpibuild" = "yes"; then
  #
  # MPIFC
  #
  AC_LANG_PUSH(Fortran)
  ACX_MPI([],AC_MSG_WARN([could not compile a FORTRAN mpi test program. YAMBO serial only.]))
  AC_LANG_POP(Fortran)
  #
  # MPIF77
  #
  AC_LANG_PUSH(Fortran 77)
  ACX_MPI([],AC_MSG_WARN([could not compile a FORTRAN 77 mpi test program. YAMBO serial only.]))
  AC_LANG_POP(Fortran 77)
  #
  # MPICC
  #
  AC_LANG_PUSH(C)
  ACX_MPI([],AC_MSG_WARN([could not compile a C mpi test program. YAMBO serial only.]))
  AC_LANG_POP(C)
  #
  def_mpi="-D_MPI"
  #
else
  #
  def_mpi=""
  #
fi
#
AC_SUBST(def_mpi)
AC_SUBST(mpibuild)

AC_ARG_WITH(mpi_libs,AC_HELP_STRING([--with-mpi-libs=<libs>],[Use MPI libraries <libs>],[32]))
AC_ARG_WITH(mpi_path, AC_HELP_STRING([--with-mpi-path=<path>],[Path to the MPI install directory],[32]),[],[])
AC_ARG_WITH(mpi_libdir,AC_HELP_STRING([--with-mpi-libdir=<path>],[Path to the MPI lib directory],[32]))
AC_ARG_WITH(mpi_includedir,AC_HELP_STRING([--with-mpi-includedir=<path>],[Path to the MPI include directory],[32]))

mpif_found="no"

MPI_INCS=""
MPI_LIBS=""
MPI_PATH=""

if test "$mpibuild" = "yes"; then
  #
  if test "$FCKIND" = "gfortran"; then
    #MPI_LIBS="-L`$CC --showme:libdirs` -l`$CC --showme:libs`"
    MPI_INC_DIRS_LIST=`$CC --showme:incdirs`
  elif test "$FCKIND" = "intel"; then
    MPI_INC_DIRS_LIST=`$CC -c -show| sed "s/-I//g"`
  fi
  #
  # Previous version by AndreaM
  #PATH_COMPILER=`which $FC|xargs dirname`
  #MPI_INC_DIRS_LIST="${PATH_COMPILER}/../include /usr/include /usr/lib/openmpi/include /usr/lib/mpich/include";
  #
  if test -d "$with_mpi_path"; then
    MPI_PATH="$with_mpi_path";
    MPI_LIB_DIR="$MPI_PATH/lib";
    MPI_INC_DIR="$MPI_PATH/include";
  fi
  if test -d "$with_mpi_libdir"; then
    MPI_LIB_DIR="$with_mpi_libdir";
  fi
  #
  if test -d "$with_mpi_includedir" ; then
    MPI_PATH="$with_mpi_includedir/../";
    MPI_INC_DIR="$with_mpi_includedir";
  fi
  #
  if test "$mpi_libs" != ""  ; then MPI_LIBS="$mpi_libs"; fi
  if test "$MPI_LIB_DIR" != ""  ; then MPI_LIBS="-L$MPI_LIB_DIR $MPI_LIBS"; fi
  #
  #mpi_libs_ok="no"
  #if test  "$MPI_LIBS" != "" ;  then
  #  AC_MSG_CHECKING([for MPI_Init in $MPI_LIBS]);
  #  AC_LINK_IFELSE($mpi_routine, [mpi_libs_ok=yes]);
  #  AC_MSG_RESULT($mpi_libs_ok);
  #  if test "$mpi_libs_ok" = "no" ; then 
  #    MPI_LIBS="";
  #    MPI_LIB_DIR="";
  #  fi
  #fi
  #
  # Check include
  #
  if test "$MPI_INC_DIR" != ""; then
    MPI_INC_DIRS_LIST="$MPI_INC_DIR $MPI_INC_DIRS_LIST";
    MPI_INC_DIR="";
  fi
  #
  for MPI_INC_DIR in $MPI_INC_DIRS_LIST; do
    if ! test -e "$MPI_INC_DIR"; then continue; fi
    AC_CHECK_FILE($MPI_INC_DIR/mpif.h,[mpif_found="yes"],[mpif_found="no"])
    if test "$mpif_found" = "yes" ; then
      IFLAG=$ax_cv_f90_modflag
      if test -z "$IFLAG" ; then IFLAG="-I" ; fi
      MPI_INCS="$IFLAG$MPI_INC_DIR"
      break
    else
      MPI_PATH="";
      MPI_INC_DIR="";
    fi
  done
  #
fi
#
AC_SUBST(mpif_found)
#
AC_SUBST(MPI_INCS)
AC_SUBST(MPI_LIBS)
#
AC_SUBST(MPI_PATH)
