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
CC_serial=$CC
#CXX_serial=$CXX
F77_serial=$F77
FC_serial=$FC
#
if test "$mpibuild" = "yes"; then
  #
  # MPIFC
  #
  AC_LANG_PUSH(Fortran)
  ACX_MPI([],AC_MSG_WARN([could not compile a FORTRAN mpi test program. YAMBO serial only.]))
  AC_LANG_POP(Fortran)
  #
fi
#
if test "$mpibuild" = "yes"; then
  #
  # MPIF77
  #
  AC_LANG_PUSH(Fortran 77)
  ACX_MPI([],AC_MSG_WARN([could not compile a FORTRAN 77 mpi test program. YAMBO serial only.]))
  AC_LANG_POP(Fortran 77)
  #
fi
#
if test "$mpibuild" = "yes"; then
  #
  # MPICC
  #
  AC_LANG_PUSH(C)
  ACX_MPI([],AC_MSG_WARN([could not compile a C mpi test program. YAMBO serial only.]))
  AC_LANG_POP(C)
  #
fi
#
if test "$mpibuild" = "yes"; then
  #
  def_mpi="-D_MPI"
  #
else
  #
  def_mpi=""
  #
  CC=$CC_serial
  #CXX=$CXX_serial
  F77=$F77_serial
  FC=$FC_serial
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
  SHADOW=`$CC --showme:incdirs 2> config_openmpi.err` ;
  SHADOW=`$CC -c -show         2> config_others.err ` ;
  #
  CHECK_openmpi=`cat config_openmpi.err`;
  CHECK_others=`cat config_others.err`;
  #
  `rm config_openmpi.err config_others.err` ;
  #
  if test x"$CHECK_openmpi" = "x" ; then  MPI_INC_DIRS_LIST=`$CC --showme:incdirs` ;
  elif test x"$CHECK_others" = "x"; then  MPI_INC_DIRS_LIST=`$CC -c -show| sed "s/.*${IFLAG}//g"` ;
  fi
  #
  # This should not be needed
  #PATH_FROM_COMPILER=`which $FC|xargs dirname|sed "s/bin/include"` ;
  #MPI_INC_DIRS_LIST="${MPI_INC_DIRS_LIST} ${PATH_FROM_COMPILER}";
  #
  if test x"$CHECK_others" = "x" ; then MPI_LIBS=`$CC -show | sed "s/.*-L/-L/g"` ; fi
  #
  for MPI_INC_DIR in $MPI_INC_DIRS_LIST; do
    if ! test -e "$MPI_INC_DIR"; then continue; fi
    MPI_INCS="${MPI_INCS}${IFLAG}${MPI_INC_DIR} "
    AC_CHECK_FILE($MPI_INC_DIR/mpif.h,[mpif_found_tmp="yes"],[mpif_found_tmp="no"])
    if test "$mpif_found_tmp" = "yes" ; then
      mpif_found="$mpif_found_tmp" ;
      MPI_PATH=`echo ${MPI_INC_DIR} | sed "s/\/include//g"` ;
    fi
  done
  MPI_INC_DIRS_LIST="" ;
  MPI_INC_DIR="" ;
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
  # Missign proper test for MPI_LIBS,
  # it maybe not needed
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
  mpif_found_tmp="$mpif_found"
  if test x"$MPI_INC_DIR" = "x"  ; then
    for MPI_INC_DIR in $MPI_INC_DIRS_LIST; do
      if ! test -e "$MPI_INC_DIR"; then continue; fi
      AC_CHECK_FILE($MPI_INC_DIR/mpif.h,[mpif_found_tmp="yes"],[mpif_found_tmp="no"])
      if test "$mpif_found_tmp" = "yes" ; then
        MPI_PATH=`echo $MPI_INC_DIR | sed "s/\/include//g"` ;
        break;
      fi
    done
  fi
  #
  if test "$mpif_found" = "yes" && test "$mpif_found_tmp" = "no"; then
    AC_MSG_CHECKING([mpif.h was found in folder pointed to by $CC]);
    AC_MSG_RESULT(but not found in folder specified in --with-mpi-XXX);
    mpif_found="$mpif_found_tmp";
  fi
  #
  if test "$mpif_found" = "no" ; then
    MPI_PATH="" ;
    MPI_LIBS="" ;
    MPI_INCS="" ;
  fi
  #
fi
#
AC_SUBST(mpif_found)
#
AC_SUBST(MPI_INCS)
AC_SUBST(MPI_LIBS)
#
AC_SUBST(MPI_PATH)
