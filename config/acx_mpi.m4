#
# Original version Available from the GNU Autoconf Macro Archive at:
# http://autoconf-archive.cryp.to/macros-by-category.html
#
# Copyright (C) 2000-2009 A. Marini and the YAMBO team
#              http://www.yambo-code.org
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
AC_DEFUN([ACX_MPI], [
AC_PREREQ(2.50) dnl for AC_LANG_CASE
acx_mpi_ok=no

AC_LANG_CASE([C], [
        AC_REQUIRE([AC_PROG_CC])
        AC_ARG_VAR(MPICC,[Parallel C compiler command])
        AC_CHECK_PROGS(MPICC, mpicc hcc mpcc mpcc_r mpxlc cmpicc, $CC)
        acx_mpi_save_CC="$CC"
        CC="$MPICC"
],
[C++], [
        AC_REQUIRE([AC_PROG_CXX])
        AC_ARG_VAR(MPICXX,[MPI C++ compiler command])
        AC_CHECK_PROGS(MPICXX, mpic++ mpiCC mpCC hcp mpxlC mpxlC_r cmpic++, $CXX)
        acx_mpi_save_CXX="$CXX"
        CXX="$MPICXX"
        AC_SUBST(MPICXX)
],
[Fortran 77], [
        AC_REQUIRE([AC_PROG_F77])
        AC_ARG_VAR(MPIF77,[MPI Fortran compiler command])
        AC_CHECK_PROGS(MPIF77, mpif77 hf77 mpxlf mpf77 mpif90 mpf90 mpxlf90 mpxlf95 mpxlf_r cmpifc cmpif90c, $F77)
        acx_mpi_save_F77="$F77"
        F77="$MPIF77"
        AC_SUBST(MPIF77)
],
[Fortran], [
        AC_CHECK_PROGS(PFC, mpif90 mpxlf90 mpxlf mpf90 mpxlf95 mpxlf_r, $FC)
        FC="$PFC"
        acx_mpi_save_FC="$PFC"
])

if test x = x"$MPILIBS"; then
  AC_LANG_CASE([C],   [AC_CHECK_FUNC(MPI_Init, [MPILIBS=" "])],
               [C++], [AC_CHECK_FUNC(MPI_Init, [MPILIBS=" "])],
               [Fortran 77], [AC_MSG_CHECKING([for MPI_Init])
                AC_TRY_LINK([],[      call MPI_Init], [MPILIBS=" "
                AC_MSG_RESULT(yes)], [AC_MSG_RESULT(no)])],
               [Fortran], [AC_MSG_CHECKING([for MPI_Init])
                AC_TRY_LINK_FUNC(MPI_init,[MPILIBS=" "
                AC_MSG_RESULT(yes)], [AC_MSG_RESULT(no)])]
)
fi
if test x = x"$MPILIBS"; then
        AC_CHECK_LIB(mpi, MPI_Init, [MPILIBS="-lmpi"])
fi
if test x = x"$MPILIBS"; then
        AC_CHECK_LIB(mpich, MPI_Init, [MPILIBS="-lmpich"])
fi

# We have to use AC_TRY_COMPILE and not AC_CHECK_HEADER because the
# latter uses $CPP, not $CC (which may be mpicc).
AC_LANG_CASE([C], [if test x != x"$MPILIBS"; then
        AC_MSG_CHECKING([for a working mpi.h])
        AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], 
        [#include <mpi.h>]),
        [AC_MSG_RESULT(yes);acx_mpi_ok="yes"],
        [MPILIBS="";AC_MSG_RESULT(no)])
fi],
[C++], [if test x != x"$MPILIBS"; then
        AC_MSG_CHECKING([for mpi.h])
        AC_TRY_COMPILE([#include <mpi.h>],[],[AC_MSG_RESULT(yes)], [MPILIBS=""
                AC_MSG_RESULT(no)])
fi])

AC_LANG_CASE([Fortran],
 [AC_MSG_CHECKING([for a working mpif.h])
  save_ldflags="$LDFLAGS"
  AS_IF([test "$LIB_MPI"], [LDFLAGS="${LDFLAGS} -L${LIB_MPI}"])
  AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [
  include 'mpif.h'
  integer :: ierr
  call MPI_Init(ierr)]), 
  [HAVE_MPIF_H=1; acx_mpi_ok="yes"; AC_MSG_RESULT(yes)], 
  [HAVE_MPIF_H=0; AC_MSG_RESULT(no)])])

AC_LANG_CASE([Fortran],
 [if test "$acx_mpi_ok" = "no"; then
  AC_MSG_CHECKING([for a working mpi.h])
  save_ldflags="$LDFLAGS"
  AS_IF([test "$LIB_MPI"], [LDFLAGS="${LDFLAGS} -L${LIB_MPI}"])
  AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [
  include 'mpi.h'
  integer :: ierr
  call MPI_Init(ierr)]), 
  [HAVE_MPI_H=1; acx_mpi_ok="yes"; AC_MSG_RESULT(yes)], 
  [HAVE_MPI_H=0; AC_MSG_RESULT(no)]);fi])

AC_LANG_CASE([Fortran],
 [if test "$acx_mpi_ok" = "no"; then 
  AC_MSG_CHECKING([for a working mpi module])
  save_ldflags="$LDFLAGS"
  AS_IF([test "$LIB_MPI"], [LDFLAGS="${LDFLAGS} -L${LIB_MPI}"])
  AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [
  use mpi 
  integer :: ierr
  call MPI_Init(ierr)]), 
  [HAVE_MPI_MOD=1; acx_mpi_ok="yes"; AC_MSG_RESULT(yes)], 
  [HAVE_MPI_MOD=0; AC_MSG_RESULT(no)]);fi])
#
if test x = x"$MPILIBS"; then acx_mpi_ok="no"; fi

mpibuild="no"
AC_LANG_CASE(
 [Fortran],
 [if test "$acx_mpi_ok" = "yes"; then
  mpibuild="yes"
  PF90=$PFC
  PF90FLAGS=$FCFLAGS
  fi],
 [C],
 [if test "$acx_mpi_ok" = "yes"; then
  mpibuild="yes"
  PCC=$MPICC
  PCCFLAGS=$CFLAGS
  fi
#
# While the Fortran checks use FC (we use F90) the
# variable for C is CC, the same used here. 
# So we need to rename it.
#
  CC=$acx_mpi_save_CC
 ],
)

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x = x"$MPILIBS"; then
        $2
        :
else
        ifelse([$1],,[AC_DEFINE(HAVE_MPI,1,[Define if you have the MPI library.])],[$1])
        :
fi
])dnl ACX_MPI
