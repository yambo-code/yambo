#
# Original version Available from the GNU Autoconf Macro Archive at:
# http://autoconf-archive.cryp.to/macros-by-category.html
#
# License-Identifier: GPL
#
# Copyright (C) 2006 The Yambo Team
#
# Authors (see AUTHORS file for details): AM DS
#
AC_DEFUN([ACX_MPI], [
AC_PREREQ([2.50]) dnl for AC_LANG_CASE
acx_mpi_ok=no

AC_LANG_CASE([C], [
        AC_REQUIRE([AC_PROG_CC])
        AC_ARG_VAR(MPICC,[Parallel C compiler command])
        if test x"$MPICC" = "x" ; then
          AC_CHECK_PROGS(MPICC_test,$MPICC mpipgicc mpiicc mpicc mpiicpx hcc mpcc mpcc_r mpxlc cmpicc, $CC)
        else
          AC_CHECK_FILES($MPICC,
                         [MPICC_test=$MPICC],
                         [AC_CHECK_PROGS(MPICC_test,$MPICC mpipgicc mpiicc mpicc mpiicpx hcc mpcc mpcc_r mpxlc cmpicc, $CC)])
        fi
        MPICC=$MPICC_test
        CC=$MPICC_test
],
[C++], [
        AC_REQUIRE([AC_PROG_CXX])
        AC_ARG_VAR(MPICXX,[Parallel C++ compiler command])
        AC_CHECK_PROGS(MPICXX_test,$MPICXX mpic++ mpiCC mpCC hcp mpxlC mpxlC_r cmpic++, $CXX)
        MPICXX=$MPICXX_test
        CXX=$MPICXX_test
],
[Fortran 77], [
        AC_REQUIRE([AC_PROG_F77])
        AC_ARG_VAR(MPIF77,[Parallel Fortran 77 compiler command])
        if ! test x"$MPIF77" = "x" ; then
          AC_CHECK_FILE($MPIF77,
                         [MPIF77_test=$MPIF77],
                         [AC_CHECK_PROGS(MPIF77_test,$MPIF77 $MPIFC mpipgifort mpiifort mpifort mpiifx mpif77 hf77 mpxlf mpf77 mpif90 mpf90 mpxlf90 mpxlf95 mpxlf_r cmpifc cmpif90c, $F77)])
        elif ! test x"$MPIFC" = "x" ; then
          AC_CHECK_FILE($MPIFC,
                         [MPIF77_test=$MPIFC],
                         [AC_CHECK_PROGS(MPIF77_test,$MPIF77 $MPIFC mpipgifort mpiifort mpifort mpiifx mpif77 hf77 mpxlf mpf77 mpif90 mpf90 mpxlf90 mpxlf95 mpxlf_r cmpifc cmpif90c, $F77)])
        else
          AC_CHECK_PROGS(MPIF77_test,$MPIF77 $MPIFC mpipgifort mpiifort mpifort mpiifx mpif77 hf77 mpxlf mpf77 mpif90 mpf90 mpxlf90 mpxlf95 mpxlf_r cmpifc cmpif90c, $F77)
        fi
        MPIF77=$MPIF77_test
        F77=$MPIF77_test
],
[Fortran], [
        AC_REQUIRE([AC_PROG_FC])
        AC_ARG_VAR(MPIFC,[Parallel Fortran compiler command])
        if test x"$MPIFC" = "x" ; then
          AC_CHECK_PROGS(MPIFC_test,$MPIFC mpipgifort mpiifort mpifort mpif90 mpiifx mpxlf90 mpxlf mpf90 mpxlf95 mpxlf_r, $FC)
        else
          AC_CHECK_FILE($MPIFC,
                        [MPIFC_test=$MPIFC],
                        [AC_CHECK_PROGS(MPIFC_test,$MPIFC mpipgifort mpiifort mpifort mpiifx mpif90 mpxlf90 mpxlf mpf90 mpxlf95 mpxlf_r, $FC)])
        fi
        MPIFC=$MPIFC_test
        FC=$MPIFC_test
])

if test "$acx_mpi_ok" = "yes"; then
  AC_LANG_CASE([C],   [AC_CHECK_FUNC(MPI_Init, [acx_mpi_ok="yes"])],
               [C++], [AC_CHECK_FUNC(MPI_Init, [acx_mpi_ok="yes"])],
               [Fortran 77], [AC_MSG_CHECKING([for MPI_Init])
                AC_TRY_LINK_FUNC(MPI_Init,[ac_mpi_ok="yes"
                AC_MSG_RESULT(yes)], [AC_MSG_RESULT(no)])],
               [Fortran], [AC_MSG_CHECKING([for MPI_Init])
                AC_TRY_LINK_FUNC(MPI_init,[acx_mpi_ok="yes"
                AC_MSG_RESULT(yes)], [AC_MSG_RESULT(no)])]
)
fi

# We have to use AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[]])],[],[]) and not AC_CHECK_HEADER because the
# latter uses $CPP, not $CC (which may be mpicc).
#
# In the following, we tentatively skip the check for mpipgicc,
# which would require the include of mpi.h out of the main()
#
if test x"$MPICC" != x"mpipgicc" ; then
echo > /dev/null
AC_LANG_CASE([C], [if test "$acx_mpi_ok" = "no"; then
        AC_MSG_CHECKING([for a working mpi.h])
        AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], 
        [#include <mpi.h>]),
        [AC_MSG_RESULT(yes);acx_mpi_ok="yes"],
        [AC_MSG_RESULT(no) ;acx_mpi_ok="no"])
fi],
[C++], [if test "$acx_mpi_ok" = "no"; then
        AC_MSG_CHECKING([for mpi.h])
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <mpi.h>]], [[]])],[AC_MSG_RESULT(yes);acx_mpi_ok="yes"],[AC_MSG_RESULT(no); acx_mpi_ok="no" ])
fi])
else
  acx_mpi_ok="yes"
fi

AC_LANG_CASE([Fortran 77],[acx_mpi_ok="yes"])

AC_LANG_CASE([Fortran],
 [AC_MSG_CHECKING([for a working mpif.h])
  save_ldflags="$LDFLAGS"
  AS_IF([test "$LIB_MPI"], [LDFLAGS="${LDFLAGS} -L${LIB_MPI}"])
  AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [
  include 'mpif.h'
  integer :: ierr
  call MPI_Init(ierr)]), 
  [HAVE_MPIF_H=1; acx_mpi_ok="yes"; AC_MSG_RESULT(yes)], 
  [HAVE_MPIF_H=0; acx_mpi_ok="no" ; AC_MSG_RESULT(no)])])


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
  [HAVE_MPI_H=0; acx_mpi_ok="no" ; AC_MSG_RESULT(no)]);fi])

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
  [HAVE_MPI_MOD=0; acx_mpi_ok="no";  AC_MSG_RESULT(no)]);fi])

#
#
mpibuild="no"
if test "$acx_mpi_ok" = "yes"; then  
 mpibuild="yes"; 
fi

#
# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test "$acx_mpi_ok" = "no"; then
        $2
        :
else
        ifelse([$1],,[AC_DEFINE(HAVE_MPI,1,[Define if you have the MPI library.])],[$1])
        :
fi

])dnl ACX_MPI
