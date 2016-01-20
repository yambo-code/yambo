!
! Copyright (C) 2002-2004 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
#include "fftqe_defs.h"
!
!------------------------------------------------------------------------------!
module kinds
  !------------------------------------------------------------------------------!
  !
  implicit none
  save
  ! kind definitions
  integer, parameter :: DP = selected_real_kind(14,200)
  integer, parameter :: SP = selected_real_kind(6,30)
  integer, parameter :: sgl = selected_real_kind(6,30)
  integer, parameter :: i4b = selected_int_kind(9)
  private
  public :: i4b, sgl, SP, DP
  !
end module kinds

!------------------------------------------------------------------------------!
module io_global
  !------------------------------------------------------------------------------!
  !
  implicit none
  save
  ! 
  integer, parameter :: stdout = 6
  integer, parameter :: ionode_id = 0
  logical, parameter :: ionode = .FALSE.
  !
end module io_global
!
!
! Copyright (C) 2003-2004 Carlo Cavazzoni
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!------------------------------------------------------------------------------!
!   SISSA Code Interface -- Carlo Cavazzoni
!------------------------------------------------------------------------------C
MODULE parallel_include

#if defined __MPI 
!
!     Include file for MPI
!
         INCLUDE 'mpif.h'
         !
         ! this is only for symmetry with respect to the serial build
         LOGICAL :: tparallel = .true.
#else
         ! an empty module can break compilation when it is used by other modules
         LOGICAL :: tparallel = .false.
#endif

END MODULE parallel_include

!------------------------------------------------------------------------------!
module mp
  !------------------------------------------------------------------------------!
  !
  use kinds,     only : DP
  use io_global, only : stdout
  use parallel_include
  !
  implicit none
  save

  character(len=80), private :: err_msg = ' '
  ! 
  interface mp_sum
     module procedure mp_sum_i1
     module procedure mp_sum_iv
     module procedure mp_sum_im
  end interface
  
contains
  !
  subroutine mp_barrier(gid)
    IMPLICIT NONE
    INTEGER, OPTIONAL, INTENT(IN) :: gid
    INTEGER :: group
    INTEGER :: ierr
#if defined(__MPI)
    group = mpi_comm_world
    IF( PRESENT( gid ) ) group = gid
    CALL MPI_BARRIER(group,IERR)
    IF (ierr/=0) CALL mp_stop( 8066 )
#endif
  !
  end subroutine mp_barrier
  !
  SUBROUTINE mp_stop(code)
    IMPLICIT NONE
    INTEGER, INTENT (IN) :: code
    WRITE( stdout, fmt='( "*** error in Message Passing (mp) module ***")' )
    WRITE( stdout, fmt='( "*** error msg:  ",A60)' ) TRIM( err_msg )
    WRITE( stdout, fmt='( "*** error code: ",I5)' ) code
#if defined(__MPI)
    CALL mpi_abort(mpi_comm_world,code)
#endif
    STOP
  END SUBROUTINE mp_stop
  !
  SUBROUTINE mp_sum_i1(msg,gid)
    IMPLICIT NONE
    INTEGER, INTENT (INOUT) :: msg
    INTEGER, OPTIONAL, INTENT(IN) :: gid
    INTEGER :: group
    INTEGER :: msglen
#if defined(__MPI)
        msglen = 1
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_integer( msglen, msg, group, -1 )
#endif
  END SUBROUTINE mp_sum_i1
  !
  SUBROUTINE mp_sum_iv(msg,gid)
    IMPLICIT NONE
    INTEGER, INTENT (INOUT) :: msg(:)
    INTEGER, OPTIONAL, INTENT(IN) :: gid
    INTEGER :: group
    INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = size(msg)
        CALL reduce_base_integer( msglen, msg, group, -1 )
#endif
  END SUBROUTINE mp_sum_iv
  !
  SUBROUTINE mp_sum_im(msg,gid)
    IMPLICIT NONE
    INTEGER, INTENT (INOUT) :: msg(:,:)
    INTEGER, OPTIONAL, INTENT(IN) :: gid
    INTEGER :: group
    INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = size(msg)
        CALL reduce_base_integer( msglen, msg, group, -1 )
#endif
   END SUBROUTINE mp_sum_im
  !
end module mp

!------------------------------------------------------------------------------!
subroutine start_clock( str )
  !------------------------------------------------------------------------------!
  implicit none
  ! do nothing
  character(*) :: str
  if ( .false. ) write(*,*) str
end subroutine start_clock

!------------------------------------------------------------------------------!
subroutine stop_clock( str )
  !------------------------------------------------------------------------------!
  implicit none
  ! do nothing
  character(*) :: str
  if ( .false. ) write(*,*) str
end subroutine stop_clock

!----------------------------------------------------------------------------
SUBROUTINE reduce_base_integer( dim, ps, comm, root )
  !----------------------------------------------------------------------------
  !
  ! ... sums a distributed variable ps(dim) over the processors.
  ! ... This version uses a fixed-length buffer of appropriate (?) dim
  !
  USE kinds, ONLY : DP
  USE parallel_include  
  USE mp
  !
  !  In some MPI implementation the communication subsystem
  !  crashes when message exceeds a given size, so we need
  !  to break down MPI communications in smaller pieces
  !
#define __MSGSIZ_MAX 100000
#define __BCAST_MSGSIZ_MAX 100000
#define __USE_BARRIER
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)    :: dim
  INTEGER                 :: ps(dim)
  INTEGER,  INTENT(IN)    :: comm    ! communicator
  INTEGER,  INTENT(IN)    :: root    ! if root <  0 perform a reduction to all procs
                                     ! if root >= 0 perform a reduce only to root proc.
  !
#if defined (__MPI)  
  !
  INTEGER            :: info, n, nbuf, nproc, myid
  INTEGER, PARAMETER :: maxb = __MSGSIZ_MAX
  !
  INTEGER :: buff(maxb)  
  COMMON / mp_base_integer / buff
  !
  !
  CALL mpi_comm_size( comm, nproc, info )
  IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_comm_size', info )

  CALL mpi_comm_rank( comm, myid, info )
  IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_comm_rank', info )
  !
  IF ( dim <= 0 .OR. nproc <= 1 ) GO TO 1  ! go to the end of the subroutine
  !
  ! ... synchronize processes
  !
#if defined __USE_BARRIER
  CALL mp_barrier( comm )
#endif
  !
  nbuf = dim / maxb
  !
  DO n = 1, nbuf
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_INTEGER, MPI_SUM, root, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_reduce 1', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_INTEGER, MPI_SUM, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_allreduce 1', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     ELSE IF( root == myid ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     END IF
     !
  END DO
  !
  ! ... possible remaining elements < maxb
  !
  IF ( ( dim - nbuf * maxb ) > 0 ) THEN
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_INTEGER, MPI_SUM, root, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_reduce 2', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_INTEGER, MPI_SUM, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_allreduce 2', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     ELSE IF( root == myid ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     END IF
     !
  END IF
  !
1 CONTINUE
  !
#endif
  !
  RETURN
  !
end subroutine reduce_base_integer

