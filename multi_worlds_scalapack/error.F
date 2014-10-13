!
! Copyright (C) 2001-2007 Quantum-ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE errore( calling_routine, message, ierr )
  !----------------------------------------------------------------------------
  !
  ! ... This is a simple routine which writes an error message to output: 
  ! ... if ierr <= 0 it does nothing, 
  ! ... if ierr  > 0 it stops.
  !
  ! ...          **** Important note for parallel execution ***
  !
  ! ... in parallel execution unit 6 is written only by the first node;
  ! ... all other nodes have unit 6 redirected to nothing (/dev/null).
  ! ... As a consequence an error not occurring on the first node
  ! ... will be invisible. For T3E and ORIGIN machines, this problem
  ! ... is solved by writing an error message to unit * instead of 6.
  ! ... Whenever possible (IBM SP machines), we write to the standard
  ! ... error, unit 0 (the message will appear in the error files 
  ! ... produced by loadleveler).
  !
  IMPLICIT NONE
  !
#if defined __MPI
  INCLUDE 'mpif.h'
#endif
  !
  INTEGER,       PARAMETER :: stdout = 6
  INTEGER,       PARAMETER :: crashunit = 50
  CHARACTER(10), PARAMETER :: crash_file = "CRASH"
  !
  CHARACTER(LEN=*), INTENT(IN) :: calling_routine, message
    ! the name of the calling calling_routinee
    ! the output messagee
  INTEGER,          INTENT(IN) :: ierr
    ! the error flag
#if defined __MPI
  INTEGER                      :: mpierr
#endif
    ! the task id  
    !
  !
  !
  IF ( ierr <= 0 ) RETURN
  !
  ! ... the error message is written un the "*" unit
  !
  WRITE( UNIT = *, FMT = '(/,1X,78("%"))' )
  WRITE( UNIT = *, &
         FMT = '(5X,"from ",A," : error #",I10)' ) TRIM(calling_routine), ierr
  WRITE( UNIT = *, FMT = '(5X,A)' ) message
  WRITE( UNIT = *, FMT = '(1X,78("%"),/)' )
  WRITE( *, '("     stopping ...")' )
  !
#if defined __MPI
  !
  !CALL MPI_ABORT( MPI_COMM_WORLD, mpierr )
  CALL MPI_FINALIZE( mpierr )
  !
#endif
  !
  STOP 2
  !
  RETURN
  !
END SUBROUTINE errore
