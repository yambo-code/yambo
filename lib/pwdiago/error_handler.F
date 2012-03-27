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
  !
  IMPLICIT NONE
#if defined _MPI
      include 'mpif.h'
#endif
  !
  CHARACTER(LEN=*), INTENT(IN) :: calling_routine, message
    ! the name of the calling calling_routinee
    ! the output messagee
  INTEGER,          INTENT(IN) :: ierr
    ! the error flag
  INTEGER                      :: mpime, mpierr
    ! the task id  
  INTEGER, parameter :: crashunit=100
  !
  !
  IF ( ierr <= 0 ) RETURN
  !
#if !defined _MPI
  !
  ! ... the error message is written un the "*" unit
  !
  WRITE( UNIT = *, FMT = '(/,1X,78("%"))' )
  WRITE( UNIT = *, &
         FMT = '(5X,"from ",A," : error #",I10)' ) calling_routine, ierr
  WRITE( UNIT = *, FMT = '(5X,A)' ) message
  WRITE( UNIT = *, FMT = '(1X,78("%"),/)' )
  !
  WRITE( *, '("     stopping ...")' )
  !
#else
  !
  mpime = 0
  !
  CALL MPI_COMM_RANK( MPI_COMM_WORLD, mpime, mpierr )
  !
  !  .. write the message to a file and close it before exiting
  !  .. this will prevent loss of information on systems that
  !  .. do not flush the open streams
  !  .. added by C.C.
  !
  if(mpime==0) then
    OPEN( UNIT = crashunit, FILE = "CRASH", &
        POSITION = 'APPEND', STATUS = 'UNKNOWN' )
    !      
    WRITE( UNIT = crashunit, FMT = '(/,1X,78("%"))' )
    WRITE( UNIT = crashunit, FMT = '(5X,"task #",I10)' ) mpime
    WRITE( UNIT = crashunit, &
          FMT = '(5X,"from ",A," : error #",I10)' ) calling_routine, ierr
    WRITE( UNIT = crashunit, FMT = '(5X,A)' ) message
    WRITE( UNIT = crashunit, FMT = '(1X,78("%"),/)' )
    !
    CLOSE( UNIT = crashunit )
  endif
  !
  ! ... try to exit in a smooth way
  !
  CALL MPI_ABORT( MPI_COMM_WORLD, mpierr )
  !
  CALL MPI_FINALIZE( mpierr )
  !
#endif
  !
  STOP 2
  !
  RETURN
  !
END SUBROUTINE errore
