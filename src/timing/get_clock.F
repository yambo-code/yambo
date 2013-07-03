!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
FUNCTION get_clock( label )
  !
  !     get_clock( label )     return wall time measured by clock "label"
  !                            returns -1 if "label" has never been started
  !----------------------------------------------------------------------------
  !
  USE pars,      ONLY: DP
  USE timing,    ONLY: no, nclock, clock_label, walltime, &
&                       notrunning, called, t0wall, t0cpu
!
! ... See comments in subroutine print_this_clock about parallel case
!
!  USE mp,        ONLY : mp_max
!  USE mp_global, ONLY : intra_image_comm
  !
  IMPLICIT NONE
  !
  REAL(DP)         :: get_clock
  CHARACTER(len=*) :: label
  INTEGER          :: n
  !
  REAL(DP), EXTERNAL :: cclock
  !
  !
  IF ( no ) THEN
     !
     IF ( label == clock_label(1) ) THEN
        !
        get_clock = cclock()
        !
     ELSE
        !
        get_clock = notrunning
        !
     ENDIF
     !
     RETURN
     !
  ENDIF
  !
  DO n = 1, nclock
     !
     IF ( label == clock_label(n) ) THEN
        !
        IF ( t0cpu(n) == notrunning ) THEN
           !
           get_clock = walltime(n)
           !
        ELSE
           !
           get_clock = walltime(n) + cclock() - t0wall(n)
           !
        ENDIF
        !
        ! ... See comments in subroutine print_this_clock about parallel case
        !
        ! CALL mp_max( get_clock, intra_image_comm )
        !
        RETURN
        !
     ENDIF
     !
  ENDDO
  !
  ! ... clock not found
  !
  get_clock = notrunning
  !
  RETURN
  !
END FUNCTION get_clock
