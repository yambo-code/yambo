!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
SUBROUTINE start_clock( label )
  !
  !     start_clock( label )   starts clock "label" (max 12 characters)
  !                            if "label" has never been started, initializes it
  !                            issues warning if "label" already started
  !----------------------------------------------------------------------------
  !
  USE pars,      ONLY : DP
  use com,       ONLY : warning
  USE timing,    ONLY : nclock, clock_label, notrunning, no, maxclock, &
                        t0cpu, t0wall, trace_depth
  !
  IMPLICIT NONE
  !
  CHARACTER(len=*) :: label
  !
  CHARACTER(len=12) :: label_
  INTEGER          :: n
  REAL(DP), EXTERNAL :: scnds, cclock
  !  !
  IF ( no .and. ( nclock == 1 ) ) RETURN
  !
  ! ... prevent trouble if label is longer than 12 characters
  !
  label_ = trim ( label )
  !
  DO n = 1, nclock
     !
     IF ( clock_label(n) == label_ ) THEN
        !
        ! ... found previously defined clock: check if not already started,
        ! ... store in t0cpu the starting time
        !
        IF ( t0cpu(n) /= notrunning ) THEN
!            WRITE( stdout, '("start_clock: clock # ",I2," for ",A12, &
!                           & " already started")' ) n, label_
        ELSE
           t0cpu(n)  = scnds()
           t0wall(n) = cclock()
        ENDIF
        !
        RETURN
        !
     ENDIF
     !
  ENDDO
  !
  ! ... clock not found : add new clock for given label
  !
  IF ( nclock == maxclock ) THEN
     !
     call warning(" Start_clock: Too many clocks! call ignored")
     !
  ELSE
     !
     nclock                                     = nclock + 1
     clock_label(nclock)        = label_
     t0cpu(nclock)                      = scnds()
     t0wall(nclock)                     = cclock()
     !
  ENDIF
  !
  RETURN
  !
END SUBROUTINE start_clock
