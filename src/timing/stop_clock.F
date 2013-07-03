!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
SUBROUTINE stop_clock( label )
  !
  !     stop_clock( label )    stops  clock "label"
  !                            issues warning if "label" is either not running
  !                            or has never been started
  !----------------------------------------------------------------------------
  !
  USE pars,      ONLY : DP
  USE com,       ONLY : warning
  USE timing,    ONLY : no, nclock, clock_label, cputime, walltime, &
                        notrunning, called, t0cpu, t0wall, trace_depth
  !
  IMPLICIT NONE
  !
  CHARACTER(len=*) :: label
  !
  CHARACTER(len=12) :: label_
  INTEGER          :: n
  REAL(DP), EXTERNAL :: scnds, cclock
  !
  !
  IF ( no ) RETURN
  !
  ! ... prevent trouble if label is longer than 12 characters
  !
  label_ = trim ( label )
  !
  DO n = 1, nclock
     !
     IF ( clock_label(n) == label_ ) THEN
        !
        ! ... found previously defined clock : check if properly initialised,
        ! ... add elapsed time, increase the counter of calls
        !
        IF ( t0cpu(n) == notrunning ) THEN
           !
           !WRITE( stdout, '("stop_clock: clock # ",I2," for ",A12, &
           !               & " not running")' ) n, label
           !
        ELSE
           !
           cputime(n)   = cputime(n) + scnds() - t0cpu(n)
           walltime(n)  = walltime(n) + cclock() - t0wall(n)
           called(n)    = called(n) + 1
           !
           t0cpu(n)     = notrunning
           t0wall(n)    = notrunning
           !
        ENDIF
        !
        RETURN
        !
     ENDIF
     !
  ENDDO
  !
  ! ... clock not found
  !
  call warning("stop_clock: no clock for "//trim(label)//" found !")
  !
  RETURN
  !
END SUBROUTINE stop_clock
