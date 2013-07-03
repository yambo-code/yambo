!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
SUBROUTINE init_clocks( go )
  !
  !     init_clocks( go )    initialization - must be called first
  !                          go = .TRUE. : up to "maxclock" clocks can be started
  !                          go = .FALSE.: only clock #1 can be started
  !----------------------------------------------------------------------------
  !
  ! ... go = .TRUE.  : clocks will run
  ! ... go = .FALSE. : only clock #1 will run
  !
  USE pars,      ONLY : DP
  USE timing,    ONLY : called, t0cpu, cputime, no, notrunning, maxclock, &
&                       clock_label, walltime, t0wall
  !
  implicit none
  !
  LOGICAL :: go
  INTEGER :: n
  !
  !
  no = .not. go
  !
  DO n = 1, maxclock
     !
     called(n)      = 0
     cputime(n)     = 0.0_DP
     t0cpu(n)       = notrunning
     walltime(n)        = 0.0_DP
     t0wall(n)          = notrunning
     clock_label(n) = ' '
     !
  ENDDO
  !
  RETURN
  !
END SUBROUTINE init_clocks
