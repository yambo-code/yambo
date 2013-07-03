!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
SUBROUTINE print_clock( label )
  !
  !     print_clock( label )   print cpu and wall time measured by clock "label"
  !                            clock "label" may be running or stopped 
  !                            and remains in the same state
  !                            issues warning if "label" has never been started
  !----------------------------------------------------------------------------
  !
  USE pars,      ONLY : DP
  USE stderr,    ONLY : logfile
  USE timing,    ONLY : nclock, clock_label
  !
  IMPLICIT NONE
  !
  CHARACTER(len=*) :: label
  !
  CHARACTER(len=12) :: label_
  INTEGER          :: n
  !
  open(unit=13,file=trim(logfile),position='append')
  !
  IF ( label == ' ' ) THEN
     !
     WRITE( 13, * )
     !
     DO n = 1, nclock
        !
        CALL print_this_clock( n )
        !
     ENDDO
     !
  ELSE
     !
     ! ... prevent trouble if label is longer than 12 characters
     !
     label_ = trim ( label )
     !
     DO n = 1, nclock
        !
        IF ( clock_label(n) == label_ ) THEN
           !
           CALL print_this_clock( n )
           !
           exit
           !
        ENDIF
        !
     ENDDO
     !
  ENDIF
  !
  close(13)
  !
  RETURN
  !
END SUBROUTINE print_clock
!
!----------------------------------------------------------------------------
SUBROUTINE print_this_clock( n )
  !----------------------------------------------------------------------------
  !
  USE pars,      ONLY : DP
  USE stderr,    ONLY : logfile
  USE timing,    ONLY : no, nclock, clock_label, cputime, walltime, &
                        notrunning, called, t0cpu, t0wall
!
! ... See comments below about parallel case
!
!  USE mp,        ONLY : mp_max
!  USE mp_global, ONLY : intra_image_comm, my_image_id
  !
  IMPLICIT NONE
  !
  INTEGER  :: n
  REAL(DP) :: elapsed_cpu_time, elapsed_wall_time, nsec, msec
  INTEGER  :: nday, nhour, nmin, nmax, mday, mhour, mmin
  !
  REAL(DP), EXTERNAL :: scnds, cclock
  !
  !
  IF ( t0cpu(n) == notrunning ) THEN
     !
     ! ... clock stopped, print the stored value for the cpu time
     !
     elapsed_cpu_time = cputime(n)
     elapsed_wall_time= walltime(n)
     !
  ELSE
     !
     ! ... clock not stopped, print the current value of the cpu time
     !
     elapsed_cpu_time   = cputime(n) + scnds() - t0cpu(n)
     elapsed_wall_time  = walltime(n) + cclock() - t0wall(n)
     called(n)  = called(n) + 1
     !
  ENDIF
  !
  nmax = called(n)
  !
  ! ... In the parallel case there are several possible approaches
  ! ... The safest one is to leave each clock independent from the others
  ! ... Another possibility is to print the maximum across all processors
  ! ... This is done by uncommenting the following lines
  !
  ! CALL mp_max( elapsed_cpu_time, intra_image_comm )
  ! CALL mp_max( elapsed_wall_time, intra_image_comm )
  ! CALL mp_max( nmax, intra_image_comm )
  !
  ! ... In the last line we assume that the maximum cpu time
  ! ... is associated to the maximum number of calls
  ! ... NOTA BENE: by uncommenting the above lines you may run into
  ! ... serious trouble if clocks are not started on all nodes
  !
  IF ( n == 1 ) THEN
     !
     ! ... The first clock is written as days/hour/min/sec
     !
     !
     nday  = elapsed_cpu_time / 86400
     nsec  = elapsed_cpu_time - 86400 * nday
     nhour = nsec / 3600
     nsec  = nsec - 3600 * nhour
     nmin  = nsec / 60
     nsec  = nsec - 60 * nmin
     !
     ! ... The first clock writes elapsed (wall) time as well
     !
     mday  = elapsed_wall_time / 86400
     msec  = elapsed_wall_time - 86400 * mday
     mhour = msec / 3600
     msec  = msec - 3600 * mhour
     mmin  = msec / 60
     msec  = msec - 60 * mmin
     !
     IF ( nday > 0 .or. mday > 0 ) THEN
        !
        WRITE( 13, &
               '(5X,A12," : ",3X,I2,"d",3X,I2,"h",I2, "m CPU ", &
           &            "   ",3X,I2,"d",3X,I2,"h",I2, "m WALL"/)' ) &
             clock_label(n), nday, nhour, nmin, mday, mhour, mmin
        !
     ELSEIF ( nhour > 0 .or. mhour > 0 ) THEN
        !
        WRITE( 13, &
               '(5X,A12," : ",3X,I2,"h",I2,"m CPU ", &
           &            "   ",3X,I2,"h",I2,"m WALL"/)' ) &
             clock_label(n), nhour, nmin, mhour, mmin
        !
     ELSEIF ( nmin > 0 .or. mmin > 0 ) THEN
        !
        WRITE( 13, &
               '(5X,A12," : ",I2,"m",F5.2,"s CPU ", &
               &        "   ",I2,"m",F5.2,"s WALL"/)' ) &
             clock_label(n), nmin, nsec, mmin, msec
        !
     ELSE
        !
        WRITE( 13, &
               '(5X,A12," : ",3X,F5.2,"s CPU ",7X,F5.2,"s WALL"/)' )&
             clock_label(n), nsec, msec
        !
     ENDIF
     !
  ELSEIF ( nmax == 1 .or. t0cpu(n) /= notrunning ) THEN
     !
     ! ... for clocks that have been called only once
     !
     WRITE( 13, &
            '(5X,A12," : ",F9.2,"s CPU ",F9.2,"s WALL (",I8," calls)")' ) &
                clock_label(n), elapsed_cpu_time, elapsed_wall_time, nmax
     !
  ELSEIF ( nmax == 0 ) THEN
     !
     ! ... for clocks that have never been called
     !
     WRITE( 13, &
            '("print_this: clock # ",I2," for ",A12," never called !"/)' ) &
                n, clock_label(n)
     !
  ELSE
     !
     ! ... for all other clocks
     !
     WRITE( 13, &
        '(5X,A12," : ",F9.2,"s CPU ",F9.2,"s WALL (",I8," calls)")' ) &
        clock_label(n), elapsed_cpu_time, elapsed_wall_time, nmax
     !
  ENDIF
  !
  RETURN
  !
END SUBROUTINE print_this_clock

