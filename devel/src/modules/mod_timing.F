! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
! <INFO>
!
MODULE timing_module
 !
 USE kinds,             ONLY : dbl
 USE mp_global,         ONLY : nproc
 USE mp,                ONLY : mp_sum
 !
 IMPLICIT NONE
 PRIVATE
 ! This module contains the definition of CLOCK type and CLOCK_LIST type;
 ! handles the timing all over the code
 !
 ! The low-level timing routine is from ESPRESSO package (cptimer.c)
 ! 
 ! routines in this module:
 ! SUBROUTINE  timing(name[,opr])
 ! SUBROUTINE  timing_allocate(nclock_max)
 ! SUBROUTINE  timing_deallocate()
 ! SUBROUTINE  timing_overview(unit,list[,main_name])
 ! SUBROUTINE  timing_upto_now(unit)
 ! SUBROUTINE  clock_start(obj)
 ! SUBROUTINE  clock_stop(obj)
 ! SUBROUTINE  clock_update(obj)
 ! SUBROUTINE  clock_find(list,name,found,index)
 ! </INFO>
 !
 !
 INTEGER, PARAMETER             :: nclockx = 500
 INTEGER, PARAMETER             :: str_len = 200
 !
 TYPE clock
   CHARACTER(str_len)          :: name              ! clock name
   INTEGER                     :: call_number       ! number of runs for this clock
   REAL(dbl)                   :: start             ! last start
   REAL(dbl)                   :: stop              ! last stop 
   REAL(dbl)                   :: total_time        ! total time up to now
   LOGICAL                     :: running           ! true if clock is counting
   LOGICAL                     :: alloc 
 END TYPE clock
 !     
 TYPE clock_list
   TYPE(clock), POINTER        :: clock(:)
   CHARACTER(str_len)          :: name              ! list name
   INTEGER                     :: nclock            ! actual number of clocks
   INTEGER                     :: nclock_max        ! max number of clocks
   LOGICAL                     :: alloc 
 END TYPE clock_list
 !
 TYPE(clock_list), SAVE         :: internal_list     ! internal use clock
 TYPE(clock_list), SAVE         :: global_list       ! internal use clock
 LOGICAL                        :: alloc = .FALSE.   ! global alloc flag
 !   
 REAL(dbl)  :: cclock 
 EXTERNAL   :: cclock
 !
 ! end of declarations
 !
 INTERFACE ASSIGNMENT(=)
   MODULE PROCEDURE clock_assignment
 END INTERFACE
 !
 PUBLIC ::  nclockx
 PUBLIC ::  clock, clock_list, ASSIGNMENT(=)
 PUBLIC ::  global_list
 PUBLIC ::  timing
 PUBLIC ::  timing_allocate
 PUBLIC ::  timing_deallocate
 PUBLIC ::  timing_overview
 PUBLIC ::  timing_upto_now
 PUBLIC ::  alloc
 !
 CONTAINS
   !
   ! Subroutines
   !   
   SUBROUTINE timing(name,opr)
     IMPLICIT NONE
     CHARACTER(*),           INTENT(in)    :: name
     CHARACTER(*), OPTIONAL, INTENT(in)    :: opr
     CHARACTER(5)                          :: opr_
     LOGICAL                               :: found
     INTEGER                               :: index
     !
     IF ( LEN( TRIM(name)) == 0 )  CALL errore('timing','Invalid name',1)
     opr_ = " "
     IF ( PRESENT(opr) ) opr_ = TRIM(opr)
     !
     CALL clock_find(global_list,name,found,index)
     !
     ! clock NOT found
     !
     IF ( .NOT. found ) THEN
       IF ( .NOT. PRESENT(opr) .OR. TRIM(opr_) == "start" .OR. TRIM(opr_) == "START") THEN
         opr_ = "start"
         CALL clock_allocate(TRIM(name), global_list%nclock, global_list%clock(index))
       ELSE 
         CALL errore('timing','Clock NOT found for operation '//TRIM(opr_)//' in '&
                     //TRIM(name),1)
       ENDIF
     ELSE
     !
     ! clock found
     !
     IF ( global_list%clock(index)%running )  THEN
       IF ( PRESENT(opr) .AND. TRIM(opr_) /= "stop" .AND. TRIM(opr_) /= "STOP" )  &
         CALL errore('timing','Operation '//TRIM(opr_)//' NOT allowed in '&
                     //TRIM(name),1)
         opr_ = "stop"
       ELSE
         IF ( .NOT. PRESENT(opr) )  opr_ = "start"
       ENDIF
     ENDIF
     ! 
     ! case selection
     ! 
     SELECT CASE ( TRIM(opr_) )  
       CASE("start","START") 
         CALL clock_start( global_list%clock(index) ) 
       CASE("stop","STOP")
         CALL clock_stop( global_list%clock(index) ) 
       CASE DEFAULT
         CALL errore('timing','Invalid operation '//TRIM(opr_),1)
     END SELECT
     !
   END SUBROUTINE timing
   !
   SUBROUTINE timing_allocate(nclock_max_)
     IMPLICIT NONE
     INTEGER,             INTENT(in)  :: nclock_max_
     !
     IF ( nclock_max_ < 1 ) CALL errore('timing_allocate','Invalid NCLOCK_MAX',1)
     !
     ! public clocks
     !
     global_list%alloc = .FALSE.
     CALL clock_list_allocate(global_list,nclock_max_,'global')
     !
     ! internal clock
     !
     internal_list%alloc = .FALSE.
     CALL clock_list_allocate(internal_list,1,'internal')
     CALL clock_allocate('internal',internal_list%nclock,internal_list%clock(1))
     CALL clock_start(internal_list%clock(1))
     alloc = .TRUE.
     !
   END SUBROUTINE timing_allocate
   !
   SUBROUTINE timing_deallocate()
     IMPLICIT NONE
     CALL clock_list_deallocate(global_list)
     CALL clock_stop(internal_list%clock(1))
     CALL clock_list_deallocate(internal_list)
     alloc = .FALSE.
   END SUBROUTINE timing_deallocate
   !
   SUBROUTINE clock_list_allocate(obj,nclock_max_,name)
     IMPLICIT NONE
     TYPE(clock_list),       INTENT(inout) :: obj    
     INTEGER,                INTENT(in)    :: nclock_max_     
     CHARACTER(*),           INTENT(in)    :: name
     CHARACTER(19)                         :: sub_name='clock_list_allocate'
     INTEGER                               :: iclock, ierr
     !
     IF ( obj%alloc ) CALL errore(sub_name,'List already allocated',1)
     IF ( nclock_max_ < 1 ) CALL errore(sub_name,'Invalid NCLOCK_MAX',1)
     IF ( LEN_TRIM(name) == 0) CALL errore(sub_name,'Invalid NAME',1)
     !
     ALLOCATE( obj%clock(nclock_max_), STAT=ierr )
     IF ( ierr /= 0 ) CALL errore(sub_name,'Unable to allocate CLOCK',ABS(ierr))
     !
     DO iclock=1,nclock_max_
       obj%clock(iclock)%alloc = .FALSE.
     ENDDO
     !
     obj%name = TRIM(name)
     obj%nclock = 0
     obj%nclock_max = nclock_max_
     obj%alloc=.TRUE.
   END SUBROUTINE clock_list_allocate
   !
   SUBROUTINE clock_list_deallocate(obj)
     IMPLICIT NONE
     TYPE(clock_list),       INTENT(inout) :: obj    
     CHARACTER(21)                         :: sub_name='clock_list_deallocate'
     INTEGER                               :: ierr
     IF ( .NOT. obj%alloc ) CALL errore(sub_name,'List not yet allocated',1)
     DEALLOCATE( obj%clock, STAT=ierr)
     IF ( ierr /= 0 ) CALL errore(sub_name,'Unable to deallocate CLOCK',ABS(ierr))
     obj%nclock = 0
     obj%nclock_max = 0
     obj%alloc=.FALSE.
   END SUBROUTINE clock_list_deallocate
   !
   SUBROUTINE clock_allocate(name,nclock,obj)
     IMPLICIT NONE
     CHARACTER(*),          INTENT(in)    :: name
     INTEGER,               INTENT(inout) :: nclock
     TYPE(clock),           INTENT(inout) :: obj    
     !
     IF ( obj%alloc ) CALL errore('clock_allocate','Clock already allocated',1)
     IF ( LEN( TRIM(name)) == 0 )  CALL errore('clock_allocate','Invalid name',1)
     !
     nclock = nclock + 1
     obj%name=TRIM(name)
     obj%call_number=0
     obj%start=0.0
     obj%stop=0.0
     obj%total_time=0.0
     obj%running=.FALSE.
     obj%alloc=.TRUE.
   END SUBROUTINE clock_allocate
   !
   SUBROUTINE clock_assignment(obj1,obj2)
     IMPLICIT NONE
     TYPE(clock),    INTENT(inout) :: obj1    
     TYPE(clock),    INTENT(in)    :: obj2    
     IF ( .NOT. obj2%alloc ) CALL errore('clock_assignment','Clock2 not allocated',1)
     obj1%name = obj2%name
     obj1%call_number = obj2%call_number
     obj1%start = obj2%start
     obj1%stop = obj2%stop
     obj1%total_time = obj2%total_time
     obj1%running = obj2%running
     obj1%alloc = .TRUE.
   END SUBROUTINE clock_assignment
   !
   SUBROUTINE clock_find(list,name,found,index)
     IMPLICIT NONE
     TYPE(clock_list),      INTENT(in)    :: list
     CHARACTER(*),          INTENT(in)    :: name
     LOGICAL,               INTENT(out)   :: found
     INTEGER,               INTENT(out)   :: index
     INTEGER                              :: i
     IF ( .NOT. list%alloc ) CALL errore('clock_find','List not yet allocated',1)
     IF ( LEN( TRIM(name)) == 0 )  CALL errore('clock_find','Invalid name',1)
     found = .FALSE.
     index = 0
     DO i=1,list%nclock
       IF ( TRIM(list%clock(i)%name) == TRIM(name) .AND. list%clock(i)%alloc ) THEN 
         index = i
         found = .TRUE.
         EXIT
       ENDIF
     ENDDO
     !
     ! clock not found, pointing to next available clock
     !
     IF ( .NOT. found ) index = list%nclock + 1
     IF ( index > list%nclock_max ) CALL errore('clock_find','too many clocks',index)
     !
   END SUBROUTINE clock_find
   !
   SUBROUTINE clock_start(obj)
     IMPLICIT NONE
     TYPE(clock),            INTENT(inout) :: obj    
     IF ( .NOT. obj%alloc  ) CALL errore('clock_start','clock not yet allocated',1)
     obj%start = cclock()
     obj%running = .TRUE.
     obj%call_number = obj%call_number + 1
   END SUBROUTINE clock_start
   !
   SUBROUTINE clock_stop(obj)
     IMPLICIT NONE
     TYPE(clock),           INTENT(inout) :: obj    
     IF ( .NOT. obj%alloc  )   CALL errore('clock_stop','Clock NOT allocated',1)
     IF ( .NOT. obj%running  ) & 
           CALL errore('clock_stop','Clock '//TRIM(obj%name)//'NOT running',1)
     obj%stop = cclock()
     obj%total_time = obj%total_time + obj%stop - obj%start
     obj%running = .FALSE.
   END SUBROUTINE clock_stop
   !
   SUBROUTINE clock_update(obj)
     IMPLICIT NONE
     TYPE(clock),           INTENT(inout) :: obj    
     IF ( obj%running ) THEN 
       CALL clock_stop(obj) 
       CALL clock_start(obj) 
       obj%call_number = obj%call_number -1 
     ENDIF
   END SUBROUTINE clock_update
   !
   SUBROUTINE clock_write(unit,obj,form)
     IMPLICIT NONE
     INTEGER,                INTENT(in) :: unit
     TYPE(clock),         INTENT(inout) :: obj    
     CHARACTER(*), OPTIONAL, INTENT(in) :: form
     CHARACTER(3)                       :: form_
     CHARACTER(256)                     :: str
     INTEGER                            :: nhour,nmin
     INTEGER                            :: call_number
     REAL(dbl)                          :: total_time
     REAL(dbl)                          :: nsec
     !
     form_="sec"
     IF ( PRESENT(form) ) form_ = TRIM(form)
     CALL clock_update(obj)
     !
     ! define an average over the pools
     !
     total_time  = obj%total_time
     call_number = obj%call_number
     !
     ! do suitable averages only for MPI related clocks
     !
     str = obj%name
     str(4:) = ' '
     IF ( TRIM(str) == 'mp_' .OR. TRIM(str) == 'para_' ) THEN
       !
       CALL mp_sum( total_time )
       total_time = total_time / REAL( nproc, dbl )
       !
       CALL mp_sum( call_number )
       call_number = NINT ( call_number / REAL( nproc, dbl ) )
       !
     ENDIF
     !
     IF ( ionode ) THEN
       !
       SELECT CASE ( TRIM(form_) ) 
         CASE ( "hms" )
           nhour = INT( total_time / 3600 )
           nmin =  INT( (total_time-3600 * nhour) / 60 )
           nsec =  INT( total_time-3600 * nhour - 60 * nmin )
           IF ( call_number == 1 )  THEN
             IF (nhour > 0) THEN
               WRITE (unit, '(5x,a20," : ",3x,i2,"h",i2,"m CPU ")') &
                           TRIM(obj%name), nhour, nmin
             ELSEIF (nmin > 0) THEN
               WRITE (unit, '(5x,a20," : ",i2,"m",f5.2,"s CPU ")') &
                     TRIM(obj%name), nmin, nsec
             ELSE
               WRITE (unit, '(5x,a20," : ",3x,f5.2,"s CPU ")') &
                     TRIM(obj%name), nsec
             ENDIF
           ELSE
             IF (nhour > 0) THEN
               WRITE(unit,'(5x,a20," : ",3x,i2,"h",i2,"m CPU (", &
                     &  i8," calls,",f8.3," s avg)")') TRIM(obj%name), nhour, nmin, &
                     call_number , total_time / REAL( call_number, dbl )
             ELSEIF (nmin > 0) THEN
               WRITE (unit, '(5x,a20," : ",i2,"m",f5.2,"s CPU (", &
                     &    i8," calls,",f8.3," s avg)")') TRIM(obj%name), nmin, nsec, &
                          call_number , total_time / REAL( call_number, dbl )
             ELSE
               WRITE (unit, '(5x,a20," : ",3x,f5.2,"s CPU (", &
                     &    i8," calls,",f8.3," s avg)")') TRIM(obj%name), nsec, &
                          call_number , total_time / REAL( call_number, dbl )
             ENDIF
           ENDIF
         CASE ( "sec" )
         !
         ! time in seconds
         !
         IF ( call_number == 1) THEN
           WRITE (unit, '(5x,a20," :",f9.2,"s CPU")') TRIM(obj%name), total_time
         ELSE
           WRITE (unit, '(5x,a20," :",f9.2,"s CPU (", i8," calls,",f8.3," s avg)")')  &
                        TRIM(obj%name) , total_time , call_number ,   &
                        total_time / REAL( call_number, dbl )
         ENDIF
         CASE DEFAULT
           CALL errore('clock_write','Invalid FORM '//TRIM(form_),1 )
       END SELECT
       !
     ENDIF
     !
   END SUBROUTINE clock_write        
   !
   SUBROUTINE timing_upto_now(unit)
     IMPLICIT NONE
     INTEGER,                INTENT(in) :: unit
     REAL(dbl) :: total_time
     !
     IF ( .NOT. internal_list%alloc ) & 
          CALL errore('timing_upto_now','Internal clock not allocated',1)
     CALL clock_update(internal_list%clock(1))
     !
     ! recovering over pools may create deadlocks
     !
     total_time  = internal_list%clock(1)%total_time
     !
     !CALL mp_sum( total_time )
     !total_time = total_time / REAL( nproc, dbl )
     !
     IF ( ionode ) THEN
       WRITE(unit,"(30x,'Total time spent up to now :',F9.2,' secs',/)") total_time
     ENDIF
     !
   END SUBROUTINE timing_upto_now    
   !
   SUBROUTINE timing_overview(unit,list,main_name)
     IMPLICIT NONE
     TYPE(clock_list),       INTENT(in) :: list
     INTEGER,                INTENT(in) :: unit
     CHARACTER(*),           INTENT(in) :: main_name
     TYPE(clock)                        :: tmp_clock
     CHARACTER(20)                      :: form
     INTEGER                            :: i
     !
     IF ( .NOT. list%alloc ) CALL errore('timing_overview','list not allocated',1)
     IF ( ionode ) THEN
       !
       INQUIRE(UNIT=unit,FORM=form)
       IF ( TRIM(form) ==  "unformatted" .OR. TRIM(form) == "UNFORMATTED" ) &
          CALL errore('Timing_overview','UNIT unformatted',1)
       !
     ENDIF
     !
     IF (ionode) WRITE(unit,"(/,3x,'<',a,' routines>')") TRIM(list%name)
     !
     IF ( list%nclock == 0 ) THEN
       IF ( ionode ) WRITE(unit,"(7x,'No clock to display',/)") 
       RETURN
     ENDIF
     !
     IF( ionode ) WRITE(unit,"(13x,'clock number : ',i5,/)") list%nclock
     !
     DO i=1,list%nclock 
       !
       tmp_clock = list%clock(i)
       IF ( TRIM(list%clock(i)%name) == TRIM(main_name) .OR. &
                 list%clock(i)%total_time >= 1000           ) THEN 
         CALL clock_write(unit,tmp_clock,FORM="hms")
         IF ( TRIM(list%clock(i)%name) == TRIM(main_name) .AND. ionode)  WRITE(unit,"()")
       ELSE
         CALL clock_write(unit,tmp_clock,FORM="sec")
       ENDIF
       !
     ENDDO
     !
     IF( ionode ) WRITE(unit,"(/)")
     !
   END SUBROUTINE timing_overview
   !
END MODULE timing_module
