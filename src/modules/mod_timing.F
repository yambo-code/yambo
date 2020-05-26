! 
! Copyright (C) 2004 WanT Group
! Copyright (C) 2000-2020 the YAMBO team
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
! <INFO>
!
MODULE timing_m
 !
 USE pars,          ONLY:SP,DP,lchlen
 USE openmp,        ONLY:master_thread
 USE stderr,        ONLY:log_as_a_file
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
 ! SUBROUTINE  timing_overview(list[,main_name])
 ! SUBROUTINE  timing_upto_now()
 ! SUBROUTINE  clock_start(obj)
 ! SUBROUTINE  clock_stop(obj)
 ! SUBROUTINE  clock_update(obj)
 ! SUBROUTINE  clock_find(list,name,found,index)
 ! </INFO>
 !
 INTEGER, PARAMETER             :: nclockx = 200
 !
 ! Overview
 !
 INTEGER :: MAX_name_length=0
 INTEGER :: MAX_calls_length=0
 !
 TYPE clock
   CHARACTER(lchlen)           :: name              ! clock name
   INTEGER                     :: cpu_id            ! CPU id
   INTEGER                     :: call_number       ! number of runs for this clock
   REAL(DP)                    :: start             ! last start
   REAL(DP)                    :: stop              ! last stop 
   REAL(DP)                    :: total_time        ! total time up to now
   LOGICAL                     :: running           ! true if clock is counting
   INTEGER                     :: indx              ! global clock index, if any
   LOGICAL                     :: alloc 
 END TYPE clock
 !     
 TYPE clock_list
   TYPE(clock), POINTER        :: clock(:) => null()
   CHARACTER(lchlen)           :: name              ! list name
   INTEGER                     :: nclock            ! actual number of clocks
   INTEGER                     :: nclock_max        ! max number of clocks
   LOGICAL                     :: alloc 
 END TYPE clock_list
 !
 TYPE(clock_list), TARGET, SAVE :: internal_list     ! internal use clock
 TYPE(clock_list), TARGET, SAVE :: global_list       ! global use clock
 !
 LOGICAL                        :: alloc = .FALSE.   ! global alloc flag
 !   
 ! end of declarations
 !
 INTERFACE ASSIGNMENT(=)
   !
   MODULE PROCEDURE clock_assignment
   !
 END INTERFACE
 !
 PUBLIC ::  nclockx
 PUBLIC ::  clock, clock_list, clock_update, ASSIGNMENT(=)
 PUBLIC ::  global_list
 PUBLIC ::  timing
 PUBLIC ::  timing_allocate
 PUBLIC ::  timing_deallocate
 PUBLIC ::  timing_upto_now
 PUBLIC ::  alloc
 PUBLIC ::  MAX_name_length,MAX_calls_length
 !
 CONTAINS
   !
   ! Subroutines
   !   
   SUBROUTINE timing(name,opr,list)
     IMPLICIT NONE
     CHARACTER(*),           INTENT(in)    :: name
     CHARACTER(*), OPTIONAL, INTENT(in)    :: opr
     TYPE(clock_list),  OPTIONAL, TARGET   :: list
     !
     CHARACTER(5)                          :: opr_
     LOGICAL                               :: found
     INTEGER                               :: index
     TYPE(clock_list), POINTER  :: listp
     !
#ifndef _TIMING
     return
#endif
     if (.not.master_thread) return
     !
     IF ( LEN( TRIM(name)) == 0 )  CALL error('timing: Invalid name')
     opr_ = " "
     IF ( PRESENT(opr) ) opr_ = TRIM(opr)
     !
     IF ( PRESENT( list ) ) THEN
         listp => list
     ELSE
         listp => global_list
     ENDIF
     ! 
     CALL clock_find(listp,name,found,index)
     !
     ! clock NOT found
     !
     IF ( .NOT. found ) THEN
       !
       IF ( .NOT. PRESENT(opr) .OR. TRIM(opr_) == "start" .OR. TRIM(opr_) == "START") THEN
         opr_ = "start"
         CALL clock_allocate(TRIM(name), listp%nclock, listp%clock(index))
       ELSE 
         CALL error('timing: Clock NOT found for operation '//TRIM(opr_)//' in '&
                     //TRIM(name))
       ENDIF
       !
     ELSE
       !
       ! clock found
       !
       IF ( listp%clock(index)%running )  THEN
         IF ( PRESENT(opr) .AND. TRIM(opr_) /= "stop" .AND. TRIM(opr_) /= "STOP" )  &
           CALL error('timing: Operation '//TRIM(opr_)//' NOT allowed in '&
                       //TRIM(name))
         opr_ = "stop"
       ELSE
          IF ( .NOT. PRESENT(opr) )  opr_ = "start"
       ENDIF
       !
     ENDIF
     ! 
     ! case selection
     ! 
     SELECT CASE ( TRIM(opr_) )  
       CASE("start","START") 
         CALL clock_start( listp%clock(index) ) 
       CASE("stop","STOP")
         CALL clock_stop( listp%clock(index) ) 
       CASE DEFAULT
         CALL error('timing:  Invalid operation '//TRIM(opr_))
     END SELECT
     !
   END SUBROUTINE timing
   !
   SUBROUTINE timing_allocate(nclock_max_)
     IMPLICIT NONE
     INTEGER,             INTENT(in)  :: nclock_max_
     !
#ifndef _TIMING
     return
#endif
     IF ( nclock_max_ < 1 ) CALL error('timing_allocate:  Invalid NCLOCK_MAX')
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
#ifndef _TIMING
     return
#endif
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
     IF ( obj%alloc ) CALL error(sub_name//': List already allocated')
     IF ( nclock_max_ < 1 ) CALL error(sub_name//': Invalid NCLOCK_MAX')
     IF ( LEN_TRIM(name) == 0) CALL error(sub_name//': Invalid NAME')
     !
     ALLOCATE( obj%clock(nclock_max_), STAT=ierr )
     IF ( ierr /= 0 ) CALL error(sub_name//': Unable to allocate CLOCK')
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
     IF ( .NOT. obj%alloc ) CALL error(sub_name//': List not yet allocated')
     DEALLOCATE( obj%clock, STAT=ierr)
     IF ( ierr /= 0 ) CALL error(sub_name//': Unable to deallocate CLOCK')
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
     IF ( obj%alloc ) CALL error('clock_allocate: Clock already allocated')
     IF ( LEN( TRIM(name)) == 0 )  CALL error('clock_allocate:  Invalid name')
     !
     nclock = nclock + 1
     obj%indx=nclock
     obj%name=TRIM(name)
     obj%cpu_id=-1
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
     IF ( .NOT. obj2%alloc ) CALL error('clock_assignment:  Clock2 not allocated')
     obj1%name = obj2%name
     obj1%cpu_id = obj2%cpu_id
     obj1%call_number = obj2%call_number
     obj1%start = obj2%start
     obj1%stop = obj2%stop
     obj1%total_time = obj2%total_time
     obj1%running = obj2%running
     obj1%indx = obj2%indx
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
     IF ( .NOT. list%alloc ) CALL error('clock_find:  List not yet allocated')
     IF ( LEN( TRIM(name)) == 0 )  CALL error('clock_find:  Invalid name')
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
     IF ( index > list%nclock_max ) CALL error('clock_find:  too many clocks')
     !
   END SUBROUTINE clock_find
   !
   SUBROUTINE clock_start(obj)
     use LIVE_t, ONLY:cti
#ifdef _NVTX
     use nvtx
#endif
     IMPLICIT NONE
     TYPE(clock), INTENT(inout) :: obj    
     !
     IF ( .NOT. obj%alloc  ) CALL error('clock_start:  clock not yet allocated')
     call cti(obj%start)
     obj%running = .TRUE.
     obj%call_number = obj%call_number + 1
#ifdef _NVTX
     call nvtxStartRange(obj%name,obj%indx)
#endif
   END SUBROUTINE clock_start
   !
   SUBROUTINE clock_stop(obj)
     use LIVE_t, ONLY:cti
#ifdef _NVTX
     use nvtx
#endif
     IMPLICIT NONE
     TYPE(clock), INTENT(inout) :: obj    
     !
     IF ( .NOT. obj%alloc  )   CALL error('clock_stop:  Clock NOT allocated')
     IF ( .NOT. obj%running  ) & 
           CALL error('clock_stop:  Clock '//TRIM(obj%name)//'NOT running')
     call cti(obj%stop)
     obj%total_time = obj%total_time + real(obj%stop-obj%start,DP)
     obj%running = .FALSE.
#ifdef _NVTX
     call nvtxEndRange()
#endif
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
   SUBROUTINE timing_upto_now()
     use pars,    ONLY:lchlen
     use com,     ONLY:msg
     use stderr,  ONLY:real2ch
     use LIVE_t,  ONLY:LIVE_message,time_string
     IMPLICIT NONE
     REAL(DP) :: total_time
     character(lchlen)::ch
     !
#ifndef _TIMING
     call msg('rn'," ")
     return
#endif
     !
     IF ( .NOT. internal_list%alloc ) CALL error('timing_upto_now:  Internal clock not allocated')
     CALL clock_update(internal_list%clock(1))
     !
     ! recovering over pools may create deadlocks
     !
     total_time = real(internal_list%clock(1)%total_time,SP)
     !
     ch='            [Time-Profile]: '//trim(time_string(total_time))
     call msg('rn',ch(:100))
     if (log_as_a_file) call LIVE_message(message="[TIMING]"//ch(:100),lfmt="n")
     !
   END SUBROUTINE timing_upto_now    
   !
END MODULE timing_m
