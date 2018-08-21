! 
! Copyright (C) 2004 WanT Group
! Copyright (C) 2000-2018 the YAMBO team
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
 USE pars,          ONLY:SP,DP
 USE com,           ONLY:error
 USE parallel_m,    ONLY:ncpu,PP_redux_wait,mpi_comm_world
 USE openmp,        ONLY:master_thread
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
 INTEGER, PARAMETER             :: str_len = 200
 !
 TYPE clock
   CHARACTER(str_len)          :: name              ! clock name
   INTEGER                     :: call_number       ! number of runs for this clock
   REAL(DP)                    :: start             ! last start
   REAL(DP)                    :: stop              ! last stop 
   REAL(DP)                    :: total_time        ! total time up to now
   LOGICAL                     :: running           ! true if clock is counting
   LOGICAL                     :: alloc 
 END TYPE clock
 !     
 TYPE clock_list
   TYPE(clock), POINTER        :: clock(:) => null()
   CHARACTER(str_len)          :: name              ! list name
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
     IF ( .NOT. obj2%alloc ) CALL error('clock_assignment:  Clock2 not allocated')
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
     IMPLICIT NONE
     TYPE(clock),            INTENT(inout) :: obj    
     IF ( .NOT. obj%alloc  ) CALL error('clock_start:  clock not yet allocated')
     call cti(obj%start)
     obj%running = .TRUE.
     obj%call_number = obj%call_number + 1
   END SUBROUTINE clock_start
   !
   SUBROUTINE clock_stop(obj)
     use LIVE_t, ONLY:cti
     IMPLICIT NONE
     TYPE(clock),           INTENT(inout) :: obj    
     IF ( .NOT. obj%alloc  )   CALL error('clock_stop:  Clock NOT allocated')
     IF ( .NOT. obj%running  ) & 
           CALL error('clock_stop:  Clock '//TRIM(obj%name)//'NOT running')
     call cti(obj%stop)
     obj%total_time = obj%total_time + real(obj%stop-obj%start,DP)
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
   SUBROUTINE clock_write(obj,form)
     use pars,       ONLY:lchlen
     use LIVE_t,     ONLY:LIVE_message
     use com,        ONLY:msg
     IMPLICIT NONE
     TYPE(clock),         INTENT(inout) :: obj    
     CHARACTER(*), OPTIONAL, INTENT(in) :: form
     CHARACTER(4)                       :: form_
     CHARACTER(256)                     :: str
     CHARACTER(lchlen)                  :: local_ch
     INTEGER                            :: nday,nhour,nmin
     INTEGER                            :: call_number
     REAL(DP)                           :: total_time
     REAL(DP)                           :: nsec
     !
     form_="sec "
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
       CALL PP_redux_wait( total_time )
       total_time = total_time / REAL( ncpu, DP )
       !
       CALL PP_redux_wait( (/call_number/) )
       call_number = NINT ( call_number / REAL( ncpu, DP ) )
       !
     ENDIF
     !
     SELECT CASE ( TRIM(form_) ) 
       CASE ( "dhms" )
         nday  = INT(  total_time/ 86400 )
         nhour = INT( (total_time -86400*nday)/3600 )
         nmin =  INT( (total_time -86400*nday -3600*nhour)/60 )
         nsec =  INT(  total_time -86400*nday -3600*nhour -60*nmin )
         IF ( call_number == 1 )  THEN
           IF (nday > 0) THEN
             WRITE (local_ch, '(a35," : ",3x,i2,"d",i2,"h CPU ")') &
                         TRIM(obj%name), nday, nhour
           ELSEIF (nhour > 0) THEN
             WRITE (local_ch, '(a35," : ",3x,i2,"h",i2,"m CPU ")') &
                         TRIM(obj%name), nhour, nmin
           ELSEIF (nmin > 0) THEN
             WRITE (local_ch, '(a35," : ",i2,"m",f5.2,"s CPU ")') &
                   TRIM(obj%name), nmin, nsec
           ELSE
             WRITE (local_ch, '(a35," : ",3x,f5.2,"s CPU ")') &
                   TRIM(obj%name), nsec
           ENDIF
           if (ncpu>1) call LIVE_message(message="[TIMING]"//local_ch(:100),lfmt="n")
           call msg('r',local_ch(:100))
         ELSE
           IF (nday > 0) THEN
             WRITE(local_ch,'(a35," : ",3x,i2,"d",i2,"h CPU (", &
                   &  i8," calls,",f8.3," s avg)")') TRIM(obj%name), nday, nhour, &
                   call_number , total_time / REAL( call_number, DP )
           ELSEIF (nhour > 0) THEN
             WRITE(local_ch,'(a35," : ",3x,i2,"h",i2,"m CPU (", &
                   &  i8," calls,",f8.3," s avg)")') TRIM(obj%name), nhour, nmin, &
                   call_number , total_time / REAL( call_number, DP )
           ELSEIF (nmin > 0) THEN
             WRITE (local_ch, '(a35," : ",i2,"m",f5.2,"s CPU (", &
                   &    i8," calls,",f8.3," s avg)")') TRIM(obj%name), nmin, nsec, &
                        call_number , total_time / REAL( call_number, DP )
           ELSE
             WRITE (local_ch, '(a35," : ",3x,f5.2,"s CPU (", &
                   &    i8," calls,",f8.3," s avg)")') TRIM(obj%name), nsec, &
                        call_number , total_time / REAL( call_number, DP )
           ENDIF
           if (ncpu>1) call LIVE_message(message="[TIMING]"//local_ch(:100),lfmt="n")
           call msg('r',local_ch(:100))
         ENDIF
       CASE ( "sec " )
         !
         ! time in seconds
         !
         IF ( call_number == 1) THEN
           WRITE (local_ch, '(a35," :",f9.2,"s CPU")') TRIM(obj%name), total_time
         ELSE
           WRITE (local_ch, '(a35," :",f9.2,"s CPU (", i8," calls,",f8.3," s avg)")')  &
                        TRIM(obj%name) , total_time , call_number ,   &
                        total_time / REAL( call_number, DP )
         ENDIF
         if (ncpu>1) call LIVE_message(message="[TIMING]"//local_ch(:100),lfmt="n")
         call msg('r',local_ch(:100))
       CASE DEFAULT
         CALL error('clock_write:  Invalid FORM '//TRIM(form_) )
     END SELECT
     !
   END SUBROUTINE clock_write        
   !
   SUBROUTINE timing_upto_now()
     use pars,    ONLY:lchlen
     use com,     ONLY:msg
     use stderr,  ONLY:real2ch
     use LIVE_t,  ONLY:LIVE_message
     IMPLICIT NONE
     REAL(SP) :: total_time
     character(lchlen)::ch
     !
     IF ( .NOT. internal_list%alloc ) CALL error('timing_upto_now:  Internal clock not allocated')
     CALL clock_update(internal_list%clock(1))
     !
     ! recovering over pools may create deadlocks
     !
     total_time = real(internal_list%clock(1)%total_time,SP)
     !
     ch='            [Time-Profile]: '//trim(real2ch(total_time))//'s'
     call msg('rn',ch(:100))
     if (ncpu>1) call LIVE_message(message="[TIMING]"//ch(:100),lfmt="n")
     !
   END SUBROUTINE timing_upto_now    
   !
   SUBROUTINE timing_overview(list,main_name)
     use LIVE_t,     ONLY:LIVE_message
     use com,        ONLY:msg
     IMPLICIT NONE
     TYPE(clock_list),       INTENT(in) :: list
     CHARACTER(*),           INTENT(in) :: main_name
     TYPE(clock)                        :: tmp_clock
     INTEGER                            :: i
     !
     IF ( .NOT. list%alloc ) CALL error('timing_overview:  list not allocated')
     !
     call msg('r','Routines: '//TRIM(list%name)//' (Only MASTER cpu here)')
     !
     IF ( list%nclock == 0 ) THEN
       if (ncpu>1) call LIVE_message(message="[TIMING]"//'No clock to display',lfmt="n")
       call msg('r','No clock to display')
       RETURN
     ENDIF
     !
     DO i=1,list%nclock 
       !
       tmp_clock = list%clock(i)
       CALL clock_write(tmp_clock,FORM="dhms")
       !
     ENDDO
     !
   END SUBROUTINE timing_overview
   !
END MODULE timing_m
