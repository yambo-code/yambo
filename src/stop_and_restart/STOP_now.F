!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
logical function STOP_now(DO_IT_NOW)
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:myid,mpi_comm_world
 use LIVE_t,         ONLY:cput_tot,USER_wall_time,USER_wall_time_string
 use parser_m,       ONLY:parser
 use it_tools,       ONLY:it_reset
 !
 implicit none
 !
 logical :: DO_IT_NOW
 ! 
 ! Work Space
 integer           ::ierr,days,hours,minutes
 real(SP)          ::CPU_t
 !
 CPU_t=abs( real(cput_tot(myid+1,1),SP) )
 !
 days=int(CPU_t/86400.d0)
 CPU_t=CPU_t-real(days*86400.d0,SP)
 hours=int(CPU_t/3600.d0)
 CPU_t=CPU_t-real(hours*3600.d0,SP)
 minutes=int(CPU_t/60.d0)
 !
 call it_reset(0)
 call parser('STOP_NOW',STOP_now) 
 !
 if (len_trim(USER_wall_time_string)>0.and.&
&    (days>=USER_wall_time(1).or.USER_wall_time(1)==0).and.&
&    (hours>=USER_wall_time(2).or.USER_wall_time(2)==0).and.&
&    (minutes>=USER_wall_time(3).or.USER_wall_time(3)==0)) then
   STOP_now=.TRUE.
 endif
 !
 if (.not.(DO_IT_NOW.or.STOP_now)) return
 !
 if (STOP_now ) call warning('USER STOP signal')
 if (DO_IT_NOW) call warning('SYSTEM STOP signal')
 !
 ! Close
 !
 call CLOSE_the_run(.TRUE.,.TRUE.,.TRUE.)
 !
#if defined _MPI
 call MPI_Abort(mpi_comm_world,1,ierr)
#endif
 stop
 !
end function
