!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
!
! This file is distributed under the terms of the GNU
! General Public License. You can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will
! be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE.  See the GNU General Public License
! for more details.
!
! You should have received a copy of the GNU General Public
! License along with this program; if not, write to the Free
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
logical function stop_now(DO_IT_NOW)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use electrons,      ONLY:levels,E_reset
 use frequency,      ONLY:w_samp,W_reset
 use X_m,            ONLY:X_t,X_reset
 use it_m,           ONLY:it_reset
 use parallel_m,     ONLY:myid,mpi_comm_world,ncpu
 use LIVE_t,         ONLY:cput_tot,USER_wall_time,USER_wall_time_string
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 logical :: DO_IT_NOW
 ! 
 ! Work Space
 !
 type(levels)      ::en
 type(bz_samp)     ::q,k
 type(X_t)         ::X
 type(w_samp)      ::Xw
 integer           ::ierr,days,hours,minutes
 real(SP)          ::CPU_t
 integer, external ::init
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
 call parser('STOP_NOW',stop_now) 
 !
 if (len_trim(USER_wall_time_string)>0.and.&
&    (days>=USER_wall_time(1).or.USER_wall_time(1)==0).and.&
&    (hours>=USER_wall_time(2).or.USER_wall_time(2)==0).and.&
&    (minutes>=USER_wall_time(3).or.USER_wall_time(3)==0)) then
   stop_now=.TRUE.
 endif
 !
 if (.not.stop_now) return
 if (.not.DO_IT_NOW) return
 !
 call E_reset(en)
 call bz_samp_reset(q)
 call bz_samp_reset(k)
 call W_reset(Xw)
 call X_reset(X,type=1)
 !
 call section('X','USER STOP signal')
 !
 ! Update init defs
 !
 ierr=init(en,q,k,X,Xw," "," ",.false.,.true.)
 !
 ! Finalize
 !
 call it_reset(-1)
 !
 
#if defined _MPI
 if (ncpu>1) call MPI_Abort(mpi_comm_world,ierr,1)
#endif
 stop
 !
end function
