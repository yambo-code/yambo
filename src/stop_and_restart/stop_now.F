!
!        Copyright (C) 2000-2015 the YAMBO team
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
 use com,            ONLY:repfile,file_exists
 use frequency,      ONLY:w_samp,W_reset
 use X_m,            ONLY:X_t,X_reset
 use it_m,           ONLY:it_reset
 use parallel_m,     ONLY:MPI_close,myid,ncpu,PP_redux_wait
 use LIVE_t,         ONLY:cput_tot,USER_wall_time,USER_wall_time_string
 use parser_m,       ONLY:parser
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
 integer           ::ierr,days,hours,minutes,i_stop_now(ncpu)
 real(SP)          ::CPU_t
 integer, external ::init
 !
 CPU_t=abs( cput_tot(myid+1,1) )
 !
 days=int(CPU_t/86400.d0)
 CPU_t=CPU_t-days*86400.d0
 hours=int(CPU_t/3600.d0)
 CPU_t=CPU_t-hours*3600.d0
 minutes=int(CPU_t/60.d0)
 !
 call it_reset(0)
 call parser('STOP_NOW',stop_now) 
 !
 i_stop_now=0
 if (stop_now) i_stop_now(myid+1)=1
 ! 
 if (len_trim(USER_wall_time_string)>0.and.&
&    (days>=USER_wall_time(1).or.USER_wall_time(1)==0).and.&
&    (hours>=USER_wall_time(2).or.USER_wall_time(2)==0).and.&
&    (minutes>=USER_wall_time(3).or.USER_wall_time(3)==0)) then
   i_stop_now(myid+1)=1
 endif
 !
 call PP_redux_wait(i_stop_now)
 !
 stop_now=any(i_stop_now==1)
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
 call MPI_close
 !
 stop
 !
end function
