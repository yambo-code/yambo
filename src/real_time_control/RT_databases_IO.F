!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine RT_databases_IO(E)
 !
 use drivers,        ONLY:l_elph_scatt
 use electrons,      ONLY:n_spin,levels
 use RT_control,     ONLY:RT_do_it,ID,RT_carriers_object,J_cache,P_cache,Ms_cache,Ml_cache,OBS_RT_IO_t,&
&                         GF_RT_IO_t,IO_OBS_cache_steps
 use real_time,      ONLY:NE_i_time,l_NE_dynamics,RT_nk,NE_i_time,RT_ind_J,RT_P,RT_magn
 use IO_m,           ONLY:io_control,OP_WR,WR_CL,WR_CL_IF_END,NONE,WR,OP_WR_CL, &
&                         io_GF,io_CARRIERs,io_OBSERVABLES,IO_and_Messaging_switch,&
&                         OP_APP_CL_IF_END,WR_CL_IF_END
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,HEAD_k_cpu,ncpu,myid
 use real_time,      ONLY:THETA_matrix,l_RT_CCA_Kernel,RT_carriers,NE_time,RT_all2ibz
 use timing_m,       ONLY:timing
#if defined  _PAR_IO
 use com,            ONLY:error
#endif
 !
 implicit none
 !
 type(levels)      :: E
 !
 ! Work Space
 !
 integer           :: ik,io_G,io_OBS,io_carr,io_THETA,i_time,i_cpu,n_sec,secs(2)
 logical           :: l_start
 integer, external :: io_RT_components
 !
 call timing('RT databases IO',OPR='start')
 !
 ! Store the current and the polarization in the cache for future writing
 !
 if(l_NE_dynamics) then
   !
   i_time=mod(NE_i_time-1,IO_OBS_cache_steps)+1
   !
   J_cache(1:3,i_time)=RT_ind_J
   P_cache(1:3,i_time)=RT_P
   if(n_spin>1) then
     Ms_cache(1:3,i_time)= RT_magn(:,1) 
     Ml_cache(1:3,i_time)= RT_magn(:,2) 
   endif
   !
 endif
 !
 ! Observables (J, P, M ...)
 !===========================
 if (RT_do_it('OBS')) then
   !
   OBS_RT_IO_t%N=OBS_RT_IO_t%N+1
   OBS_RT_IO_t%Time(OBS_RT_IO_t%N)=NE_time
   OBS_RT_IO_t%last_point=NE_i_time
   !
   if (io_OBSERVABLES) then
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(4)) 
     io_OBS=io_RT_components('OBSERVABLES',ID(4))
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/NE_i_time+1/),ID=ID(4))
     io_OBS=io_RT_components('OBSERVABLES',ID(4))
   endif
   !
   !
 endif
 !
 !
 call timing('RT databases IO',OPR='stop')
 !
end subroutine RT_databases_IO
