!
!        Copyright (C) 2000-2019 the YAMBO team
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
&                         OP_IF_START_APP_CL_IF_END,manage_action
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,HEAD_k_cpu,ncpu,myid
 use real_time,      ONLY:THETA_matrix,l_RT_CCA_Kernel,RT_carriers,NE_time,RT_all2ibz
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(levels)      :: E
 !
 ! Work Space
 !
 integer           :: ik,io_G,io_OBS,io_carr,io_THETA,IO_ACT,i_time,i_cpu,n_sec,secs(2)
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
   ! GPL_EXCLUDE_START
   !
   if (io_CARRIERs) then
     !
     call RT_carriers_object(RT_carriers,E,WHAT='fill')
     !
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(6)) 
     io_carr=io_RT_components('carriers',ID(6))
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/NE_i_time+1/),ID=ID(6))
     io_carr=io_RT_components('carriers',ID(6))
   endif
   !
   ! GPL_EXCLUDE_END
   !
 endif
 !
 ! GPL_EXCLUDE_START
 !
 ! G_lesser and THETA
 !====================
 if (io_GF.and.RT_do_it('G')) then
   !
   GF_RT_IO_t%N=GF_RT_IO_t%N+1
   GF_RT_IO_t%Time(GF_RT_IO_t%N)=NE_time
   GF_RT_IO_t%last_point=NE_i_time
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE, SEC=(/1/),ID=ID(5))
   io_G=io_RT_components('G',ID(5))
   !
   call IO_and_Messaging_switch("SAVE")
   call IO_and_Messaging_switch("+io_out",CONDITION=HEAD_k_cpu)
   !
#if defined _PAR_IO
   !
   n_sec=0
   do ik = 1, RT_nk
     if (.not.PAR_IND_Xk_ibz%element_1D(RT_all2ibz(ik)) ) cycle
     if (n_sec==0) secs=ik+1
     if (n_sec >0) then
       if (ik/=secs(2)) call error(" Non contiguous G distr does not work with PAR-IO")
       secs(2)=ik+1
     endif
     n_sec=n_sec+1
   enddo
   !
   if(n_sec>0) then
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=secs,ID=ID(5))
     io_G=io_RT_components('G',ID(5))
   endif
   !
#else
   !
   do ik = 1, RT_nk
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(RT_all2ibz(ik)) ) cycle
     !
     IO_ACT=manage_action(OP_IF_START_APP_CL_IF_END,ik,FIRST=1,LAST=RT_nk,PAR_index=PAR_IND_Xk_ibz)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/ik+1/),ID=ID(5))
     io_G=io_RT_components('G',ID(5))
     !
   enddo 
   !
   call PP_redux_wait( )
   !
#endif
   !
   call IO_and_Messaging_switch("RESTORE")
   !
#if defined _ELPH
   if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
     !
     call io_control(ACTION=OP_WR,COM=NONE, SEC=(/1/),ID=ID(7))
     io_THETA=io_RT_components('THETA',ID(7))
     !
     do ik = 1, RT_nk
       !
       ! The I/O of the THETA matrix is tricky when the run is parallel.
       ! The point is that every CPU evolves a certain group of q-points.
       ! If I do here a all-to-all each CPU will have non-zero values for 
       ! q-points that it will not evolve. At the next call of this routine
       ! therefore, values relative to different times will be wrongly merged.
       !
       call PP_redux_wait(THETA_matrix(:,:,:,ik))
       !
       IO_ACT=manage_action(WR_CL_IF_END,ik,FIRST=1,LAST=RT_nk)       
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/ik+1/),ID=ID(7))
       io_THETA=io_RT_components('THETA',ID(7))
       !
       call RT_ELPH_zeroing()
       !
     enddo
     !
   endif
#endif
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 call timing('RT databases IO',OPR='stop')
 !
end subroutine RT_databases_IO
