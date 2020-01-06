!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elph_scatt
 use electrons,      ONLY:n_spin,levels
 use RT_control,     ONLY:RT_do_it,ID,RT_carriers_object,J_cache,P_cache,Ms_cache,Ml_cache,A_cache,OBS_RT_IO_t,&
&                         GF_RT_IO_t,CARR_RT_IO_t,TMP_OBS_cache_steps,IO_OBS_cache_steps,SAVE_G_history,NE_i_start_time
 use real_time,      ONLY:NE_i_time,NE_steps,l_NE_dynamics,RT_nk,RT_ind_J,RT_P,RT_spin_magn,RT_orbt_magn,&
&                         l_NE_with_fields,RT_step,RT_dyn_step
 use IO_m,           ONLY:io_control,OP_WR,WR_CL,WR_CL_IF_END,NONE,WR,OP_WR_CL, &
&                         io_GF,io_CARRIERs,io_OBSERVABLES,IO_and_Messaging_switch,&
&                         OP_IF_START_APP_CL_IF_END,OP_APP,OP_APP_CL
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,HEAD_k_cpu,ncpu,myid
 use real_time,      ONLY:THETA_matrix,l_RT_CCA_Kernel,RT_carriers,NE_time,RT_all2ibz,G_MEM_steps
 use timing_m,       ONLY:timing
 use fields,         ONLY:A_tot
#if defined _PAR_IO
 use parallel_m,     ONLY:PAR_IO_G_kpts
#endif
 !
 implicit none
 !
 type(levels)      :: E
 !
 ! Work Space
 !
 integer           :: i_mem,ik,io_G,io_OBS,io_carr,io_THETA,i_time,i_cpu,secs(2),MODE
 integer, external :: io_RT_components
 logical, external :: stop_NOW
 !
 call timing('RT databases IO',OPR='start')
 !
 ! Observables (J, P, M ...)
 !===========================
 if (io_OBSERVABLES) then
   !
   if(NE_i_time==NE_i_start_time) then
     call io_control(ACTION=OP_APP,COM=NONE,SEC=(/1/),ID=ID(4)) 
     io_OBS=io_RT_components('OBSERVABLES',ID(4))
   endif
   !
   if (NE_i_time>=OBS_RT_IO_t%last_point) then
     !
     ! Cache the current and the polarization for future writing
     !
     i_time=NE_i_time
     if(OBS_RT_IO_t%last_point>0) i_time=i_time-OBS_RT_IO_t%last_point
     i_time=i_time/int(RT_dyn_step/RT_step)
     !
     if(i_time>0) then
       OBS_RT_IO_t%N=OBS_RT_IO_t%N+1
       OBS_RT_IO_t%Time(OBS_RT_IO_t%N)=NE_time
     else
       i_time=1
     endif
     !
     J_cache(1:3,i_time)=RT_ind_J
     P_cache(1:3,i_time)=RT_P
     if(n_spin>1) then
       Ms_cache(1:3,1:3,i_time)= RT_spin_magn
       Ml_cache(1:3,1:2,i_time)= RT_orbt_magn
     endif
     if(l_NE_with_fields) then
       A_cache(1:3,1,i_time)=A_tot%vecpot
       A_cache(1:3,2,i_time)=A_tot%vecpot_vel
       A_cache(1:3,3,i_time)=A_tot%vecpot_acc
     endif
     !
     if (RT_do_it('OBS').or.(NE_i_time>=NE_steps-int(RT_dyn_step/RT_step)+1)&
     &   .or.STOP_now(.FALSE.).or.NE_i_time==OBS_RT_IO_t%last_point) then
       !
       TMP_OBS_cache_steps=i_time
       !
       call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/2,3/),ID=ID(4))
       io_OBS=io_RT_components('OBSERVABLES',ID(4))
       !
       J_cache=0._SP
       !
     endif
     !
   endif
   !
 endif
 !
 ! GPL_EXCLUDE_START
 !
 ! Carriers 
 !=========
 if (io_CARRIERs) then
   !
   if(NE_i_time==NE_i_start_time) then
     call io_control(ACTION=OP_APP,COM=NONE,SEC=(/1/),ID=ID(6)) 
     io_carr=io_RT_components('carriers',ID(6))
   endif
   !
   if(RT_do_it('CARR').and.NE_i_time>=CARR_RT_IO_t%last_point) then
     !
     if (NE_i_time>CARR_RT_IO_t%last_point) then
       CARR_RT_IO_t%N=CARR_RT_IO_t%N+1
       CARR_RT_IO_t%Time(CARR_RT_IO_t%N)=NE_time
     endif
     !
     call RT_carriers_object(RT_carriers,E,WHAT='fill')
     !
     call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/2,3/),ID=ID(6))
     io_carr=io_RT_components('carriers',ID(6))
     !
   endif
   !
 endif
 !
 ! G_lesser and THETA
 !====================
 if (io_GF) then
   !
   if(NE_i_time==NE_i_start_time) then
     if(.not.SAVE_G_history) MODE=OP_WR_CL
     if(     SAVE_G_history) MODE=OP_APP_CL
     call io_control(ACTION=MODE,COM=NONE, SEC=(/1/),ID=ID(5))
     io_G=io_RT_components('G',ID(5))
     !
     if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
       call io_control(ACTION=OP_APP,COM=NONE, SEC=(/1/),ID=ID(7))
       io_THETA=io_RT_components('THETA',ID(7))
     endif
     !
   endif
   !
   !
   if(RT_do_it('G')) then
     !
     if (NE_i_time==1) then
       GF_RT_IO_t%N=1
       GF_RT_IO_t%Time(GF_RT_IO_t%N)=NE_time
     else
       if(.not.SAVE_G_history) GF_RT_IO_t%N=G_MEM_steps
       if(     SAVE_G_history) GF_RT_IO_t%N=GF_RT_IO_t%N+G_MEM_steps
       do i_mem=1-G_MEM_steps,0
         i_time=GF_RT_IO_t%N+i_mem
         GF_RT_IO_t%Time(i_time)=NE_time+i_mem*RT_dyn_step
       enddo
     endif
     !
     call IO_and_Messaging_switch("SAVE")
     call IO_and_Messaging_switch("+io_out",CONDITION=HEAD_k_cpu)
     !
#if defined _PAR_IO
     !
     secs=0
     if(myid==0) secs(1)=2
     if(PAR_IO_G_kpts(1)>0) secs(2)=3
     if(any(secs>0)) then
       call io_control(ACTION=OP_IF_START_APP_CL_IF_END,COM=NONE,SEC=secs,ID=ID(5))
       io_G=io_RT_components('G',ID(5))
     endif
     !
#else
     !
     do i_cpu=1,ncpu
       !
       if (myid+1/=i_cpu) cycle
       !
       do ik = 1, RT_nk
         !
         if (.not.PAR_IND_Xk_ibz%element_1D(RT_all2ibz(ik)) ) cycle
         !
         call io_control(ACTION=OP_IF_START_APP_CL_IF_END,COM=NONE,SEC=(/2,ik+2/),ID=ID(5))
         io_G=io_RT_components('G',ID(5))
         !
       enddo
       !
       call PP_redux_wait( )
       !
     enddo 
#endif
     !
     call IO_and_Messaging_switch("RESTORE")
     !
#if defined _ELPH
     if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
       !
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
         call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(7))
         io_THETA=io_RT_components('THETA',ID(7))
         !
         call RT_ELPH_zeroing()
         !
       enddo
       !
     endif
     !
#endif
   !
   endif
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 call timing('RT databases IO',OPR='stop')
 !
end subroutine RT_databases_IO
