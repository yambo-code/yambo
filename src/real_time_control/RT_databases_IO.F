!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_databases_IO(E,DO_IT)
 !
 use drivers,        ONLY:l_elph_scatt
 use electrons,      ONLY:n_spin,levels
 use RT_control,     ONLY:ID,RT_carriers_object,J_cache,P_cache,Ms_cache,Ml_cache,A_cache,OBS_RT_IO_t,&
&                         GF_RT_IO_t,CARR_RT_IO_t,CACHE_OBS_steps_now,CACHE_OBS_steps,    &
&                         SAVE_G_history,NE_i_start_time,CACHE_OBS_last_point
 use real_time,      ONLY:NE_i_time,NE_steps,RT_nk,RT_ind_J,RT_P,RT_spin_magn,RT_orbt_magn,&
&                         l_NE_with_fields,RT_step,RT_dyn_step,NE_time,THETA_matrix,&
&                         l_RT_CCA_Kernel,RT_carriers,G_MEM_steps
 use IO_m,           ONLY:io_control,manage_action,NONE,REP, &
&                         io_GF,io_CARRIERs,io_OBSERVABLES,IO_and_Messaging_switch,&
&                         OP_IF_START_APP_CL_IF_END,WR_CL_IF_END,OP_APP,OP_APP_CL,OP_WR_CL
 use parallel_m,     ONLY:PP_redux_wait,PP_bcast,HEAD_k_cpu,PAR_G_k_range
 use timing_m,       ONLY:timing
 use fields,         ONLY:A_tot
#if defined _PAR_IO
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX
#else
 use real_time,      ONLY:RT_k_FG
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,ncpu,myid
#endif
 !
 implicit none
 !
 type(levels)      :: E
 logical           :: DO_IT
 !
 ! Work Space
 !
 logical           :: write_OBS,write_G,write_CARR
 integer           :: i_mem,ik,G_IO_steps,io_G,io_OBS,io_carr,io_THETA,IO_ACT,i_time,i_cpu
 integer, external :: io_RT_components
 logical, external :: RT_do_it
 !
 call timing('RT databases IO',OPR='start')
 !
 ! Observables (J, P, M ...)
 !===========================
 !
 write_OBS =.false.
 write_CARR=.false.
 write_G   =.false.
 !
 if (io_OBSERVABLES) then
   !
   if(NE_i_time==NE_i_start_time) then
     call io_control(ACTION=OP_APP,COM=NONE,SEC=(/1/),ID=ID(4)) 
     io_OBS=io_RT_components('OBSERVABLES',ID(4))
     CACHE_OBS_last_point=OBS_RT_IO_t%last_point
   endif
   !
   write_OBS=RT_do_it('OBS').or.DO_IT
   !
   if (RT_do_it('OBS_cache').or.DO_IT) then
     !
     ! Cache the current and the polarization for future writing
     !
     CACHE_OBS_last_point=NE_i_time
     CACHE_OBS_steps_now =CACHE_OBS_steps_now+1
     !
     OBS_RT_IO_t%N=OBS_RT_IO_t%N+1
     OBS_RT_IO_t%Time(OBS_RT_IO_t%N)=NE_time
     !
     J_cache(1:3,CACHE_OBS_steps_now)=RT_ind_J
     P_cache(1:3,CACHE_OBS_steps_now)=RT_P
     if(n_spin>1) then
       Ms_cache(1:3,CACHE_OBS_steps_now)= RT_spin_magn
       Ml_cache(1:3,1:2,CACHE_OBS_steps_now)= RT_orbt_magn
     endif
     if(l_NE_with_fields) then
       A_cache(1:3,1,CACHE_OBS_steps_now)=A_tot%vecpot
       A_cache(1:3,2,CACHE_OBS_steps_now)=A_tot%vecpot_vel
       A_cache(1:3,3,CACHE_OBS_steps_now)=A_tot%vecpot_acc
     endif
     !
     if (write_OBS) then
       !
       IO_ACT=manage_action(WR_CL_IF_END,NE_i_time,NE_i_start_time,NE_steps)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2,3/),ID=ID(4))
       io_OBS=io_RT_components('OBSERVABLES',ID(4))
       !
       CACHE_OBS_steps_now=0
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
   write_CARR=RT_do_it('CARR').or.DO_IT
   !
   if (write_CARR) then
     !
     if (NE_i_time>CARR_RT_IO_t%last_point) then
       CARR_RT_IO_t%N=CARR_RT_IO_t%N+1
       CARR_RT_IO_t%Time(CARR_RT_IO_t%N)=NE_time
     endif
     !
     call RT_carriers_object(RT_carriers,E,WHAT='fill')
     !
     IO_ACT=manage_action(WR_CL_IF_END,NE_i_time,NE_i_start_time,NE_steps)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2,3/),ID=ID(6))
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
     if(.not.SAVE_G_history) IO_ACT=OP_WR_CL
     if(     SAVE_G_history) IO_ACT=OP_APP_CL
     call io_control(ACTION=IO_ACT,COM=REP, SEC=(/1/),ID=ID(5))
     io_G=io_RT_components('G',ID(5))
     !
     if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
       call io_control(ACTION=OP_APP_CL,COM=NONE, SEC=(/1/),ID=ID(7))
       io_THETA=io_RT_components('THETA',ID(7))
     endif
     !
   endif
   !
   write_G=RT_do_it('G').or.DO_IT
   !
   if(write_G) then
     !
     if (NE_i_time==1) then
       GF_RT_IO_t%N=1
       GF_RT_IO_t%Time(GF_RT_IO_t%N)=NE_time
     else
       G_IO_steps=GF_RT_IO_t%INTERVAL_steps/nint(RT_dyn_step/RT_step)
       if(.not.SAVE_G_history) GF_RT_IO_t%N=G_MEM_steps
       if(     SAVE_G_history) GF_RT_IO_t%N=GF_RT_IO_t%N+min(G_MEM_steps,G_IO_steps)
       do i_mem=1-G_MEM_steps,0
         i_time=GF_RT_IO_t%N+i_mem
         GF_RT_IO_t%Time(i_time)=NE_time+i_mem*RT_dyn_step
       enddo
     endif
     !
#if defined _PAR_IO
     IO_ACT=manage_action(OP_IF_START_APP_CL_IF_END,NE_i_time,NE_i_start_time,NE_steps)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2/),ID=ID(5))
#else
     call io_control(ACTION=OP_APP_CL,COM=NONE,SEC=(/2/),ID=ID(5))
#endif
     io_G=io_RT_components('G',ID(5))
     !
     call IO_and_Messaging_switch("SAVE")
     call IO_and_Messaging_switch("+io_out",CONDITION=HEAD_k_cpu)
     !
#if defined _PAR_IO
     !
     if(PAR_G_k_range(1)>0.and.HEAD_k_CPU) then
       IO_ACT=manage_action(OP_IF_START_APP_CL_IF_END,NE_i_time,NE_i_start_time,NE_steps)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/3/),ID=ID(5),COMM=PAR_COM_Xk_ibz_INDEX%COMM)
       io_G=io_RT_components('G',ID(5))
     endif
     !
#else
     !
     do i_cpu=1,ncpu
       !
       do ik = 1, RT_nk
         !
         if (myid+1/=i_cpu) cycle
         !
         if (.not.PAR_IND_Xk_ibz%element_1D(RT_k_FG%k_map(ik)) ) cycle
         !
         IO_ACT=manage_action(OP_IF_START_APP_CL_IF_END,ik,FIRST=PAR_G_k_range(1),LAST=PAR_G_k_range(2))
         call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/ik+2/),ID=ID(5))
         io_G=io_RT_components('G',ID(5))
         !
       enddo 
       !
       call PP_redux_wait( )
       !
     enddo
     !
#endif
     !
     call IO_and_Messaging_switch("RESTORE")
     !
#if defined _ELPH
     !
     if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
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
         IO_ACT=manage_action(OP_IF_START_APP_CL_IF_END,ik,1,RT_nk)
         call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/ik+2/),ID=ID(7))
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
 if (write_OBS)  call PP_bcast( OBS_RT_IO_t%last_point,0)
 if (write_CARR) call PP_bcast(CARR_RT_IO_t%last_point,0)
 if (write_G)    call PP_bcast(  GF_RT_IO_t%last_point,0)
 !
 ! GPL_EXCLUDE_END
 !
 call timing('RT databases IO',OPR='stop')
 !
end subroutine RT_databases_IO
