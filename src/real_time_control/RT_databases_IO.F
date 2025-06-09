!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!                  
subroutine RT_databases_IO(E,DO_IT)
 !
 use electrons,      ONLY:n_spin,levels
 use RT_control,     ONLY:RT_carriers_object,J_cache,P_cache,Ms_cache,Ml_cache,A_cache,OBS_RT_IO_t,Vbands_RT_IO_t,&
&                         Gless_RESTART_RT_IO_t,CARR_RT_IO_t,CACHE_OBS_steps_now,CACHE_OBS_steps,    &
&                         SAVE_G_history,NE_i_start_time,CACHE_OBS_last_point,Gless_RT_IO_t,RT_DB_ID,&
&                         RT_return_db_ID,RT_DB_name,SAVE_Vb_history
#if defined _ELPH_ITERATIVE
 use RT_lifetimes,   ONLY:l_RT_iterative_ELPH_lifetimes
#endif
 use real_time,      ONLY:NE_i_time,NE_steps,RT_nk,RT_ind_J,RT_P,RT_spin_magn,RT_orbt_magn,&
&                         l_NE_with_fields,RT_step,RT_dyn_step,NE_time,RT_carriers,G_MEM_steps
 use IO_int,         ONLY:io_control,IO_and_Messaging_switch
 use IO_m,           ONLY:manage_action,NONE,io_GF,io_CARRIERs,io_OBSERVABLES,io_V_bands,OP_APP_CL,OP_WR_CL,&
&                         OP_IF_START_APP_CL_IF_END,WR_CL_IF_END,OP_APP,WR_CL,WR,OP_APP,OP_WR
 use parallel_m,     ONLY:PAR_G_k_range,myid,HEAD_k_cpu
 use parallel_int,   ONLY:PP_wait,PP_redux_wait,PP_bcast
 use timing_m,       ONLY:timing
 use fields,         ONLY:A_tot
#if defined _PAR_IO
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX,master_cpu
#else
 use real_time,      ONLY:RTibz
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,ncpu,myid
#endif
#if defined _ELPH_ITERATIVE
 use RT_iterative,   ONLY:THETA_matrix
#endif
 !
 implicit none
 !
 type(levels)      :: E
 logical           :: DO_IT
 !
 ! Work Space
 !
 integer           :: i_mem,io_OBS,io_carr,IO_ACT,i_time,i_db
 integer, external :: io_RT_components
 logical, external :: RT_do_it
 !
 ! ... G_lesser
 !
 integer           :: ik,G_IO_steps,io_G,io_Vb,i_cpu,i_G_db,i_G_k_db,i_G_RES_db,i_G_RES_k_db,idb_Vb,idb_Vb_k
#if defined _ELPH_ITERATIVE
 integer           :: io_THETA
#endif
 !
 call timing('RT databases IO',OPR='start')
 !
 ! Observables (J, P, M ...)
 !===========================
 if (io_OBSERVABLES) then
   !
   i_db=RT_return_db_ID("OBSERVABLES")
   !
   if(NE_i_time==NE_i_start_time) then
     call io_control(ACTION=OP_APP,COM=NONE,SEC=(/1/),ID=RT_DB_ID(i_db)) 
     io_OBS=io_RT_components('OBSERVABLES',RT_DB_ID(i_db))
     CACHE_OBS_last_point=OBS_RT_IO_t%last_point
     if (NE_i_time>1) OBS_RT_IO_t%N=OBS_RT_IO_t%N-1
   endif
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
       Ms_cache(1:3,1:3,CACHE_OBS_steps_now)= RT_spin_magn
       Ml_cache(1:3,1:2,CACHE_OBS_steps_now)= RT_orbt_magn
     endif
     if(l_NE_with_fields) then
       A_cache(1:3,1,CACHE_OBS_steps_now)=A_tot%vecpot
       A_cache(1:3,2,CACHE_OBS_steps_now)=A_tot%vecpot_vel
       A_cache(1:3,3,CACHE_OBS_steps_now)=A_tot%vecpot_acc
     endif
     !
     if (RT_do_it('OBS').or.DO_IT) then
       !
       IO_ACT=manage_action(WR_CL_IF_END,NE_i_time,NE_i_start_time,NE_steps)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2,3/),ID=RT_DB_ID(i_db))
       io_OBS=io_RT_components('OBSERVABLES',RT_DB_ID(i_db))
       !
       CACHE_OBS_steps_now=0
       !
     endif
     !
   endif
   !
 endif
 !
 ! Carriers 
 !=========
 if (io_CARRIERs) then
   !
   i_db=RT_return_db_ID("carriers")
   !
   if (NE_i_time==NE_i_start_time) then
     call io_control(ACTION=OP_APP,COM=NONE,SEC=(/1/),ID=RT_DB_ID(i_db)) 
     io_carr=io_RT_components('carriers',RT_DB_ID(i_db))
     if (NE_i_time>1) CARR_RT_IO_t%N=CARR_RT_IO_t%N-1
   endif
   !
   if (RT_do_it('CARR').or.DO_IT) then
     !
     if (NE_i_time>CARR_RT_IO_t%last_point) then
       CARR_RT_IO_t%N=CARR_RT_IO_t%N+1
       CARR_RT_IO_t%Time(CARR_RT_IO_t%N)=NE_time
     endif
     !
     call RT_carriers_object(RT_carriers,E,WHAT='fill')
     !
     IO_ACT=manage_action(WR_CL_IF_END,NE_i_time,NE_i_start_time,NE_steps)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2,3/),ID=RT_DB_ID(i_db))
     io_carr=io_RT_components('carriers',RT_DB_ID(i_db))
     !
   endif
   !
 endif
 !
 ! G_lesser and THETA
 !====================
 if (io_GF) then
   !
   i_G_RES_db  =RT_return_db_ID("G_lesser_RESTART")
   i_G_db      =RT_return_db_ID("G_lesser")
   i_G_RES_k_db=RT_return_db_ID("G_lesser_RESTART_K_section")
   i_G_k_db    =RT_return_db_ID("G_lesser_K_section")
   !  
   if(NE_i_time==NE_i_start_time) then
     !
     ! Serial SECTION 1 
     !
     call elemental_G_lesser_IO(i_G_RES_db,OP_WR_CL,i_G_db,OP_APP_CL,SEC_=1,MODE="SER")
     !
#if defined _ELPH_ITERATIVE
     if (l_RT_iterative_ELPH_lifetimes) then
       call io_control(ACTION=OP_APP_CL,COM=NONE, SEC=(/1/),ID=RT_DB_ID(RT_return_db_ID("THETA")))
       io_THETA=io_RT_components('THETA',RT_DB_ID(RT_return_db_ID("THETA")))
     endif
#endif
     !
   endif
   !
   if (RT_do_it('G').or.DO_IT) then
     !
     if (NE_i_time==1) then
       Gless_RESTART_RT_IO_t%N=1
       Gless_RESTART_RT_IO_t%Time(1)=NE_time
       if (SAVE_G_history) then
         Gless_RT_IO_t%N=1
         Gless_RT_IO_t%Time(1)=NE_time
       endif
     else
       G_IO_steps=Gless_RESTART_RT_IO_t%INTERVAL_steps/nint(RT_dyn_step/RT_step)
       Gless_RESTART_RT_IO_t%N=G_MEM_steps
       do i_mem=1-G_MEM_steps,0
         i_time=Gless_RESTART_RT_IO_t%N+i_mem
         Gless_RESTART_RT_IO_t%Time(i_time)=NE_time+i_mem*RT_dyn_step
       enddo
       if (SAVE_G_history) then
         Gless_RT_IO_t%N=Gless_RT_IO_t%N+1
         Gless_RT_IO_t%Time(Gless_RT_IO_t%N)=NE_time
       endif
     endif
     !
     ! SERIAL SECTION 2 
     ! 
#if defined _PAR_IO
     IO_ACT=manage_action(OP_IF_START_APP_CL_IF_END,NE_i_time,NE_i_start_time,NE_steps)
#else
     IO_ACT=OP_APP_CL
#endif
     call elemental_G_lesser_IO(i_G_RES_db,IO_ACT,i_G_db,IO_ACT,SEC_=2,MODE="SER")
     !
     ! PARALLEL k-SECTION 3
     !
#if defined _PAR_IO
     IO_ACT=manage_action(OP_IF_START_APP_CL_IF_END,NE_i_time,NE_i_start_time,NE_steps)
     call elemental_G_lesser_IO(i_G_RES_k_db,IO_ACT,i_G_k_db,IO_ACT,SEC_=3,MODE="PAR")
#else
     !
     ! I/O parallel permissions
     !
     call IO_and_Messaging_switch("SAVE")
     call IO_and_Messaging_switch("+io_out",CONDITION=HEAD_k_cpu)
     !
     do i_cpu=1,ncpu
       !
       do ik = 1, RT_nk
         !
         if (myid+1/=i_cpu) cycle
         !
         if (.not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
         !
         IO_ACT=manage_action(OP_IF_START_APP_CL_IF_END,ik,FIRST=PAR_G_k_range(1),LAST=PAR_G_k_range(2))
         !
         call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/ik+2/),ID=RT_DB_ID(i_G_RES_k_db))
         io_G=io_RT_components('G_lesser_RESTART_K_section',RT_DB_ID(i_G_RES_k_db))
         !
         if (SAVE_G_history) call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/ik+2/),ID=RT_DB_ID(i_G_k_db))
         if (SAVE_G_history) io_G=io_RT_components('G_lesser_K_section',RT_DB_ID(i_G_k_db))
         !
       enddo 
       !
       call PP_wait( )
       !
     enddo
     !
     call IO_and_Messaging_switch("RESTORE")
     !
#endif
     !
#if defined _ELPH_ITERATIVE
     if (l_RT_iterative_ELPH_lifetimes) then
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
         call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/ik+2/),ID=RT_DB_ID(8))
         io_THETA=io_RT_components('THETA',RT_DB_ID(8))
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
 endif
 !
#if defined _NL
 !
 ! V_bands
 !====================
 if (io_V_bands) then
   !
#if !defined _PAR_IO
   call error(" Vbands restart not coded for serial I/O")
#endif
   !
   idb_Vb     =RT_return_db_ID("V_bands")
   idb_Vb_k   =RT_return_db_ID("V_bands_K_section")
   !
   ! Serial SECTION 1 
   !
   if(NE_i_time==NE_i_start_time) then
     if (NE_i_time==1) IO_ACT=OP_WR_CL
     if (NE_i_time> 1) IO_ACT=OP_APP_CL
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/1/),ID=RT_DB_ID(idb_Vb))
     io_Vb=io_RT_components("V_bands",RT_DB_ID(idb_Vb))
   endif
   !
   if (RT_do_it('Vb').or.DO_IT) then
     !
     if (.not.SAVE_Vb_history) Vbands_RT_IO_t%N=1
     if (     SAVE_Vb_history) Vbands_RT_IO_t%N=Vbands_RT_IO_t%N+1
     Vbands_RT_IO_t%Time(Vbands_RT_IO_t%N)=NE_time
     !
     IO_ACT=manage_action(OP_IF_START_APP_CL_IF_END,NE_i_time,NE_i_start_time,NE_steps)
     !
     ! SERIAL SECTION 2 
     !
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2/),ID=RT_DB_ID(idb_Vb))
     io_Vb=io_RT_components("V_bands",RT_DB_ID(idb_Vb))
     !
     ! (PARALLEL) k-SECTION 3
     !
     !call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/3/),ID=RT_DB_ID(idb_Vb_k),COMM=PAR_COM_Xk_ibz_INDEX,DO_IT=HEAD_k_CPU)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/3/),ID=RT_DB_ID(idb_Vb_k))
     io_Vb=io_RT_components("V_bands_K_section",RT_DB_ID(idb_Vb_k))
     !
   endif
   !
 endif
 !
#endif
 !
 if (RT_do_it('OBS').or.DO_IT ) call PP_bcast( OBS_RT_IO_t%last_point,0)
 if (RT_do_it('CARR').or.DO_IT) call PP_bcast(CARR_RT_IO_t%last_point,0)
 if (RT_do_it('Vb').or.DO_IT)   call PP_bcast(Vbands_RT_IO_t%last_point,0)
 if (RT_do_it('G').or.DO_IT)    call PP_bcast(Gless_RESTART_RT_IO_t%last_point,0)
 if (RT_do_it('G').or.DO_IT)    call PP_bcast(Gless_RT_IO_t%last_point,0)
 !
 call timing('RT databases IO',OPR='stop')
 !
 contains
   !
   subroutine elemental_G_lesser_IO(idb_G_RES,G_RES_ACT,idb_G,G_ACT,SEC_,MODE)
     use pars, ONLY:schlen
     integer      :: idb_G_RES,G_RES_ACT,idb_G,G_ACT,SEC_
     character(*) :: MODE
     character(schlen) :: G_RES_name,G_name
     !
     G_RES_name=RT_DB_name(idb_G_RES)
     G_name    =RT_DB_name(idb_G)
     !  
#if defined _PAR_IO
     if (MODE=="PAR") then
       call io_control(ACTION=G_RES_ACT,COM=NONE,SEC=(/SEC_/),ID=RT_DB_ID(idb_G_RES),COMM=PAR_COM_Xk_ibz_INDEX,DO_IT=HEAD_k_CPU)
       io_G=io_RT_components(trim(G_RES_name),RT_DB_ID(idb_G_RES))
       if (SAVE_G_history)  then
         call io_control(ACTION=G_ACT,COM=NONE,SEC=(/SEC_/),ID=RT_DB_ID(idb_G),COMM=PAR_COM_Xk_ibz_INDEX,DO_IT=HEAD_k_CPU)
         io_G=io_RT_components(trim(G_name),RT_DB_ID(idb_G))
       endif
     else
#endif
       !
       call io_control(ACTION=G_RES_ACT,COM=NONE,SEC=(/SEC_/),ID=RT_DB_ID(idb_G_RES))
       io_G=io_RT_components(trim(G_RES_name),RT_DB_ID(idb_G_RES))
       if (SAVE_G_history)  then
         call io_control(ACTION=G_ACT,COM=NONE,SEC=(/SEC_/),ID=RT_DB_ID(idb_G))
         io_G=io_RT_components(trim(G_name),RT_DB_ID(idb_G))
       endif
       !
#if defined _PAR_IO
     endif
#endif
   end subroutine
   !
end subroutine RT_databases_IO
