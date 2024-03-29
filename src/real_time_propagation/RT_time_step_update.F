!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM 
!
subroutine RT_time_step_update( E )
 !
 use pars,           ONLY:SP,cZERO
 use stderr,         ONLY:real2ch
 use units,          ONLY:AUT2FS
 use RT_lifetimes,   ONLY:RT_life_extrap_times,Life_MEM_steps
 use real_time,      ONLY:RT_bands,dG_lesser,G_lesser,MEM_pointer,i_MEM_old,l_Update_Time_Step,&
&                         i_MEM_prev,i_MEM_now,RT_step,RT_dyn_step,RT_step_manual_prefactor,&
&                         NE_time,NE_time_step_update_manual,&
&                         NE_step_update_treshold,NE_i_time,NE_time_step_update_last_point,RT_MAX_step,&
&                         NE_initial_time_step_update,RT_nk,RTibz
 use RT_control,     ONLY:TIME_adjust
 use RT_output_m,    ONLY:TIME_STEP_desc
 use com,            ONLY:msg
 use electrons,      ONLY:levels
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX,PAR_IND_Xk_ibz
 use parallel_int,   ONLY:PP_redux_wait
 use interfaces,     ONLY:DESC_write
 use IO_int,         ONLY:IO_and_Messaging_switch
 !
 implicit none
 !
 type(levels)           :: E
 ! 
 ! Work Space 
 ! 
 logical           :: l_Update_Time_Step_Manual
 integer           :: ib,ibp,ik,nT_updated,i_desc_ref
 complex(SP)       :: dG_dT_max(2),dG_dT(2),ERROR,dG(3)
 real(SP)          :: dT_now,dT_previous,dT_next
#if defined _RT_SCATT
 integer, external :: RT_Lifetimes_evaluation_plan
#endif
 logical, external :: RT_do_it
 !
 l_Update_Time_Step=NE_time>=NE_initial_time_step_update.and.NE_initial_time_step_update>0._SP
 !
 l_Update_Time_Step_Manual=abs(NE_time-NE_time_step_update_manual)<RT_dyn_step/2._SP .and. NE_time>0._SP
 !
 if (.not.(l_Update_Time_Step.or.l_Update_Time_Step_Manual)) return
 !
 if ((.not.(RT_do_it('deltaT')).and.(.not.l_Update_Time_Step_Manual))) return
 !
 NE_time_step_update_last_point=NE_i_time
 !
 dT_now      =(MEM_pointer(i_MEM_now)-MEM_pointer(i_MEM_prev))*RT_step
 !
 if(l_update_time_step_manual) then
   !
   do ik=1,RT_nk
     !
     if( .not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
     do ib=RT_bands(1),E%nbf(1)
       do ibp=E%nbf(1)+1,RT_bands(2)
         dG_lesser(ib,ibp,ik,:)=cZERO
         dG_lesser(ibp,ib,ik,:)=cZERO
          G_lesser(ib,ibp,ik,:)=cZERO
          G_lesser(ibp,ib,ik,:)=cZERO
       enddo
     enddo
     !
   enddo
   !
   dT_next=dT_now*RT_step_manual_prefactor
   !
 else
   !
   dT_previous =(MEM_pointer(i_MEM_prev)-MEM_pointer(i_MEM_old))*RT_step
   !
   dG_dT_max   =cZERO
   dG          =cZERO
   !
   do ik=1,RT_nk
     !
     if( .not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
     !
     do ib=RT_bands(1),RT_bands(2)
       do ibp=RT_bands(1),RT_bands(2)
         dG_dT(1)=(dG_lesser(ib,ibp,ik,i_MEM_prev)-dG_lesser(ib,ibp,ik,i_MEM_old))/dT_previous
         dG_dT(2)=(dG_lesser(ib,ibp,ik,i_MEM_now)-dG_lesser(ib,ibp,ik,i_MEM_prev))/dT_now
         if ( abs(dG_dT(2)) >  abs(dG_dT_max(2)) ) then
           dG(1)=dG_lesser(ib,ibp,ik,i_MEM_old)
           dG(2)=dG_lesser(ib,ibp,ik,i_MEM_prev)
           dG(3)=dG_lesser(ib,ibp,ik,i_MEM_now)
           dG_dT_max=dG_dT
         endif
       enddo
     enddo
   enddo
   !
   call PP_redux_wait(dG,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   call PP_redux_wait(dG_dT_max,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   !
   ERROR=(dG(1)+dG_dT_max(1)*(dT_previous+dT_now)-dG(3))/dG(3)
   !
   dT_next=dT_previous*2._SP
   !
 endif
 !
 if (all((/abs(ERROR)<NE_step_update_treshold/100,dT_next>RT_dyn_step,dT_next<RT_MAX_step/)).or.l_Update_Time_Step_Manual) then
   !
   call IO_and_Messaging_switch("-fragments_IO_log")
   call RT_databases_IO(E,.TRUE.)
   call IO_and_Messaging_switch("+fragments_IO_log")
   !
   RT_dyn_step=dT_next
   !
   call RT_IO_type_time_steps("UPDATE")
   !
   ! I need to re-calculate the lifetimes evaluation plan in order to be consistent with the new Time Step
   !
   RT_life_extrap_times(2) = TIME_adjust(RT_life_extrap_times(2),RT_dyn_step)
   RT_life_extrap_times(1) = RT_life_extrap_times(2)*Life_MEM_steps
   !
   ! ... and Lifetimes evaluation plan update
   !
#if defined _RT_SCATT
   nT_updated =RT_Lifetimes_evaluation_plan( )
#else
   nT_updated =0
#endif
   !
   ! ... create the linked descriptions
   !
   i_desc_ref=TIME_STEP_desc%n+1
   !
   call RT_update_TIME_CONF_descriptions( )
   !
   call msg("nr","Dynamics steps update @ TIME "//trim(real2ch(NE_time*AUT2FS))//" fs")
   !
   call DESC_write("r","  ",TIME_STEP_desc,(/i_desc_ref,TIME_STEP_desc%n/))
   !
   call msg("r"," ")
   !
 endif
 !
end subroutine RT_time_step_update
