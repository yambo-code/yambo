!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_time_step_update( )
 !
 use pars,           ONLY:SP,cZERO
 use stderr,         ONLY:real2ch
 use units,          ONLY:AUT2FS
 use real_time,      ONLY:RT_bands,RT_nk,dG_lesser,MEM_pointer,i_MEM_old,l_Update_Time_Step,&
&                         i_MEM_prev,i_MEM_now,RT_step,RT_dyn_step,&
&                         RT_life_extrap_times,Life_MEM_steps,NE_time,NE_time_step_update_jump,&
&                         NE_step_update_treshold,NE_i_time,NE_time_step_update_last_point,RT_MAX_step,&
&                         NE_initial_time_step_update,RT_all2ibz
 use RT_control,     ONLY:CARR_RT_IO_t,OBS_RT_IO_t,OUTPUT_RT_IO_t,GF_RT_IO_t,&
&                         RT_do_it,SETUP_RT_IO_type,TIME_adjust,STEP_adjust,&
&                         CACHE_OBS_steps,RT_control_alloc,RT_control_free,J_cache,&
&                         CACHE_OBS_INTERVAL_time,CACHE_OBS_last_point
 use RT_output_m,    ONLY:TIME_STEP_desc
 use com,            ONLY:msg
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_Xk_ibz_INDEX,PAR_G_kpts,myid
 use interfaces,     ONLY:DESC_write
 !
 implicit none
 ! 
 ! Work Space 
 ! 
 integer           :: ib,ibp,ik,nT_updated,i_desc_ref
 complex(SP)       :: dG_dT_max(2),dG_dT(2),ERROR,dG(3)
 real(SP)          :: dT_now,dT_previous,dT_next,T_tmp(4)
 integer, external :: RT_Lifetimes_evaluation_plan
 !
 l_Update_Time_Step=NE_time>=NE_initial_time_step_update.and.NE_initial_time_step_update>0._SP
 !
 if (.not.l_Update_Time_Step) return
 !
 if (.not.RT_do_it('deltaT')) return
 !
 NE_time_step_update_last_point=NE_i_time
 !
 dG_dT_max   =cZERO
 dG          =cZERO
 !
 dT_now      =(MEM_pointer(i_MEM_now)-MEM_pointer(i_MEM_prev))*RT_step
 dT_previous =(MEM_pointer(i_MEM_prev)-MEM_pointer(i_MEM_old))*RT_step
 !
 do ik=PAR_G_kpts(1),PAR_G_kpts(2)
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
 if (all((/abs(ERROR)<NE_step_update_treshold/100,dT_next>RT_dyn_step,dT_next<RT_MAX_step/))) then
   !
   RT_dyn_step=dT_next
   !
   !call SETUP_RT_IO_type(OBS_RT_IO_t,.FALSE.)
   call SETUP_RT_IO_type(CARR_RT_IO_t,.FALSE.)
   call SETUP_RT_IO_type(OUTPUT_RT_IO_t,.FALSE.)
   GF_RT_IO_t%INTERVAL_time=TIME_adjust(GF_RT_IO_t%INTERVAL_time,CARR_RT_IO_t%INTERVAL_time)
   call SETUP_RT_IO_type(GF_RT_IO_t,.FALSE.)
   !
   CARR_RT_IO_t%last_point   = NE_i_time
   OUTPUT_RT_IO_t%last_point = NE_i_time
   GF_RT_IO_t%last_point     = NE_i_time
   !
   ! ... and the dT update steps
   !
   NE_time_step_update_jump = max(TIME_adjust(NE_time_step_update_jump,CARR_RT_IO_t%INTERVAL_time),CARR_RT_IO_t%INTERVAL_time)
   !
   ! J, P and M (spin) cache Time size
   !=========================
   !
   CACHE_OBS_last_point    = NE_i_time
   CACHE_OBS_INTERVAL_time = max(TIME_adjust(CACHE_OBS_INTERVAL_time,RT_dyn_step),RT_dyn_step)
   !
   OBS_RT_IO_t%INTERVAL_time=TIME_adjust(OBS_RT_IO_t%INTERVAL_time,NE_time_step_update_jump)
   CACHE_OBS_INTERVAL_time=STEP_adjust(OBS_RT_IO_t%INTERVAL_time,CACHE_OBS_INTERVAL_time,RT_dyn_step)
   !
   call SETUP_RT_IO_type(OBS_RT_IO_t,.FALSE.)
   !
   CACHE_OBS_steps=OBS_RT_IO_t%INTERVAL_steps/nint(CACHE_OBS_INTERVAL_time/RT_dyn_step)/nint(RT_dyn_step/RT_step)
   call RT_control_free("cache")
   call RT_control_alloc("cache")
   !
   ! I need to re-calculate the lifetimes evaluation plan in order to be consistent with the new Time Step
   !
   RT_life_extrap_times(2) = max(TIME_adjust(RT_life_extrap_times(2),RT_dyn_step),RT_dyn_step)
   RT_life_extrap_times(1) = RT_life_extrap_times(2)*Life_MEM_steps
   !
   ! ... and Lifetimes evaluation plan update
   !
   nT_updated =RT_Lifetimes_evaluation_plan( )
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
