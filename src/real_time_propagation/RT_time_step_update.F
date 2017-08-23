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
subroutine RT_time_step_update( )
 !
 use pars,           ONLY:SP
 use stderr,         ONLY:real2ch
 use units,          ONLY:AUT2FS
 use real_time,      ONLY:RT_bands,RT_nk,dG_lesser,MEM_pointer,i_MEM_previous,&
&                         i_MEM_now,i_MEM_next,MAX_dG_dT_indexes,RT_step,RT_dyn_step,&
&                         RT_life_extrap_times,Life_MEM_steps,NE_time,NE_time_step_update_jump,&
&                         NE_step_update_treshold,NE_i_time,NE_time_step_update_last_point,RT_MAX_step
 use RT_control,     ONLY:JPC_RT_IO_t,OUTPUT_RT_IO_t,GF_RT_IO_t,n_TIME_STEP_CONF_descriptions,&
&                         TIME_STEP_CONF_description,RT_do_it,SETUP_RT_IO_type,TIME_adjust
 use com,            ONLY:msg
 implicit none
 ! 
 ! Work Space 
 ! 
 integer           :: ib,ibp,ik,nT_updated,i1,n_mesgs
 complex(SP)       :: dG_dT_max(2),dG_dT(2),ERROR,dG(3)
 real(SP)          :: dT_now,dT_previous,dT_next
 integer, external :: RT_Lifetimes_evaluation_plan
 !
 if (.not.RT_do_it('deltaT')) return
 !
 NE_time_step_update_last_point=NE_i_time
 !
 dG_dT_max   =0._SP
 dT_now      =(MEM_pointer(i_MEM_next)-MEM_pointer(i_MEM_now))*RT_step
 dT_previous =(MEM_pointer(i_MEM_now)-MEM_pointer(i_MEM_previous))*RT_step
 !
 do ik=1,RT_nk
   do ib=RT_bands(1),RT_bands(2)
     do ibp=RT_bands(1),RT_bands(2)
       dG_dT(1)=(dG_lesser(ib,ibp,ik,i_MEM_now)-dG_lesser(ib,ibp,ik,i_MEM_previous))/dT_previous
       dG_dT(2)=(dG_lesser(ib,ibp,ik,i_MEM_next)-dG_lesser(ib,ibp,ik,i_MEM_now))/dT_now
       if ( abs(dG_dT(2)) >  abs(dG_dT_max(2)) ) then
         dG(1)=dG_lesser(ib,ibp,ik,i_MEM_previous)
         dG(2)=dG_lesser(ib,ibp,ik,i_MEM_now)
         dG(3)=dG_lesser(ib,ibp,ik,i_MEM_next)
         dG_dT_max=dG_dT
         MAX_dG_dT_indexes=(/ib,ibp,ik/)
       endif
     enddo
   enddo
 enddo
 !
 ERROR=(dG(1)+dG_dT_max(1)*(dT_previous+dT_now)-dG(3))/dG(3)
 !
 dT_next=dT_previous*2._SP
 !
 if (all((/abs(ERROR)<NE_step_update_treshold/100,dT_next>RT_dyn_step,dT_next<RT_MAX_step/))) then
   !
   RT_dyn_step=dT_next
   !
   call SETUP_RT_IO_type(JPC_RT_IO_t,.FALSE.)
   call SETUP_RT_IO_type(OUTPUT_RT_IO_t,.FALSE.)
   call SETUP_RT_IO_type(GF_RT_IO_t,.FALSE.)
   !
   JPC_RT_IO_t%last_point    = NE_i_time
   OUTPUT_RT_IO_t%last_point = NE_i_time
   GF_RT_IO_t%last_point     = NE_i_time
   !
   ! ... and the dT update steps
   !
   NE_time_step_update_jump = max(TIME_adjust(NE_time_step_update_jump,JPC_RT_IO_t%INTERVAL_time),JPC_RT_IO_t%INTERVAL_time)
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
   n_mesgs=n_TIME_STEP_CONF_descriptions
   !
   call RT_update_TIME_CONF_descriptions( )
   !
   call msg("nr","Dynamics steps update @ TIME "//trim(real2ch(NE_time*AUT2FS))//" fs")
   do i1=n_mesgs+1,n_TIME_STEP_CONF_descriptions
     call msg("r",trim(TIME_STEP_CONF_description(i1)))
   enddo
   !
 endif
 !
end subroutine RT_time_step_update
