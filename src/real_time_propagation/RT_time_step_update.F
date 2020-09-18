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
subroutine RT_time_step_update( E )
 !
 use pars,           ONLY:SP,cZERO
 use stderr,         ONLY:real2ch
 use units,          ONLY:AUT2FS
 use real_time,      ONLY:RT_bands,dG_lesser,MEM_pointer,i_MEM_old,l_Update_Time_Step,&
&                         i_MEM_prev,i_MEM_now,RT_step,RT_dyn_step,&
&                         RT_life_extrap_times,Life_MEM_steps,NE_time,&
&                         NE_step_update_treshold,NE_i_time,NE_time_step_update_last_point,RT_MAX_step,&
&                         NE_initial_time_step_update,RT_nk,RTibz
 use RT_control,     ONLY:TIME_adjust
 use RT_output_m,    ONLY:TIME_STEP_desc
 use com,            ONLY:msg
 use electrons,      ONLY:levels
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_Xk_ibz_INDEX,PAR_IND_Xk_ibz
 use interfaces,     ONLY:DESC_write
 use IO_m,           ONLY:IO_and_Messaging_switch
 !
 implicit none
 !
 type(levels)           :: E
 ! 
 ! Work Space 
 ! 
 integer           :: ib,ibp,ik,nT_updated,i_desc_ref
 complex(SP)       :: dG_dT_max(2),dG_dT(2),ERROR,dG(3)
 real(SP)          :: dT_now,dT_previous,dT_next
 integer, external :: RT_Lifetimes_evaluation_plan
 logical, external :: RT_do_it
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
 if (all((/abs(ERROR)<NE_step_update_treshold/100,dT_next>RT_dyn_step,dT_next<RT_MAX_step/))) then
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
