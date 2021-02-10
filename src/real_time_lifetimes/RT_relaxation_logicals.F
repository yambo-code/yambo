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
subroutine RT_relaxation_logicals( )
 !
 ! Here I define the logicals the govermns the relaxation procedure
 !
 use RT_control,     ONLY:NE_i_start_time
 use RT_operations,  ONLY:FIRST_step
 use RT_lifetimes,   ONLY:l_RT_eval_lifetimes,l_RT_relaxation_update_reference,Life_MEM_steps,&
&                         l_initial_equilibrium,l_RT_constant_lifetimes,l_RT_extrapolate_lifetimes
 use real_time,      ONLY:RT_TIME_status,NE_i_time,RT_is_dynamically_dephased
 !
 implicit none
 !
 if (.not.RT_is_dynamically_dephased) return
 !
 if (l_initial_equilibrium) then
   l_RT_eval_lifetimes=.TRUE.
   l_RT_constant_lifetimes=.FALSE.
   l_RT_extrapolate_lifetimes=.FALSE.
   l_RT_relaxation_update_reference=.FALSE.
   return
 endif
 !
 ! RT_TIME_status = -1   KEEP constant 
 !                  >0   EVAL and SAVE (= to the time point)
 !                   0   EXTRAPOLATE  
 !
 ! Eval Lifetimes?
 !================
 l_RT_eval_lifetimes= RT_TIME_status(NE_i_time)>0.and.(FIRST_step( ).or.NE_i_time<=NE_i_start_time+Life_MEM_steps-1)
 !
 ! Keep the Lifetimes constant in the intermediate steps
 !=======================================================
 l_RT_constant_lifetimes= RT_TIME_status(NE_i_time)<0
 !
 ! Interpolate the Lifetimes
 !============================
 l_RT_extrapolate_lifetimes= RT_TIME_status(NE_i_time)==0 .or. (RT_TIME_status(NE_i_time)>0.and..not.FIRST_step( ))
 !
 ! Update the references
 !========================
 l_RT_relaxation_update_reference=RT_TIME_status(NE_i_time)> 1
 !
end subroutine RT_relaxation_logicals
