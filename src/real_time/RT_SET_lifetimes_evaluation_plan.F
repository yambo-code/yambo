!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_SET_lifetimes_evaluation_plan( )
 !
 ! Here I define the components of an integer array
 ! RT_GAMMA_T_status(NE_steps)
 !
 use pars,           ONLY:IP2
 use fields,         ONLY:Efield
 use memory_m,       ONLY:mem_est
 use real_time,      ONLY:RT_GAMMA_T_status,NE_steps,RT_dynamics_steps,Life_MEM_steps,&
&                         RT_step,it_start
 !
 implicit none
 !
 ! Work Space
 !
 integer :: it,it_relative,N_inter_steps,N_intra_steps,i_group,itp,Nt_each_group
 !
 allocate(RT_GAMMA_T_status(NE_steps))
 call mem_est("RT_GAMMA_T_status",(/NE_steps/),(/IP2/))
 RT_GAMMA_T_status=0 ! Interpolate
 !
 if (RT_dynamics_steps(1)<0.) then
   RT_GAMMA_T_status=-1
   return
 endif 
 !
 N_inter_steps=RT_dynamics_steps(1)/RT_step
 N_intra_steps=RT_dynamics_steps(2)/RT_step
 !
 Nt_each_group=Life_MEM_steps*N_intra_steps+N_inter_steps
 !
 ! RT_GAMMA_T_status = -1   EVAL no SAVE
 !                      I>0 EVAL and SAVE
 !                      0   INTERPOLATE  
 !
 ! Note that the "plan" starts from it_start to allow easily
 ! a restart of the simulation.
 !
 do itp=it_start,NE_steps
   !
   it=itp-it_start+1 
   !
   it_relative   = mod(it-1,Nt_each_group)+1
   !
   i_group = (it-1)/(Life_MEM_steps*N_intra_steps+N_inter_steps)+1
   !
   if (i_group==1) RT_GAMMA_T_status(it)=-1
   if (i_group==2.and.it_relative< Nt_each_group-Life_MEM_steps+1) RT_GAMMA_T_status(it)=-1
   if (i_group==2.and.it_relative==Nt_each_group-Life_MEM_steps+1) call BUILD_up_group(Life_MEM_steps,1)
   if (i_group>=3.and.it_relative==1) call BUILD_up_group(N_inter_steps,N_intra_steps)
   !
 enddo
 !
 ! Fix the last points in order to have enough points for FIT or evaluate it directly
 !
 if (RT_GAMMA_T_status(NE_steps)==0) return
 !
 do it=NE_steps,1,-1
   if ( RT_GAMMA_T_status(it) == 0 ) exit
   RT_GAMMA_T_status(it)=-1
 enddo
 !
 contains
   !
   subroutine BUILD_up_group(Group_SIZE,INTRA_steps)
     !
     integer :: Group_SIZE,it_,INTRA_steps
     !
     do it_=1,Group_SIZE
       !
       RT_GAMMA_T_status(it_+it-1)=0
       !
       if (INTRA_steps==1) then
         if (it_<=Life_MEM_steps) RT_GAMMA_T_status(it_+it-1)=it_
       else
         if (it_<INTRA_steps*Life_MEM_steps) then
           if (mod(it_-1,INTRA_steps)==0) then
             RT_GAMMA_T_status(it_+it-1)=it_/INTRA_steps+1
           endif
         endif
       endif
       !
     enddo
     !
   end subroutine
   !
end subroutine RT_SET_lifetimes_evaluation_plan
