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
integer function RT_SET_lifetimes_evaluation_plan( )
 !
 ! Here I define the components of an integer array
 ! RT_GAMMA_T_status(NE_steps)
 !
 use units
 use pars,           ONLY:IP2,SP
 use fields,         ONLY:Efield,i_Pump,ext_field,Efield_duplicate,small_a,l_manual_excitation
 use memory_m,       ONLY:mem_est
 use real_time,      ONLY:RT_GAMMA_T_status,NE_steps,RT_dynamics_steps,Life_MEM_steps,&
&                         RT_step,RT_dynamics_KIND,RT_dynamics_decay_time,it_start
 use LIVE_t,         ONLY:live_timing
 !
 implicit none
 !
 ! Work Space
 !
 integer         :: it,it_relative,N_inter_steps,N_intra_steps,Nt_each_group,it_last,it_0,it_
 real(SP)        :: weight(NE_steps)
 complex(SP)     :: TIME
 type(ext_field) :: E_dummy
 !
 if (.not.allocated(RT_GAMMA_T_status)) then
   allocate(RT_GAMMA_T_status(NE_steps))
   call mem_est("RT_GAMMA_T_status",(/NE_steps/),(/IP2/))
 endif
 RT_GAMMA_T_status=0 ! Interpolate
 !
 if (RT_dynamics_steps(1)<0.) then
   RT_GAMMA_T_status=1
   return
 endif 
 !
 N_inter_steps=RT_dynamics_steps(1)/RT_step
 N_intra_steps=RT_dynamics_steps(2)/RT_step
 !
 ! RT_GAMMA_T_status = -1   KEEP constant 
 !                      1   EVAL and SAVE
 !                      0   INTERPOLATE  
 !
 RT_SET_lifetimes_evaluation_plan=0
 if (it_start==1) then
   it_last=1
   Nt_each_group=N_inter_steps
 else
   it_last=it_start
   N_inter_steps=real(RT_dynamics_steps(1))/weight(it_start)/RT_step
   N_intra_steps=real(RT_dynamics_steps(2))/weight(it_start)/RT_step
   Nt_each_group=N_inter_steps
 endif
 !
 weight=1._SP
 it_0=1
 if (trim(RT_dynamics_KIND)=="FIELD") then
   call Efield_duplicate(Efield(i_Pump),E_dummy)
   if (trim(E_dummy%ef_name)=="QSSIN")  E_dummy%ef_name="GAUSS"
   do it=1,NE_steps
     TIME=cmplx(RT_step*(it-1)-Efield(i_Pump)%t_initial_indx*RT_step,0._SP)
     weight(it)=abs(small_a(TIME,E_dummy,1))**2
     if (abs(weight(it)-1.)<1.E-5) it_0=it
     if (it>Efield(i_Pump)%t_final_indx) then
       it_0=it
       weight(it+1:)=weight(it)
       exit
     endif
   enddo
 endif
 !
 if (RT_dynamics_decay_time>0.) then
   do it=it_0,NE_steps
     TIME=RT_step*(it-it_0)
     if(it>1) weight(it)=weight(it-1)
     if(real(TIME)<RT_dynamics_decay_time) cycle
     weight(it)=min(1._SP/(real(TIME)/RT_dynamics_decay_time)**0.5_SP,0.05)
   enddo
 endif
 !
 call live_timing('Lifetimes evaluation plan',NE_steps)
 !
 do it=it_start,NE_steps
   if (it< it_0) weight(it)=max(weight(it),0.05)
   if (it>=it_0) weight(it)=max(weight(it),0.002)
 enddo
 !
 do it=it_start,NE_steps
   !
   call live_timing(steps=1)
   !
   it_relative  = mod(it-it_last,Nt_each_group)+1
   !
   if (it_relative>1) cycle
   !
   if (NE_steps-it+1<N_intra_steps*Life_MEM_steps) then
     call BUILD_up_group(1)
   else
     call BUILD_up_group(N_intra_steps)
   endif
   if (it==it_start) then
     do it_=it_start,it_start+N_intra_steps*(Life_MEM_steps-1)-1
       if (RT_GAMMA_T_status(it_)==0.and.     l_manual_excitation) RT_GAMMA_T_status(it_)= 1
       if (RT_GAMMA_T_status(it_)==0.and..not.l_manual_excitation) RT_GAMMA_T_status(it_)=-1
     enddo
   endif
   !
   N_inter_steps=real(RT_dynamics_steps(1))/weight(it)/RT_step
   N_intra_steps=real(RT_dynamics_steps(2))/weight(it)/RT_step
   !
   Nt_each_group= N_inter_steps
   it_last=it
   RT_SET_lifetimes_evaluation_plan=RT_SET_lifetimes_evaluation_plan+1
   !
 enddo
 !
 call live_timing( )
 !
 contains
   !
   subroutine BUILD_up_group(INTRA_steps)
     !
     integer :: it_,INTRA_steps
     !
     do it_=1,INTRA_steps*Life_MEM_steps
       !
       if (it_+it-1>NE_steps) cycle
       !
       RT_GAMMA_T_status(it_+it-1)=0
       !
       if (INTRA_steps==1) then
         if (it_<=Life_MEM_steps) RT_GAMMA_T_status(it_+it-1)=1
       else
         if (it_<INTRA_steps*Life_MEM_steps) then
           if (mod(it_-1,INTRA_steps)==0) then
             RT_GAMMA_T_status(it_+it-1)=1
           endif
         endif
       endif
       !
     enddo
     !
   end subroutine
   !
end function RT_SET_lifetimes_evaluation_plan
