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
 use pars,           ONLY:IP2,SP
 use fields,         ONLY:Efield,i_Pump,ef_i_time,ext_field,Efield_duplicate,small_a
 use units
 use memory_m,       ONLY:mem_est
 use real_time,      ONLY:RT_GAMMA_T_status,NE_steps,RT_dynamics_steps,Life_MEM_steps,&
&                         RT_step,RT_dynamics_KIND,RT_dynamics_decay_time
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
 allocate(RT_GAMMA_T_status(NE_steps))
 call mem_est("RT_GAMMA_T_status",(/NE_steps/),(/IP2/))
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
 it_last=1
 Nt_each_group=N_inter_steps
 RT_SET_lifetimes_evaluation_plan=0
 weight=1._SP
 !
 if (trim(RT_dynamics_KIND)=="FIELD") then
   call Efield_duplicate(Efield(i_Pump),E_dummy)
   if (trim(E_dummy%ef_name)=="QSSIN")  E_dummy%ef_name="GAUSS"
   do it=1,NE_steps
     TIME=cmplx(RT_step*(it-1)-ef_i_time(i_Pump)*RT_step,0._SP)
     weight(it)=abs(small_a(TIME,E_dummy,1))**2
     if (abs(weight(it)-1.)<1.E-5) it_0=it
   enddo
 else if (RT_dynamics_decay_time>0.) then
   do it=1,NE_steps
     TIME=RT_step*(it-1)
     weight(it)=exp(-TIME/RT_dynamics_decay_time)
   enddo
   it_0=-1
 endif
 !
 do it=1,NE_steps
   !
   it_relative  = mod(it-it_last,Nt_each_group)+1
   !
   if (it_relative>1) cycle
   !
   if (NE_steps-it+1<N_intra_steps*Life_MEM_steps) then
     call BUILD_up_group(Life_MEM_steps,1)
   else
     call BUILD_up_group(min(N_inter_steps,NE_steps-it+1),N_intra_steps)
   endif
   if (it==1) then
     do it_=1,N_intra_steps*(Life_MEM_steps-1)
       if (RT_GAMMA_T_status(it_)==0) RT_GAMMA_T_status(it_)=-1
     enddo
   endif
   !
   if (it<it_0) then
     weight(it)=max(weight(it),0.1)  
   else
     weight(it)=max(weight(it),0.01)  
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
 !DEBUG>
 ! do it=1,NE_steps
 !   TIME=RT_step*(it-1)
 !   if (RT_GAMMA_T_status(it)==1) write (300,*) real(TIME)/FS2AUT,1
 !   !write (100,*) real(TIME)/FS2AUT,RT_GAMMA_T_status(it),weight(it)
 ! enddo
 ! stop
 !DEBUG<
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
