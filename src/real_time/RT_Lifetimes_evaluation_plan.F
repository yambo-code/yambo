!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
integer function RT_Lifetimes_evaluation_plan( )
 !
 ! Here I define the components of an integer array
 ! RT_GAMMA_T_status(NE_steps)
 !
 use units,          ONLY:FS2AUT
 use pars,           ONLY:IP2,SP
 use fields,         ONLY:Efield,i_Pump,ext_field,Efield_duplicate,small_a
 use memory_m,       ONLY:mem_est
 use real_time,      ONLY:RT_GAMMA_T_status,NE_steps,RT_dynamics_steps,&
&                         RT_step,RT_dynamics_KIND,RT_dynamics_decay_time,it_start,Life_MEM_steps
 !
 implicit none
 !
 ! Work Space
 !
 integer         :: it,N_inter_steps,N_intra_steps,it_first,it_0,it_,i1,N_blocks
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
 N_inter_steps=nint(RT_dynamics_steps(1)/RT_step)
 N_intra_steps=nint(RT_dynamics_steps(2)/RT_step)
 !
 ! RT_GAMMA_T_status = -1   KEEP constant 
 !                     >0   EVAL and SAVE
 !                      0   INTERPOLATE  
 !
 RT_Lifetimes_evaluation_plan=0
 !
 weight=1._SP
 it_0=1
 !
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
 ! Exponential decay for T>To
 !
 if (RT_dynamics_decay_time>0.) then
   do it=it_0,NE_steps
     TIME=RT_step*(it-it_0)
     weight(it)=weight(it)*exp(-real(TIME)/RT_dynamics_decay_time)
   enddo
 endif
 !
 if (it_start> 1) then
   N_inter_steps=nint(real(RT_dynamics_steps(1))/weight(it_start+1)/RT_step)
   N_intra_steps=nint(real(RT_dynamics_steps(2))/weight(it_start+1)/RT_step)
 endif
 !
 it_first=it_start+1
 !
 do it=it_start,NE_steps
   if (it< it_0) weight(it)=max(weight(it),0.1)
   if (it>=it_0) weight(it)=max(weight(it),0.001)
 enddo
 !
 ! Number of blocks composed by N_inter_steps elements 
 !
 N_blocks=N_inter_steps/N_intra_steps
 !
 ! At the beginning I do not have enough points to perform the fit. I, therefore,
 ! introduce blocks with an increasing number of elements
 !
 it=it_start
 do i1=1,N_intra_steps,2
   call BUILD_up_group(N_blocks*i1,i1)
   it=it+N_blocks*i1
 enddo
 !
 ! And now the groups with the full number
 !
 do while (it<NE_steps)
   !
   if (NE_steps-it+1<N_inter_steps) then
     call BUILD_up_group(N_inter_steps/N_intra_steps,1)
   else
     call BUILD_up_group(N_inter_steps,N_intra_steps)
   endif
   !
   N_inter_steps=nint(real(RT_dynamics_steps(1))/weight(it)/RT_step)
   N_intra_steps=nint(real(RT_dynamics_steps(2))/weight(it)/RT_step)
   !
   it=it+N_inter_steps
   RT_Lifetimes_evaluation_plan=RT_Lifetimes_evaluation_plan+Life_MEM_steps
   !
 enddo
 !
 !DEBUG>
 !do it=1,NE_steps
 ! TIME=RT_step*(it-1)
 ! !if (RT_GAMMA_T_status(it)/=0) 
 ! write (*,'(f6.2,2i6)') real(TIME/FS2AUT),RT_GAMMA_T_status(it),it
 !enddo
 !stop
 !DEBUG<
 !
 contains
   !
   subroutine BUILD_up_group(N_inter,N_intra)
     !
     integer :: it_,N_inter,N_intra
     logical :: local_condition
     !
     do it_=1,N_inter
       !
       if (it_+it-1>NE_steps) cycle
       !
       RT_GAMMA_T_status(it_+it-1)=0
       !
       if (N_intra>1) then
         local_condition=mod(it_,N_intra)==1
         if (local_condition) RT_GAMMA_T_status(it_+it-1)=int(real(it_-1)/real(N_intra))+1
       else
         RT_GAMMA_T_status(it_+it-1)=it_
       endif
       !
     enddo
     !
   end subroutine
   !
end function RT_Lifetimes_evaluation_plan
