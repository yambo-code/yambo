!
!        Copyright (C) 2000-2018 the YAMBO team
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
integer function RT_Lifetimes_evaluation_plan( )
 !
 ! Here I define the components of an integer array RT_TIME_status(NE_steps)
 !
 use pars,           ONLY:SP
 use RT_control,     ONLY:NE_i_start_time
 use real_time,      ONLY:RT_TIME_status,NE_steps,RT_life_extrap_times,&
&                         RT_dyn_step,l_RT_lifetimes_extrapolation,Life_MEM_steps,NE_i_time
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer         :: it,N_inter_steps,N_intra_steps,it_,i1,N_blocks,i_default
 complex(SP)     :: TIME
 !
 if (.not.allocated(RT_TIME_status)) then
   YAMBO_ALLOC(RT_TIME_status,(NE_steps))
   RT_TIME_status=1
 endif
 !
 if (RT_life_extrap_times(1)<=0. .or. NE_i_start_time>=NE_steps) then
   l_RT_lifetimes_extrapolation=.FALSE.
   Life_MEM_steps=1
   RT_Lifetimes_evaluation_plan=NE_steps
   return
 endif 
 !
 ! Number of steps & blocks composed by N_inter_steps elements 
 !
 N_inter_steps=nint(RT_life_extrap_times(1)/RT_dyn_step)
 N_intra_steps=nint(RT_life_extrap_times(2)/RT_dyn_step)
 N_blocks     =N_inter_steps/N_intra_steps
 !
 ! RT_TIME_status = -1   KEEP constant 
 !                  >0   EVAL and SAVE
 !                   0   EXTRAPOLATE  
 !
 ! Default action
 !
 i_default= -1 ! Keep constant
 if (l_RT_lifetimes_extrapolation) i_default=0 ! Extrapolate
 !
 RT_Lifetimes_evaluation_plan=i_default
 !
 ! Starting time point
 !
 it=NE_i_time
 !
 ! At the beginning I do not have enough points to perform the fit. I, therefore,
 ! introduce blocks with an increasing number of elements
 !
 if (NE_i_time==NE_i_start_time .and. l_RT_lifetimes_extrapolation) then
   !
   do i1=1,N_intra_steps,max(N_intra_steps/10,1)
     call BUILD_up_group(N_blocks*i1,i1)
     it=it+N_blocks*i1
   enddo
   !
 endif
 !
 ! And now the groups with the full number
 !
 do while (it<NE_steps)
   !
   ! Final part
   !
   ! (AM 18/9/18)
   !
   ! The RT_TIME_status form is (let's assume 3 blocks)
   !
   !  1----2----3----
   !  |---|
   !  N_intra_steps
   !
   ! This means that N_inter_steps=N_blocks*N_intra_steps
   !
   ! Now, what I want here is that the last part of the simulation is made in such a way to mantain the number of blocks
   !
   ! If now only NT_last<N_intra_steps*(N_blocks-1) are left I will do
   !
   !  N_intra_steps=(NT_last-1)/(N_blocks-1)
   !
   if (NE_steps-it+1<N_inter_steps) then
     !
     N_inter_steps=NE_steps-it+1
     N_intra_steps=N_inter_steps/N_blocks+1
     call BUILD_up_group(N_inter_steps,N_intra_steps)
     !
     RT_TIME_status(NE_steps)=1
     !
     !call BUILD_up_group(N_inter_steps/N_intra_steps,1)
     !
   else
     call BUILD_up_group(N_inter_steps,N_intra_steps)
   endif
   !
   it=it+N_inter_steps
   !
 enddo
 !
 RT_Lifetimes_evaluation_plan = count( RT_TIME_status >0 )
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
       RT_TIME_status(it_+it-1)=i_default
       !
       if (N_intra>1) then
         local_condition=mod(it_,N_intra)==1
         if (local_condition) RT_TIME_status(it_+it-1)=int(real(it_-1)/real(N_intra))+1
       else
         RT_TIME_status(it_+it-1)=it_
       endif
       !
     enddo
     !
   end subroutine
   !
end function RT_Lifetimes_evaluation_plan
