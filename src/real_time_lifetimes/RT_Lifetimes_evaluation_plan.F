!
!        Copyright (C) 2000-2016 the YAMBO team
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
 use pars,           ONLY:IP2,SP
 use memory_m,       ONLY:mem_est
 use RT_control,     ONLY:it_start
 use real_time,      ONLY:RT_TIME_status,NE_steps,RT_life_interp_steps,&
&                         RT_step,l_RT_lifetimes_interpolation,Life_MEM_steps
 !
 implicit none
 !
 ! Work Space
 !
 integer         :: it,N_inter_steps,N_intra_steps,it_first,it_0,it_,i1,N_blocks,i_default
 real(SP)        :: weight(NE_steps)
 complex(SP)     :: TIME
 !
 if (.not.allocated(RT_TIME_status)) then
   allocate(RT_TIME_status(NE_steps))
   call mem_est("RT_TIME_status",(/NE_steps/),(/IP2/))
   RT_TIME_status=1
 endif
 !
 if (RT_life_interp_steps(1)<0. .or. it_start>=NE_steps) then
   l_RT_lifetimes_interpolation=.FALSE.
   Life_MEM_steps=1
   RT_Lifetimes_evaluation_plan=0
   return
 endif 
 !
 Life_MEM_steps=nint(RT_life_interp_steps(1)/RT_life_interp_steps(2))
 if (Life_MEM_steps<3) Life_MEM_steps=3
 RT_life_interp_steps(1)=RT_life_interp_steps(2)*Life_MEM_steps
 !
 N_inter_steps=nint(RT_life_interp_steps(1)/RT_step)
 N_intra_steps=nint(RT_life_interp_steps(2)/RT_step)
 !
 ! RT_TIME_status = -1   KEEP constant 
 !                  >0   EVAL and SAVE
 !                   0   INTERPOLATE  
 !
 ! Default action
 !
 i_default= -1 ! Keep constant
 if (l_RT_lifetimes_interpolation) i_default=0 ! Interpolate
 !
 RT_Lifetimes_evaluation_plan=i_default
 !
 weight=1._SP
 it_0=1
 !
 if (it_start> 1) then
   N_inter_steps=nint(real(RT_life_interp_steps(1))/weight(it_start+1)/RT_step)
   N_intra_steps=nint(real(RT_life_interp_steps(2))/weight(it_start+1)/RT_step)
 endif
 !
 it_first=it_start+1
 !
 do it=it_start,NE_steps
   if (it< it_0) weight(it)=max(weight(it),0.1)
   if (it>=it_0) weight(it)=max(weight(it),0.01)
 enddo
 !
 ! Number of blocks composed by N_inter_steps elements 
 !
 N_blocks=N_inter_steps/N_intra_steps
 !
 it=it_start
 !
 ! At the beginning I do not have enough points to perform the fit. I, therefore,
 ! introduce blocks with an increasing number of elements
 !
 do i1=1,N_intra_steps,max(N_intra_steps/10,1)
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
   N_inter_steps=nint(real(RT_life_interp_steps(1))/weight(it)/RT_step)
   N_intra_steps=nint(real(RT_life_interp_steps(2))/weight(it)/RT_step)
   !
   it=it+N_inter_steps
   !
 enddo
 !
 RT_Lifetimes_evaluation_plan = count( RT_TIME_status>0 )
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
