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
subroutine RT_time_step_update( )
 !
 use units,          ONLY:AUT2FS
 use pars,           ONLY:SP
 use real_time,      ONLY:RT_bands,RT_nk,dG_lesser,MEM_pointer,i_MEM_previous,&
&                         i_MEM_now,i_MEM_next,MAX_dG_dT_indexes,RT_step,RT_dyn_step,&
&                         RT_life_interp_times,Life_MEM_steps
 use RT_control,     ONLY:RT_IO_time
 use com,            ONLY:warning
 use stderr,         ONLY:real2ch
 implicit none
 ! 
 ! Work Space 
 ! 
 integer           :: ib,ibp,ik,nT_updated
 complex(SP)       :: dG_dT_max(2),dG_dT(2),ERROR,dG(3)
 real(SP)          :: dT_now,dT_previous
 integer, external :: RT_Lifetimes_evaluation_plan
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
 if (abs(ERROR)<0.01.and.(dT_previous+dT_now)<minval(RT_IO_time)) then 
   !
   call warning("Time Step update. From "//trim(real2ch(dT_now*AUT2FS*1000.))//"as => "//&
&                                          trim(real2ch((dT_previous+dT_now)*AUT2FS*1000.))//"as")
   !
   RT_dyn_step=dT_previous+dT_now
   !
   ! I need to re-calculate the lifetimes evaluation plan in order to be consistent with the new Time Step
   !
   RT_life_interp_times(2) = nint(RT_life_interp_times(2)/RT_dyn_step)*RT_dyn_step
   RT_life_interp_times(1)=RT_life_interp_times(2)*Life_MEM_steps
   !
   nT_updated =RT_Lifetimes_evaluation_plan( )
   !
 endif
 !
end subroutine RT_time_step_update
