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
subroutine RT_MEMORY_index()
 !
 use real_time,      ONLY:G_MEM_steps,MEM_pointer,i_MEM_now,NE_i_time,&
&                         i_MEM_previous,i_MEM_next,RT_dyn_step,RT_step
 !
 implicit none
 !
 ! Work Space
 !
 integer :: i_min_index,i_mem
 !
 i_MEM_now     =0
 i_MEM_next    =0
 i_MEM_previous=0
 !
 ! Find a "NEW" Memory Location for  NE_i_time+RT_dyn_step/RT_step
 !
 i_min_index=minval(MEM_pointer)
 !
 do i_mem=1,G_MEM_steps
   if (MEM_pointer(i_mem)==i_min_index) then
     MEM_pointer(i_mem)=nint(NE_i_time+RT_dyn_step/RT_step)
     i_MEM_next=i_mem
     exit
   endif
 enddo
 !
 ! Find a "OLD" Memory Location for NE_i_time and for the previous Time point
 !
 i_min_index=minval(MEM_pointer)
 !
 do i_mem=1,G_MEM_steps
   !
   if (MEM_pointer(i_mem)==NE_i_time)   i_MEM_now=i_mem
   !
   if (NE_i_time<G_MEM_steps.or.G_MEM_steps==2) cycle
   !
   if (MEM_pointer(i_mem)==i_min_index) i_MEM_previous=i_mem
   !
 enddo
 !
end subroutine RT_MEMORY_index
