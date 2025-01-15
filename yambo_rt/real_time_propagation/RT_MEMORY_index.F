!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM 
!
subroutine RT_MEMORY_index( )
 !
 use real_time,      ONLY:G_MEM_steps,MEM_pointer,i_MEM_prev,NE_i_time,&
&                         i_MEM_old,i_MEM_now,RT_dyn_step,RT_step
 !
 implicit none
 !
 ! Work Space
 !
 integer :: i_min_index,i_mem
 !
 i_MEM_prev=0
 i_MEM_now =0
 i_MEM_old =0
 !
 ! Find a "NEW" Memory Location for  NE_i_time+RT_dyn_step/RT_step
 !
 i_min_index=minval(MEM_pointer)
 !
 do i_mem=1,G_MEM_steps
   if (MEM_pointer(i_mem)/=i_min_index) cycle
   MEM_pointer(i_mem)=NE_i_time+nint(RT_dyn_step/RT_step)
   i_MEM_now=i_mem
 enddo
 !
 ! Find a "OLD" Memory Location for NE_i_time and for the previous Time point
 !
 i_min_index=minval(MEM_pointer)
 !
 do i_mem=1,G_MEM_steps
   if (MEM_pointer(i_mem)==NE_i_time)    i_MEM_prev=i_mem
   if (G_MEM_steps==2.and.G_MEM_steps>1) cycle
   if (MEM_pointer(i_mem)==i_min_index)  i_MEM_old=i_mem
 enddo
 !
end subroutine RT_MEMORY_index
