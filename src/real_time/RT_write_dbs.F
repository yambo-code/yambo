!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_write_dbs(Xen,it)
 !
 use com,            ONLY:error
 use QP_m,           ONLY:QP_nk
 use real_time,      ONLY:ID,i_time,l_NE_with_fields,max_rt_steps_disk
 use IO_m,           ONLY:io_control,OP_WR,WR_CL_IF_END,NONE,WR,REP,OP_WR_CL
 use electrons,      ONLY:levels
 !
 implicit none
 !
 integer, intent(in)      :: it
 integer, external        :: io_SC_components
 type(levels),intent(in)  :: Xen
 !
 ! Work Space
 !
 integer :: ik,io_G,io_J
 !
 io_G=0
 !
 if (it==1.or.it==max_rt_steps_disk) call io_control(ACTION=OP_WR,COM=REP, SEC=(/1/),ID=ID(5))
 if (it> 1) call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=ID(5))
 io_G=io_SC_components('G' ,Xen,ID(5))
 !
 do ik = 1, QP_nk 
   call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(5))
   io_G=io_SC_components('G',Xen,ID(5))
 enddo
 !
 if(io_G/=0) call error("Error writing G_lesser! ")
 !
 if(l_NE_with_fields.and.i_time>0) then
   !
   ! Remember that negative time = adiabatic switching
   ! no need to store current
   !
   io_J=0 
   !
   ! Write current on disk
   !
   if(i_time==1) then 
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(4)) 
     io_J=io_SC_components('J' ,Xen,ID(4))
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/2/),ID=ID(4)) 
     io_J=io_SC_components('J' ,Xen,ID(4))
   else
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/i_time+1/),ID=ID(4)) 
     io_J=io_SC_components('J' ,Xen,ID(4))
   endif
   !
   if(io_J/=0) call error("Error writing current! ")
   !
 endif
 !
end subroutine RT_write_dbs
