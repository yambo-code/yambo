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
subroutine RT_databases_IO(Xen,it)
 !
 use QP_m,           ONLY:QP_nk
 use real_time,      ONLY:ID,NE_dynamics,l_NE_with_fields,NE_i_time,it_start,&
&                         OCCUPATIONS_IO_and_interpolate
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
 integer :: ik,io_G,io_J_and_P,COM_
 !
 ! Write G_lesser (always)
 !
 io_G=0
 COM_=NONE
 if (it==it_start) COM_=REP
 call io_control(ACTION=OP_WR,COM=COM_, SEC=(/1/),ID=ID(5))
 io_G=io_SC_components('G' ,Xen,ID(5))
 !
 do ik = 1, QP_nk 
   call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(5))
   io_G=io_SC_components('G',Xen,ID(5))
 enddo
 !
 ! Occupations
 !
 call OCCUPATIONS_IO_and_interpolate(Xen,'G')
 !
 if(l_NE_with_fields.and.NE_dynamics) then
   !
   ! Remember that negative time = adiabatic switching
   ! no need to store current
   !
   io_J_and_P=0 
   !
   ! Write current and polarization on disk
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(4)) 
   io_J_and_P=io_SC_components('J_and_P' ,Xen,ID(4))
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/NE_i_time+1/),ID=ID(4)) 
   io_J_and_P=io_SC_components('J_and_P' ,Xen,ID(4))
   !
 endif
 !
end subroutine RT_databases_IO
