!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine INTERPOLATION_driver_end(ID)
 !
 use interpolate_tools,    ONLY:INTERPOLATION_reset
 use interpolate,          ONLY:INTERPOLATE_msg_fmt,INTERPOLATE_initialized,&
&                               lattice_vectors,INTERP_obj,max_interpls,int_sop
 !
#include<memory.h>
 !
 integer, intent(in) :: ID
 !
 ! Work space
 !
 integer             :: i_obj
 !
 if (ID>0) then
   !
   ! Reset only specific ID
   !
   if (.not.INTERP_obj(ID)%in_use) return 
   call INTERPOLATION_reset(ID)
   !
 else
   !
   ! Reset all IDs and clear lattice vectors
   !
   do i_obj=1,max_interpls
     call INTERPOLATION_reset(i_obj)
   enddo
   !
   YAMBO_FREE_P(lattice_vectors)
   !
   YAMBO_FREE(int_sop)
   INTERPOLATE_initialized=.FALSE.
   INTERPOLATE_msg_fmt="rs"
   !
 endif
 !
end subroutine INTERPOLATION_driver_end
