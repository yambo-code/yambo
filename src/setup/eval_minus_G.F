!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine eval_minus_G()
 !
 ! Output: minus_G(ng_closed)
 !
 ! G_{minus_G(ig)} = -G_ig
 !
 use D_lattice,    ONLY:inv_index
 use R_lattice,    ONLY:ng_closed,g_rot,minus_G
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer  :: ig
 !
 YAMBO_FREE(minus_G)
 YAMBO_ALLOC(minus_G,(ng_closed))
 !
 minus_G=0
 !
 if (inv_index>0) then
   do ig=1,ng_closed
     minus_G(ig)=g_rot(ig,inv_index)
   enddo
 else
   minus_G(1)=1
   if (ng_closed>1) call G_rot_grid(-1,'set_table')
 endif
 !
end subroutine
