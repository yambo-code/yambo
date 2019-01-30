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
subroutine MATRIX_transfer_side_crop(I,J,O)
 !
 implicit none
 integer :: I(2),J(2),O(2)
 !
 O=0
 !
 if (I(2)<J(1)) return
 if (I(1)>J(2)) return
 !
 O(1)=I(1)
 if (I(1)<=J(1)) O(1)=J(1)
 !
 O(2)=J(2)
 if (I(2)<J(2)) O(2)=I(2)
 !
end subroutine
