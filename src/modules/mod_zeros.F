!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
module zeros
 !
 use pars,        ONLY:SP,zero_dfl
 implicit none
 !
 ! ZERO's
 !
 real(SP)            :: zero_norm
 real(SP)            :: k_iku_zero(3)
 real(SP)            :: k_rlu_zero(3)
 real(SP)            :: G_iku_zero(3)
 !
 contains 
   !
   subroutine define_zeros(vector_,ref_value,zero_) 
   !
   use vec_operate, ONLY: sort
   !
   real(SP)          ::  zero_
   real(SP), optional::  vector_(:)
   real(SP), optional::  ref_value
   !
   ! Work Space
   !
   integer              :: i1
   real(SP)             :: r1
   real(SP), allocatable:: tmp_arr(:)
   !
   if (present(ref_value)) then
     zero_=ref_value    
     return
   endif
   !
   if (present(vector_)) then
     allocate(tmp_arr(size(vector_)))
     forall(i1=1:size(vector_)) tmp_arr(i1)=abs(vector_(i1))
     call sort(tmp_arr)
     zero_=zero_dfl
     do i1=1,size(vector_)-1
       r1=(tmp_arr(i1+1)-tmp_arr(i1))/10._SP
       if (r1>zero_dfl) then
         zero_=r1
         exit
       endif
     enddo
     deallocate(tmp_arr)
     return
   endif
   !
   zero_=zero_dfl
   !
   end subroutine
   !
end module 
