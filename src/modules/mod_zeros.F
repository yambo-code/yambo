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
   subroutine define_zeros(vector_,ref_value,zero_,RLU) 
     !
     use vec_operate, ONLY: sort,c2a
     !
     real(SP)          ::  zero_(3)
     real(SP), optional::  vector_(:,:)
     real(SP), optional::  ref_value(3)
     logical , optional::  RLU
     !
     ! Work Space
     !
     integer              :: i1,ic,nv
     real(SP)             :: r1
     real(SP), allocatable:: tmp_arr(:),vector_rlu(:,:)
     !
     if (present(ref_value)) then
       zero_=ref_value    
       return
     endif
     !
     if (present(vector_)) nv=size(vector_,1)
     !
     if (present(RLU)) then
       if (RLU) then
         allocate(vector_rlu(nv,3))
         do i1=1,nv
           call c2a(v_in=vector_(i1,:),v_out=vector_rlu(i1,:),mode='ki2a')
         enddo
       endif
     endif
     !
     if (present(vector_)) then
       allocate(tmp_arr(size(vector_,1)))
       do ic=1,3
         if (allocated(vector_rlu)) then
           forall(i1=1:nv) tmp_arr(i1)=abs(vector_rlu(i1,ic))
         else
           forall(i1=1:nv) tmp_arr(i1)=abs(vector_(i1,ic))
         endif
         call sort(tmp_arr)
         zero_(ic)=zero_dfl
         do i1=1,nv-1
           r1=(tmp_arr(i1+1)-tmp_arr(i1))/10._SP
           if (r1>zero_dfl) then
             zero_(ic)=r1
             exit
           endif
         enddo
       enddo
       !
       zero_=minval(zero_)
       !
       deallocate(tmp_arr)
       if (allocated(vector_rlu)) deallocate(vector_rlu)
       return
     endif
     !
     zero_=zero_dfl
     !
   end subroutine
   !
end module 
