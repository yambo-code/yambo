!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
integer function k_the_nearest(k_ref,k,n_k,ROTATE_k_ref,i_err)
 !
 ! On output : - k(k_the_nearest,:) is the nearest to k_ref
 !             - k(k_the_nearest,:)==k_ref when i_err=0
 !
 use com,            ONLY:error
 use pars,           ONLY:SP,zero_dfl
 use vec_operate,    ONLY:iku_v_norm,c2a,k2bz
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:rl_sop
 implicit none
 !
 integer :: n_k
 real(SP):: k_ref(3)
 real(SP):: k(n_k,3)
 logical, optional :: ROTATE_k_ref
 integer, optional :: i_err
 !
 ! Work Space
 !
 integer :: i1,i2,is
 real(SP):: v(3),k_distance
 real(SP):: k_rotated(3,nsym)
 logical :: local_flag
 !
 k_the_nearest=0
 if (present(i_err)) then
   i_err=-1
 endif
 k_distance=10000.
 !
 local_flag=.FALSE.
 if (present(ROTATE_k_ref)) then
   local_flag=ROTATE_k_ref
 endif
 !
 if (.not.local_flag) then
   do i1=1,n_k
     v=k_ref-k(i1,:)
     call k2bz(v)
     if (iku_v_norm(v)<k_distance) then
       k_distance=iku_v_norm(v)
       k_the_nearest=i1
     endif
     if (k_distance<zero_dfl) then
       if (present(i_err)) then
         i_err=0
       endif
       exit 
     endif
   enddo
   return
 endif
 !
 do is=1,nsym
   k_rotated(:,is)=matmul(rl_sop(:,:,is),k_ref)
 enddo
 ! 
 do i1=1,n_k
   !
   do is=1,nsym
     v=k_rotated(:,is)-k(i1,:)
     !
     call c2a(v_in=v,mode='ki2a')
     v(1)=abs(v(1)-nint(v(1)))     
     v(2)=abs(v(2)-nint(v(2)))     
     v(3)=abs(v(3)-nint(v(3)))   
     call c2a(v_in=v,mode='ka2i')
     !
     if (iku_v_norm(v)<k_distance) then
       k_distance=iku_v_norm(v)
       k_the_nearest=i1
     endif
     if (k_distance<zero_dfl) then
       if (present(i_err)) then
         i_err=0
       endif
       exit
     endif
     !
   enddo
 enddo
 !
 if (k_the_nearest==0) call error('in k_the_nearest. Nearest point not found.')
 !
end function
