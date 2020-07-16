!
!        Copyright (C) 2000-2020 the YAMBO team
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
integer function k_the_nearest(k_ref,k,n_k,k_dist,ROTATE_k_ref,k_symm,i_err)
 !
 ! On output : - k(k_the_nearest,:) is the nearest to k_ref
 !             - k(k_the_nearest,:)==k_ref when i_err=0
 !
 ! All k-points must be in IKU coordinates
 !
 use pars,           ONLY:SP,zero_dfl
 use vec_operate,    ONLY:iku_v_norm,k2bz
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:rl_sop
 !
 implicit none
 !
 integer :: n_k
 real(SP):: k_ref(3)
 real(SP):: k(n_k,3)
 real(SP),optional :: k_dist(n_k)
 logical, optional :: ROTATE_k_ref
 integer, optional :: k_symm
 integer, optional :: i_err
 !
 ! Work Space
 !
 integer :: ik,is,nsym_loop
 real(SP):: v(3),k_distance_loc,k_rotated(3,nsym)
 logical :: ROTATE_k_ref_
 !
 k_the_nearest=0
 if (present(i_err)) then
   i_err=-1
 endif
 k_distance_loc=10000._SP
 !
 ROTATE_k_ref_=.FALSE.
 nsym_loop=nsym
 if (present(ROTATE_k_ref)) then
   ROTATE_k_ref_=ROTATE_k_ref
 endif
 !
 if (ROTATE_k_ref_) then
   nsym_loop=nsym
   do is=1,nsym
     k_rotated(:,is)=matmul(rl_sop(:,:,is),k_ref)
   enddo
 else
   nsym_loop=1
   k_rotated(:,1)=k_ref
 endif
 ! 
 if(present(k_dist)) then
   k_dist=10000.
 endif
 k_distance_loc=10000.
 do ik=1,n_k
   do is=1,nsym_loop
     v=k_rotated(:,is)-k(ik,:)
     call k2bz(v)
     if (present(k_dist)) then
       k_dist(ik)=min(k_dist(ik),iku_v_norm(v))
     endif
     if ( iku_v_norm(v)<k_distance_loc) then
       k_distance_loc=iku_v_norm(v)
       k_the_nearest=ik
       if (present(k_symm)) then
         k_symm=is
       endif
     endif
     if (k_distance_loc<zero_dfl) then
       if (present(i_err)) i_err=0
       exit
     endif
     !
   enddo
 enddo
 !
 if (k_the_nearest==0) call error('in k_the_nearest. Nearest point not found.')
 !
end function
