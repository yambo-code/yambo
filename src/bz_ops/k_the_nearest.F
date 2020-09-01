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
 use com,            ONLY:error
 use pars,           ONLY:SP,zero_dfl
 use zeros,          ONLY:k_iku_zero
 use vec_operate,    ONLY:iku_v_norm,c2a,k2bz
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:rl_sop
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
 integer :: i1,is
 real(SP):: v(3),k_distance_loc,k_distance_tmp,k_distance_min
 real(SP):: k_rotated(3,nsym)
 logical :: local_flag
 !
 k_the_nearest=0
 if (present(i_err)) i_err=-1
 k_distance_loc=10000._SP
 k_distance_min=iku_v_norm(k_iku_zero)
 !
 local_flag=.FALSE.
 if (present(ROTATE_k_ref)) local_flag=ROTATE_k_ref
 !
 if (.not.local_flag) then
   do i1=1,n_k
     v=k_ref-k(i1,:)
     call k2bz(v)
     k_distance_tmp=iku_v_norm(v,inside_bz=.true.)
     if(present(k_dist)) k_dist(i1)=k_distance_tmp
     if ( k_distance_tmp<abs(k_distance_loc-k_distance_min) ) then
       k_distance_loc=k_distance_tmp
       k_the_nearest=i1
     endif
     if (k_distance_loc<zero_dfl) then
       if (present(i_err)) i_err=0
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
 if(present(k_dist)) k_dist=10000.
 k_distance_loc=10000.
 do i1=1,n_k
   !
   do is=1,nsym
     v=k_rotated(:,is)-k(i1,:)
     !
     ! 2020/07/16: these lines should be replaced with call k2bz(v)
     !
     if(.not.present(k_symm)) then
       call c2a(v_in=v,mode='ki2a')
       v(1)=v(1)-nint(v(1))     
       v(2)=v(2)-nint(v(2))     
       v(3)=v(3)-nint(v(3))   
       call c2a(v_in=v,mode='ka2i')
     endif
     !
     k_distance_tmp=iku_v_norm(v,inside_bz=.true.)
     if(present(k_dist)) k_dist(i1)=min(k_dist(i1),k_distance_tmp)
     if ( k_distance_tmp<abs(k_distance_loc-k_distance_min) ) then
       k_distance_loc=k_distance_tmp
       k_the_nearest=i1
       if(present(k_symm)) k_symm=is
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
