!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
integer function k_the_nearest(k_ref,k,n_k,be_carefull,i_err)
 !
 ! On output : - k(k_the_nearest,:) is the nearest to k_ref
 !             - k(k_the_nearest,:)==k_ref when i_err=0
 !
 use com,            ONLY:error
 use pars,           ONLY:SP,zero_dfl
 use vec_operate,    ONLY:iku_v_norm,c2a,k2bz
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:rl_sop
#if defined _RT
 use real_time,      ONLY:rl_sop_eq,nsym_eq
#endif
 implicit none
 !
 integer :: n_k,i_err
 real(SP):: k_ref(3),k(n_k,3)
 logical :: be_carefull
 !
 ! Work Space
 !
 integer :: i1,i2,is
 real(SP):: v(3),k_distance
#if defined _RT
 real(SP):: k_rotated(3,max(nsym_eq,nsym))
#else
 real(SP):: k_rotated(3,nsym)
#endif
 !
 k_the_nearest=0
 i_err=-1
 k_distance=10000.
 !
 if (.not.be_carefull) then
   do i1=1,n_k
     v=k_ref-k(i1,:)
     call k2bz(v)
     if (iku_v_norm(v)<k_distance) then
       k_distance=iku_v_norm(v)
       k_the_nearest=i1
     endif
     if (k_distance<zero_dfl) then
       i_err=0
       exit 
     endif
   enddo
   return
 endif
 !
 do is=1,nsym
   k_rotated(:,is)=matmul(rl_sop(:,:,is),k_ref)
 enddo
#if defined _RT
 if (nsym_eq/=0) then
   do is=1,nsym_eq
     k_rotated(:,is)=matmul(rl_sop_eq(:,:,is),k_ref)
   enddo
 endif
#endif
 ! 
 do i1=1,n_k
   !
#if defined _RT
   do is=1,max(nsym_eq,nsym)
#else
   do is=1,nsym
#endif
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
       i_err=0
       exit
     endif
     !
   enddo
 enddo
 !
 if (k_the_nearest==0) call error('in k_the_nearest. Nearest point not found.')
 !
end function
