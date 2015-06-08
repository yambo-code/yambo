!
! Copyright (C) 2000-2013 C. Attaccalite and the YAMBO team 
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
subroutine Build_Overlaps_Det(Xen,ik,X,Sm1_plus,Sm1_minus,S_det)
 !
 ! Build inverse overlaps qnd overlaps determinants between valence bands
 !
 use pars,           ONLY:SP,cZERO,cONE
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:k_map
 use matrix_operate, ONLY:hermitian,INV,USE_LK
 use interfaces,     ONLY:MATRIX_driver
 use vec_operate,    ONLY:k_periodic_idx
 use X_m,            ONLY:X_t,DIP_S
 implicit none
 !
 integer,      intent(in) :: ik
 type(levels), intent(in) :: Xen
 type(X_t),    intent(in) :: X
 complex(SP), intent(out) :: Sm1_plus (Xen%nbf,Xen%nbf,3,n_sp_pol)
 complex(SP), intent(out) :: Sm1_minus(Xen%nbf,Xen%nbf,3,n_sp_pol)
 complex(SP), intent(out) :: S_det(3,n_sp_pol)
 !
 ! Work Space
 !
 integer     :: i1,i2,i3,i4,iv,idx(3),id,ikp,ikm,i_spin
 complex(SP) :: S_val(1:Xen%nbf,1:Xen%nbf)
 !
 S_det=cZERO
 !   
 do id=1,3
   !
   if(k_map%max_kdir(id)/=1) then
     !
     ! Overlap S(k, k-dk)
     !
     idx=k_map%k_map_inv(ik,:)
     idx(id)=idx(id)-1
     idx=k_periodic_idx(idx,k_map)
     ikm=k_map%k_map_dir(idx(1),idx(2),idx(3))
     !
     do i_spin=1,n_sp_pol
       !
       S_val=hermitian(DIP_S(1:Xen%nbf,1:Xen%nbf,id,ikm,i_spin))
       call MATRIX_driver(INV,S_val,lib_in=USE_LK)
       !
       Sm1_minus(:,:,id,i_spin)=S_val
       !
       ! Overlap S(k, k+dk)
       !
       S_val=DIP_S(1:Xen%nbf,1:Xen%nbf,id,ik,i_spin)
       call MATRIX_driver(INV,S_val,lib_in=USE_LK,det=S_det(id,i_spin))
       Sm1_plus(:,:,id,i_spin)=S_val
       !
     enddo
     !
   else  ! non-periodic directions
     !
     do i_spin=1,n_sp_pol
       S_det(id,i_spin)        =cZERO
       Sm1_plus(:,:,id,i_spin) =cZERO
       Sm1_minus(:,:,id,i_spin)=cZERO
     enddo
     !
   endif
   ! 
 enddo 
 !
end subroutine Build_Overlaps_Det
