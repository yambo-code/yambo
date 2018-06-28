!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA
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
subroutine Build_Overlaps_Det_EQ(Xen,i_sp_pol,ikbz,Sm1_plus,Sm1_minus,S_det)
 !
 ! Build inverse overlaps and overlaps determinants between valence bands at
 ! equilibrium
 !
 use pars,           ONLY:SP,cZERO
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:k_map
 use matrix_operate, ONLY:hermitian
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver
 use linear_algebra, ONLY:INV,USE_LK
 use vec_operate,    ONLY:k_periodic_idx
 use X_m,            ONLY:DIP_S
 implicit none
 !
 integer,      intent(in) :: ikbz,i_sp_pol
 type(levels), intent(in) :: Xen
 complex(SP), intent(out) :: S_det(6)
 complex(SP), intent(out) :: Sm1_plus (Xen%nbf,Xen%nbm,6)
 complex(SP), intent(out) :: Sm1_minus(Xen%nbf,Xen%nbm,6)
 !
 ! Work Space
 !
 integer     :: idx(3),id,ikm,istep,max_step
 complex(SP) :: S_val(1:Xen%nbf,1:Xen%nbf)
 !
 S_det    =cZERO
 Sm1_plus =cZERO
 Sm1_minus=cZERO
 !   
 do id=1,3
   !
   if(k_map%max_kdir(id)/=1) then
     !
     max_step=1
     if(k_map%max_kdir(id)>=6) max_step=2
     !
     do istep=1,max_step
       idx=k_map%k_map_inv(ikbz,:)
       idx(id)=idx(id)-istep
       idx=k_periodic_idx(idx,k_map)
       ikm=k_map%k_map_dir(idx(1),idx(2),idx(3))
       !
       ! Overlap S(k, k-dk)
       !
       S_val=hermitian(DIP_S(1:Xen%nbm,1:Xen%nbm,id+(istep-1)*3,ikm,i_sp_pol))
       call LINEAR_ALGEBRA_driver(INV,M=S_val,lib_in=USE_LK)
       Sm1_minus(:,:,id+(istep-1)*3)=S_val
       !
       ! Overlap S(k, k+dk)
       !
       S_val=DIP_S(1:Xen%nbm,1:Xen%nbm,id+(istep-1)*3,ikbz,i_sp_pol)
       call LINEAR_ALGEBRA_driver(INV,M=S_val,lib_in=USE_LK,det=S_det(id+(istep-1)*3))
       Sm1_plus(:,:,id+(istep-1)*3)=S_val
       !
     enddo
     !
   endif
   ! 
 enddo 
 !
end subroutine Build_Overlaps_Det_EQ
