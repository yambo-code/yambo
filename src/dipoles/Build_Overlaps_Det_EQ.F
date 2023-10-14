!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
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
 use DIPOLES,        ONLY:DIP_S
 !
 implicit none
 !
 integer,      intent(in) :: ikbz,i_sp_pol
 type(levels), intent(in) :: Xen
 !
 complex(SP), intent(out) :: S_det(6)
 complex(SP), intent(out) :: Sm1_plus (Xen%nbf(i_sp_pol),Xen%nbm(i_sp_pol),6)
 complex(SP), intent(out) :: Sm1_minus(Xen%nbf(i_sp_pol),Xen%nbm(i_sp_pol),6)
 !
 ! Work Space
 !
 integer     :: idx(3),id,ikm,istep,max_step,nb
 complex(SP) :: S_val(1:Xen%nbf(i_sp_pol),1:Xen%nbf(i_sp_pol))
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
       nb=Xen%nbm(i_sp_pol)
       !
       ! Overlap S(k, k-dk)
       !
       S_val=hermitian(DIP_S(1:nb,1:nb,id+(istep-1)*3,ikm,i_sp_pol))
       call LINEAR_ALGEBRA_driver(INV,M=S_val,lib_in=USE_LK)
       Sm1_minus(:,:,id+(istep-1)*3)=S_val
       !
       ! Overlap S(k, k+dk)
       !
       S_val=DIP_S(1:nb,1:nb,id+(istep-1)*3,ikbz,i_sp_pol)
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
