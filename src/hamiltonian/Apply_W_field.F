!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
subroutine Apply_W_field(A_input,Wk,ik,H_nl_sc)
 !
 use pars,           ONLY:SP,cZERO
 use units,          ONLY:SPEED_OF_LIGHT
 use hamiltonian,    ONLY:H_ref_bands
 use fields,         ONLY:gauge_field
 use electric,       ONLY:k_symm_grp,l_no_res_symms 
 use D_lattice,      ONLY:dl_sop
 !
 implicit none
 !
 type(gauge_field), intent(in) :: A_input
 complex(SP),  intent(inout)   :: Wk(3,H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 complex(SP),  intent(inout)   :: H_nl_sc(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 integer,      intent(in)      :: ik
 !
 ! Work Space
 !
 integer :: i1,i2,id,is,is_idx
 complex(SP) :: E_vec_pot(3),WF_symm(3)
 !
 E_vec_pot=-A_input%vecpot_vel/SPEED_OF_LIGHT
 !
 ! Apply residual symmetries if present to Wk
 !
 if(.not.l_no_res_symms.and.k_symm_grp%nsym(ik)>1) then
 !$omp parallel do default(shared), private(i1,i2,WF_symm)
   do i1=H_ref_bands(1),H_ref_bands(2)
     do i2=i1,H_ref_bands(2)
       WF_symm=cZERO  
       do is=1,k_symm_grp%nsym(ik)
	 is_idx=k_symm_grp%indx(ik,is)
	 WF_symm=WF_symm+matmul(dl_sop(:,:,is_idx),Wk(:,i1,i2))
       enddo
       Wk(:,i1,i2)=WF_symm/dble(k_symm_grp%nsym(ik))
       Wk(:,i2,i1)=conjg(Wk(:,i1,i2))
     enddo
   enddo
 !$omp end parallel do
 endif
 !
 !$omp parallel do default(shared), private(i1,id)
 do i1=H_ref_bands(1),H_ref_bands(2)
   do id=1,3
     H_nl_sc(i1,H_ref_bands(1):H_ref_bands(2))=H_nl_sc(i1,H_ref_bands(1):H_ref_bands(2))-         &
     &                                               E_vec_pot(id)*Wk(id,i1,H_ref_bands(1):H_ref_bands(2))
   enddo
 enddo
 !$omp end parallel do
 !
end subroutine Apply_W_field
