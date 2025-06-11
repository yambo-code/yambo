!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
subroutine Apply_W_field(A_input,Wk,H_nl_sc)
 !
 use pars,           ONLY:SP,cZERO
 use units,          ONLY:SPEED_OF_LIGHT
 use hamiltonian,    ONLY:H_ref_bands,H_ref_hole_bands,H_ref_elec_bands
 use fields,         ONLY:gauge_field
 !
 implicit none
 !
 type(gauge_field), intent(in) :: A_input
 complex(SP),  intent(in)      :: Wk(3,H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 complex(SP),  intent(inout)   :: H_nl_sc(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 !
 ! Work Space
 !
 integer :: i1,i2,id
 logical :: l_selective_coupling
 complex(SP) :: E_vec_pot(3)
 !
 l_selective_coupling=all((/H_ref_hole_bands/=-1,H_ref_elec_bands/=-1/))
 !
 E_vec_pot=-A_input%vecpot_vel/SPEED_OF_LIGHT
 !
 !$omp parallel do default(shared), private(i1,i2,id)
 do i1=H_ref_bands(1),H_ref_bands(2)
   if ( l_selective_coupling .and. i1<H_ref_hole_bands(1) ) cycle
   if ( l_selective_coupling .and. i1>H_ref_hole_bands(2) .and. i1<H_ref_elec_bands(1) ) cycle
   if ( l_selective_coupling .and. i1>H_ref_elec_bands(2) ) cycle
   do i2=H_ref_bands(1),H_ref_bands(2)
     if ( l_selective_coupling .and. i2<H_ref_hole_bands(1) ) cycle
     if ( l_selective_coupling .and. i2>H_ref_hole_bands(2) .and. i2<H_ref_elec_bands(1) ) cycle
     if ( l_selective_coupling .and. i2>H_ref_elec_bands(2) ) cycle
     do id=1,3
       H_nl_sc(i1,i2)=H_nl_sc(i1,i2)+E_vec_pot(id)*Wk(id,i1,i2)
     enddo
   enddo
 enddo
 !$omp end parallel do
 !
end subroutine Apply_W_field
