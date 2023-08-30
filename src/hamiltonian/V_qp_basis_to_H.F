!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine V_qp_basis_to_H(ik,i_sp_pol,V,H)
 !
 use electrons,      ONLY:spin
 use pars,           ONLY:SP
 use QP_m,           ONLY:QP_table,QP_n_states
 use hamiltonian,    ONLY:H_ref_bands
 use matrix_operate, ONLY:UP_matrix_index
 use timing_m,       ONLY:timing
 use parallel_m,     ONLY:PAR_IND_QP,HEAD_QP_cpu,PAR_IND_B_mat_ordered,PAR_IND_WF_k,HEAD_QP_cpu,l_par_SC,l_par_RT
 implicit none
 !
 integer                 :: ik,i_sp_pol
 complex(SP),intent(in)  :: V(QP_n_states)
 complex(SP),intent(out) :: H(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 !
 ! Work Space
 !
 integer    :: i1,ib,ibp
 !
 ! Fill ...
 !
 if (l_par_RT) then
   if (.not.PAR_IND_WF_k%element_1D(ik) .or. .not.HEAD_QP_cpu) return
 endif
 !
 call timing('V_qp_basis_to_H',OPR='start')
 !
 do i1=1,QP_n_states
   !
   if (     QP_table(i1,3) /=ik      ) cycle
   if (spin(QP_table(i1,:))/=i_sp_pol) cycle
   !
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   !
   if(ib>ibp) cycle
   !
   if (.not.HEAD_QP_cpu) cycle
   if (l_par_SC) then
     if (.not.(PAR_IND_QP%element_1D(i1))) cycle
   endif
   if (l_par_RT) then
     if (.not.PAR_IND_B_mat_ordered%element_1D( UP_matrix_index(ib-H_ref_bands(1)+1,ibp-H_ref_bands(1)+1)-1 )) cycle
   endif
   !
   ! In the RT simulation the bands QP_table matrix is not ordered
   ! when the carriers Couloumb scattering is calculated.
   ! As a consequence the H matrix is not correctly imposed to be Hermitian
   ! and the dynamics does not work.
   !
   H(ib,ibp) = H(ib,ibp) + V(i1)
   !
   ! Force to be Hermitian
   !
   if (ib/=ibp) H(ibp,ib) = conjg(H(ib,ibp))
   !
 end do
 !
 call timing('V_qp_basis_to_H',OPR='stop')
 !
end subroutine 
