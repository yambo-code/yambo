!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DV
!
subroutine QP_load_SC_xc() 
 !
 ! Load and Rotate non-local Hamiltonian coming from SC calculation 
 !
 use electrons,       ONLY:spin,n_sp_pol
 use QP_m,            ONLY:QP_table,QP_n_states,QP_vnl_xc
 use IO_m,            ONLY:VERIFY,DUMP,NONE
 use R_lattice,       ONLY:nkibz
 use SC,              ONLY:SC_bands,load_SC_components,compatible_SC_DB,&
&                          found_SC_DB,SC_bands
 use hamiltonian,     ONLY:H_nl_sc,H_rotation
 use interfaces,      ONLY:LINEAR_ALGEBRA_driver
 use linear_algebra,  ONLY:INV
 !
 implicit none
 !
 ! Work Space
 !
 integer  :: i_qp,i_k,i_sp_pol
 !
 ! Load SC_R rotation matrices
 !
 call load_SC_components('WF',COM_=NONE,MODE_=DUMP)
 !
 do i_sp_pol=1,n_sp_pol
   do i_k=1,nkibz
     !
     if (.not.any(QP_table(:,3)==i_k)  ) cycle 
     !
     call load_SC_components('Hsc',COM_=NONE,MODE_=VERIFY,ik=i_k,i_sp_pol=i_sp_pol)
     !
     if (.not.compatible_SC_DB.or..not.found_SC_DB) return
     !
     call LINEAR_ALGEBRA_driver(INV,M=H_rotation(:,:,i_k,i_sp_pol))
     call OBS_rotate(H_rotation(:,:,i_k,i_sp_pol),H_nl_sc,SC_bands(2),-1)
     !
     do i_qp=1,QP_n_states
       if (QP_table(i_qp,1) < SC_bands(1))        cycle
       if (QP_table(i_qp,1) > SC_bands(2))        cycle
       if (.not.QP_table(i_qp,3) ==i_k)           cycle
       if (.not.spin(QP_table(i_qp,:))==i_sp_pol) cycle
       QP_vnl_xc(i_qp)=QP_vnl_xc(i_qp)-H_nl_sc(QP_table(i_qp,1),QP_table(i_qp,1))
     enddo
     !
   enddo
 enddo
 !
 ! Deallocations
 !
 call load_SC_components('WF_clean')
 call load_SC_components('Hsc_clean')
 !
end subroutine
