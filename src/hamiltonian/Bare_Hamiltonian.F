!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): DV DS
!
subroutine Bare_Hamiltonian(E,Xk,k)
 !
 use pars,           ONLY:SP,cZERO
 use electrons,      ONLY:levels,n_sp_pol,spin,n_spin
 use QP_m,           ONLY:QP_nk,QP_nb,QP_Vxc,QP_n_states,QP_table
 use hamiltonian,    ONLY:Hzero,E_reference,V_hartree_sc,rho_n,magn_n,V_xc_sc,&
&                         H_ref_bands,WF_G_max,WF_Go_indx
 use parallel_m,     ONLY:PP_indexes
 use parallel_m,     ONLY:PAR_IND_WF_k,PAR_IND_WF_linear
 use parallel_m,     ONLY:PAR_IND_B_mat_ordered,PAR_IND_WF_k 
 use matrix_operate, ONLY:UP_matrix_index,matrix_index
 use parallel_int,   ONLY:PP_redux_wait
 use wave_func,      ONLY:WF
 use R_lattice,      ONLY:bz_samp,nkibz
 use xc_functionals, ONLY:V_xc
 use interfaces,     ONLY:el_density_and_current,el_magnetization,WF_load
 use H_interfaces,   ONLY:V_real_space_to_H
 use timing_m,       ONLY:timing
 use drivers,        ONLY:l_sc_run
 use drivers,        ONLY:l_real_time
 use drivers,        ONLY:l_sc_magnetic
 !
#include<y_memory.h>
 !
 type(levels)             :: E
 type(bz_samp)            :: Xk,k
 !
 ! Work space
 !
 integer          :: i1,ik,ib,ibp,i_sp_pol,WFbands(2)
 complex(SP), allocatable  :: H_pseudo(:,:,:,:)
 type(PP_indexes) :: PAR_IND
 !
 Hzero=cZERO
 !
 call timing('Bare_Hamiltonian',OPR='start')
 !
 if (l_sc_magnetic) then
   !
   ! MAG_Hamiltonian
   !=================
   ! - call WF_derivative which computes the derivative in space G
   ! - load the wf in real space
   ! - compute the magnetic part of the hamiltonian, 
   !   allocate and initialise Hzero, write H_magnetic in Hzero 
   !
   call MAG_Hamiltonian()
   !
   ! If exist the non local part of the pseudo then construct kbv
   !
   call Pseudo_KB_G_to_R_space(k,E,.false.)
   allocate(H_pseudo(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2),k%nibz,n_sp_pol))
   call Pseudo_Hamiltonian(Xk,E,H_pseudo,.true.)
   Hzero=Hzero-H_pseudo
   call Pseudo_Hamiltonian(Xk,E,H_pseudo,.false.)
   Hzero=Hzero+H_pseudo
   deallocate(H_pseudo)
   !
 endif
 !
 ! WF Loading, density and magnetization
 !=======================================
 WFbands=(/H_ref_bands(1),max(H_ref_bands(2),maxval(E%nbm))/)
 call WF_load(WF,WF_G_max,WF_Go_indx,WFbands,(/1,nkibz/),title='-SC')
 !
 if(l_sc_run) then
   call el_density_and_current(E,Xk,rho=rho_n,bands=H_ref_bands)
   if(n_spin>1) call el_magnetization(E,Xk,magn_n,bands=H_ref_bands)
 endif
 !
 ! The Hzero = -nabla**2. + V_ext is deduced from the input energies en0:
 !
 ! H = Hzero + v_hxc[rho_in] and
 ! H psi_0(n) = en0(n) psi_0(n)
 !
 ! then Hzero_n,m = en0(n) delta_n,m - v_hxc[rho_0]_n,m
 !
 ! where the matrix elements are calculated from the  psi_0(n)
 !
 ! ... so first store -Vhartree in Hzero ...
 !========================================
 !
 ! V_hartree
 call V_Hartree(rho_n,V_hartree_sc)
 !
 ! QP_Vxc
 YAMBO_ALLOC(QP_Vxc,(QP_n_states))
 QP_Vxc=cZERO
 !
 !! In case a non-DFT potential is required
 !!
 !! ...SC EXX and EXX+LDA_C
 !! 
 !if (mod(WF_kind,1000)>=300) then
 !  !
 !  ! Here I need to check which other actions from XCo_local are needed
 !  call error(" part to be fixed ")
 !  !
 !  !AMBO_ALLOC(V_xc_sc,(fft_size,n_spin))
 !  !AMBO_ALLOC(V_hartree_sc,(fft_size,n_spin))
 !  SC_fft_size=fft_size
 !  !
 !  call load_SC_components('V',COM_=REP,MODE_=VERIFY)
 !  !
 !  if (.not.compatible_SC_DB) call error('Incompatible potential database')
 !  !
 !  V_xc=V_xc_sc
 !  !
 !  !AMBO_FREE(V_xc_sc)
 !  !AMBO_FREE(V_hartree_sc)
 !  !
 !endif
 !
 allocate(PAR_IND%element_2D(QP_nb,QP_nk))
 !
 ! TMP PAR_IND
 !
 if (l_real_time) then
   do i1=1,QP_n_states
     ib =QP_table(i1,1)
     ibp=QP_table(i1,2)
     ik =QP_table(i1,3)
     PAR_IND%element_2D(ib,ik)=PAR_IND_WF_k%element_1D(ik) .and. &
     &  PAR_IND_B_mat_ordered%element_1D( UP_matrix_index(ib-H_ref_bands(1)+1,ibp-H_ref_bands(1)+1)-1 ) 
   enddo
 else
   PAR_IND%element_2D=PAR_IND_WF_linear%element_2D
 endif
 !
 call XCo_local(E,k,PAR_IND,l_real_time,.false.)
 !
 V_xc_sc=V_xc
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,QP_nk
     !
     ! Add bare energies ... 
     !=======================
     !
     do ib=H_ref_bands(1),H_ref_bands(2)
       if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
       Hzero(ib,ib,ik,i_sp_pol) = Hzero(ib,ib,ik,i_sp_pol) + E_reference%E(ib,ik,i_sp_pol)  
     enddo
     !
     if(l_real_time) then
       if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
     endif
     !
     ! ... then remove Vh[rho_0] 
     !===========================
     !
     call V_real_space_to_H(ik,i_sp_pol,Hzero(:,:,ik,i_sp_pol),WF,'def',V=-V_hartree_sc)
     !
     ! ... then remove Vxc[rho_0] also
     !=================================
     !
     call V_qp_basis_to_H(ik,i_sp_pol,-QP_Vxc,Hzero(:,:,ik,i_sp_pol))
     !
   enddo
   !
 enddo
 !
 YAMBO_FREE(QP_Vxc)
 !
 call PP_redux_wait(Hzero)
 !
 call timing('Bare_Hamiltonian',OPR='stop')
 !
end subroutine Bare_Hamiltonian
