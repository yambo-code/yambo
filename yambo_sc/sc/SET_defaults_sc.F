!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SET_defaults_sc(INSTR,IND,OD,COM_DIR)
 !
 use pars,       ONLY:SP
 use global_XC,  ONLY:EXT_NONE
 use SC,         ONLY:SC_iterations,SC_rho_threshold,SC_potential,OEP_approx,l_NSC_shot,&
&                     compatible_SC_DB,SC_cycle_mixing,SC_fft_size,found_SC_DB,&
&                     l_Mean_Potential,SC_up_W_iters,l_SC_diagonal,&
&                     l_SC_nl_mix,SC_kind,SC_perturbation,SC_desc,SC_neq_kind,SC_last_coh_it
 use magnetic,    ONLY:MAG_B,MAG_radius,MAG_hamiltonian_type,MAG_landau,MAG_pauli,&
&                      MAG_gauge,MAG_psi,MAG_theta,phase_trick
 use electric,    ONLY:l_P_periodic,l_force_SndOrd
 use descriptors, ONLY:IO_desc_reset
 !
 implicit none
 !
 character(*) :: INSTR,IND,OD,COM_DIR
 !
 call SET_defaults(instr,ind,od,com_dir)
 call SET_defaults_comm(instr,ind,od,com_dir)
 !
 ! SC
 !
 SC_fft_size=0
 SC_iterations=100
 SC_cycle_mixing=0.5_SP
 SC_rho_threshold=1.E-5_SP
 SC_up_W_iters=0
 SC_potential='NONE'
 SC_kind=0
 SC_perturbation=EXT_NONE
 OEP_approx='n'
 compatible_SC_DB=.FALSE.
 found_SC_DB=.FALSE.
 l_Mean_Potential=.FALSE.
 l_SC_diagonal=.FALSE.
 l_SC_nl_mix =.FALSE.
 l_NSC_shot  =.FALSE.
 !
 SC_neq_kind   ='none'
 SC_last_coh_it=50
 !
 call IO_desc_reset(SC_desc)
 !
 l_P_periodic   =.FALSE.
 l_force_SndOrd =.FALSE.
 !
 ! Magnetic
 !
 MAG_B=0._SP
 MAG_radius=0._SP
 MAG_hamiltonian_type=' '
 MAG_pauli=.FALSE.
 MAG_landau=.FALSE.
 MAG_gauge='SYMM'
 MAG_psi=0._SP
 MAG_theta=0._SP
 phase_trick=.FALSE.
 !
end subroutine
