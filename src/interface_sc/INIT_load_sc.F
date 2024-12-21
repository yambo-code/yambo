!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_load_sc(defs,en,q,k,X,Xw,Dip)
 !
 use drivers,       ONLY:infile_editing
 use pars,          ONLY:schlen,SP
 use electrons,     ONLY:levels
 use frequency,     ONLY:w_samp
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,initmode,V_sc,V_qp,V_parallel,&
 &                       E_unit,Angle_unit,Bfield_unit
 use X_m,           ONLY:X_t
 use DIPOLES,       ONLY:DIPOLE_t
 use R_lattice,     ONLY:bz_samp
 use hamiltonian,   ONLY:H_potential
 use collision_ext, ONLY:COLLISIONS_cutoff,COLL_bands
 use QP_m,          ONLY:SC_E_threshold
 use QP_m,          ONLY:SC_band_mixing
 use SC,            ONLY:SC_iterations,SC_up_W_iters,&
&                        SC_rho_threshold,SC_cycle_mixing,&
&                        SC_bands,SC_preconditioning,OEP_approx,&
&                        SC_neq_kind,SC_mu,SC_last_coh_it
 use fields,        ONLY:Efield
 use magnetic,      ONLY:MAG_B,MAG_hamiltonian_type,MAG_radius,MAG_gauge,MAG_psi,MAG_theta
 !
 implicit none
 !
 type(initdefs)::defs
 type(levels)  ::en          
 type(bz_samp) ::q,k
 type(X_t)     ::X(5)
 type(DIPOLE_t)::Dip
 type(w_samp)  ::Xw(5)
 !
 ! Work Space
 !
 real(SP) :: IO_times(3)
 logical  :: l_USER_fields(16)
 !
 ! RunLevels...
 !==============
 !
 call INIT_load(defs,en,q,k,X,Xw,Dip)
 !
 ! ... list of USER fields 
 call LEVELS_mirror_sc("USER_fields",en,X,Dip,l_USER_fields)
 !
 call it(defs,'Hamiltonian',   '[MAG] Hamiltonian kind [pauli,landau,all]',MAG_hamiltonian_type,protect=.FALSE.)
 call it(defs,'B_Field',       '[MAG] Magnetic field modulus',MAG_B,unit=Bfield_unit)
 call it(defs,'B_psi',         '[MAG] Magnetic field psi angle',MAG_psi,unit=Angle_unit(1),verb_level=V_sc)
 call it(defs,'B_theta',       '[MAG] Magnetic field theta angle',MAG_theta,unit=Angle_unit(1),verb_level=V_sc)
 call it(defs,'B_Gauge',       '[MAG] Gauge ("SYMM"etric, "X_ASYMM", "Y_ASYMM" or "Z_ASYMM"etric)',MAG_gauge,verb_level=V_sc)
 call it('f',defs,'PhaseTrick','[MAG] Phase trick for a better diagonalization',verb_level=V_sc)
 call it(defs,'B_radius',      '[MAG] Magnetic flux radius',MAG_radius,verb_level=V_sc)
 !
 ! DIPOLES
 !
#if defined _ELECTRIC
 call it('f',defs,'EvPolarization','[DIP] Evaluate Polarization (require DipApproach=Covariant)',verb_level=V_resp)
 call it('f',defs,'FrPolPerdic','[DIP] Force periodicity of polarization respect to the external field')
#endif
 !
 ! SC
 !
 call it(defs,'SCBands',   '[SC] Bands',SC_bands)
 !
 call it(defs,'SCIter',    '[SC] SC Iterations',SC_iterations)
 call it(defs,'SCRhoTresh','[SC] Rho convergence threshold',SC_rho_threshold)
 call it(defs,'SC_precondition','[SC] Kind of preconditionin: thomas-fermi, simple, none',SC_preconditioning,verb_level=V_sc)
 call it(defs,'SCUpWIter', "[SC] Update W(q,G,G') every SCUpWIter iteractions",SC_up_W_iters,verb_level=V_qp) 
 call it('f',defs,'Mean_Potential','[SC] Real-space Mean Potential',verb_level=V_sc)
 call it('f',defs,'SCnlMix','[SC] Use SC non-local mixing',verb_level=V_sc)
 call it('f',defs,'FrozeDensity','[NL] Do not update density (for testing purposes)',verb_level=V_sc)
 !
 ! SC [NEQ]
 !
 call it(defs,'SCneqKIND',  '[SC] Options are [contrained-occ/constrained-mu/matsubara]',SC_neq_kind,verb_level=V_sc)
 call it(defs,'SCmu',       '[SC] Reference / holes / electrons chem potential',SC_mu,unit=E_unit,verb_level=V_sc)
 call it(defs,'SCcohIt',    '[SC] Impose off-diagonal rho in the initial basis set for N iterations',SC_last_coh_it,verb_level=V_sc)
 ! 
 ! SC [common with RT]
 !
 call it(defs,'BandMix',   '[SC] Band mixing',SC_band_mixing)
 call it(defs,'SCmixing',  '[SC] SC Cycle Mixing (< 1.)',SC_cycle_mixing,verb_level=V_sc)
 call it('f',defs,'SCdiag','[SC] Diagonal approximation for the self-energy(WF unchaged)',verb_level=V_sc)
 ! 
 call it(defs,'SCEtresh',  '[SC] Energy convergence threshold for SC-GW',SC_E_threshold,unit=E_unit,verb_level=V_qp)
 !
 ! SC and RT common
 !
 call it(defs,'COLLBands',   '[COLL] Bands for the collisions',COLL_bands)
 call it(defs,'HXC_Potential', '[SC] SC HXC Potential',H_potential,protect=.FALSE.,case="A")
 call it(defs,'COLLCut',    '[SC,RT] Cutoff on the collisions, 0=full 1=none',COLLISIONS_cutoff,verb_level=V_sc)
 !
 call it('f',defs,'OEPItSolver','[SC] Iterative solution instead of inversion of OEP')
 call it(defs,'OEPapprox','[SC] OEP approximation: n=none s=Slater k=KLI c=CED +w=Weighted',OEP_approx)
 !
 if (initmode==4) then
   call LEVELS_mirror_sc("MIRROR_q",en,X,Dip,l_USER_fields)
   call LEVELS_mirror_sc("MIRROR_bands",en,X,Dip,l_USER_fields)
 endif
 ! 
end subroutine
