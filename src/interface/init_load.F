!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine init_load(defs,en,q,k,X,Xw)
 !
 use pars,          ONLY:schlen
 use electrons,     ONLY:levels,nel,filled_tresh
 use frequency,     ONLY:w_samp
 use it_m,          ONLY:it,initdefs,E_unit,G_unit,T_unit,Bfield_unit,&
&                        Time_unit,I_unit,Angle_unit,&
&                        V_RL,V_kpt,V_sc,V_qp,V_io,V_general,V_resp,V_real_time
 use X_m,           ONLY:X_t,q_plus_G_direction,Q_Shift_Order
 use fields,        ONLY:global_gauge,grid_path
 use QP_m,          ONLY:QP_cg_percent,QP_G_damp,QP_solver,&
&                        QP_n_G_bands,QP_ng_Sx,GW_en_comp,&
&                        QP_G_er,QP_G_dr,QP_Sc_steps,GWo_iterations,SC_band_mixing,&
&                        SC_E_threshold,QP_dSc_delta,QP_G_Zoom_treshold,OCC_T_ref
 use timing,        ONLY:nhash
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:Tel,non_periodic_directions,Bose_Temp
 use R_lattice,     ONLY:ng_closed,n_g_shells,QP_states_k,nXkibz,&
&                        bz_samp,RIM_ng,RIM_epsm1,RIM_id_epsm1_reference,&
&                        RIM_n_rand_pts,cyl_ph_radius,box_length,cyl_length,cut_geometry
 use BS,            ONLY:BSE_mode,BS_eh_en,BS_eh_win,BS_q,BS_bands,BS_columns,&
&                        BS_n_g_W,BS_n_g_exch,BSS_mode,Haydock_treshold,BSS_n_freqs,&
&                        BSS_dr,BSS_er,BSS_q0,BSS_damp_reference
 use TDDFT,         ONLY:FXC_n_g_corr,FXC_per_memstps,FXC_LRC_alpha,FXC_LRC_beta, &
&                        FXC_SVD_digits
 use ACFDT,         ONLY:ACFDT_n_lambda,ACFDT_n_freqs,ACFDT_E_range
 use functions,     ONLY:bose_E_cut
#if defined _SC
 use SC,            ONLY:SC_bands,SC_iterations,SC_potential,SC_up_W_iters,&
&                        SC_rho_threshold,SC_cycle_mixing,OEP_approx
 use collisions_IO, ONLY:COLLISIONS_cutoff,COLLISIONS_path
 use collisions_CACHE, ONLY:COLLISIONS_cache_size
#endif
#if defined _RT
 use real_time,     ONLY:RT_step, NE_steps, Thermal_steps, Integrator_name, RAD_LifeTime, Phase_LifeTime,  &
&                        RT_diagonalization_steps,SwitchOnTime,max_rt_steps_disk,NE_tot_time,NE_Hxc_jumps,&
&                        RT_ave_dephasing,NE_MEM_treshold
 use fields,        ONLY:Efield
#endif
#if defined _MAGNETIC
 use magnetic,      ONLY:MAG_B,MAG_hamiltonian_type,MAG_radius,MAG_gauge,MAG_psi,MAG_theta
#endif
#if defined _ELPH 
 use ELPH,                 ONLY:QP_PH_n_G_bands,elph_nDBs_used,elph_Ham_ik,elph_Ham_bands,elph_branches
#endif
 !
 implicit none
 type(initdefs)::defs
 type(levels)  ::en          
 type(bz_samp) ::q,k
 type(X_t)     ::X(4)
 type(w_samp)  ::Xw(4)
 !
 !===============
 ! GPL RUNLEVELS
 !===============
 call it('r',defs,'setup',   '[R INI] Initialization') 
 call it('r',defs,'optics',  '[R OPT] Optics')
 call it('r',defs,'chi',     '[R CHI] Dyson equation for Chi.')
 call it('r',defs,'bse',     '[R BSE] Bethe Salpeter Equation.')
 call it('r',defs,'IP',      '[R   K] Independet Particles')
 call it('r',defs,'tdh',     '[R   K] TD Hartree kernel = Local fields term')
 call it('r',defs,'tddft',   '[R   K] Use TDDFT kernel')
 call it('r',defs,'alda_fxc','[R   K]     Adiabatic-LDA Fxc')
 call it('r',defs,'lrc_fxc', '[R   K]     LRC (1/alpha) Fxc')
 call it('r',defs,'tdhf',    '[R BSK] Unscreened e-h interaction: TD Hartree-Fock kernel')
 call it('r',defs,'W_eh',    '[R BSK]   screened e-h interaction: W kernel')
 call it('r',defs,'bss',     '[R BSS] Bethe Salpeter Equation solver')
 call it('r',defs,'em1s',    '[R Xs] Static Inverse Dielectric Matrix')
 call it('r',defs,'em1d',    '[R Xd] Dynamical Inverse Dielectric Matrix')
 call it('r',defs,'ppa',     '[R Xp] Plasmon Pole Approximation')
 call it('r',defs,'HF_and_locXC',   '[R XX] Hartree-Fock Self-energy and Vxc')
 call it('r',defs,'gw0',     '[R GW] GoWo Quasiparticle energy levels')
 call it('r',defs,'life',    '[R GW] GoWo Quasiparticle lifetimes')
 call it('r',defs,'rim_cut', '[R RIM CUT] Coulomb interaction')
 call it('r',defs,'cohsex',  '[R Xp] COlumb Hole Screened EXchange')
 !
 ! GPL_EXCLUDE_START
 !
 !===================
 ! NON GPL RUNLEVELS
 !===================
 call it('r',defs,'acfdt',   '[R ETOT] ACFDT Total Energy')
 call it('r',defs,'bs_fxc',  '[R TDDFT] The BSE TDDFT kernel')
#if defined _ELPH 
 call it('r',defs,'ElPhHam', '[R] Frohlich Hamiltonian') 
#endif
 !
 ! GPL_EXCLUDE_END
 !
#if defined _SC 
 call it('r',defs,'scpot',   '[R] Self-Consistent potentials') 
#endif
#if defined _RT
 call it('r',defs,'collisions_IO',   '[R] Eval the extended Collisions')
 call it('r',defs,'negf',            '[R] Real-Time dynamics') 
#endif
#if defined _MAGNETIC
 call it('r',defs,'magnetic','[R] Magnetic fields')
#endif
#if defined _ELPH 
 call it('r',defs,'el_ph',   '[R] Electron-Phonon Correlation')   
 call it('r',defs,'el_el',   '[R] Electron-Electron Correlation') 
#endif
 !
 !================
 ! GPL VARIABLES
 !================
 !
 ! Basics
 !
 call it(defs,'StdoHash','[IO] Live-timing Hashes',nhash,verb_level=V_io)
 call it(defs,'MaxGvecs','[INI] Max number of G-vectors planned to use',ng_closed,unit=G_unit,verb_level=V_RL)
 call it(defs,'FFTGvecs','[FFT] Plane-waves',wf_ng,G_unit,verb_level=V_RL) 
 call it(defs,'NonPDirs','[X/BSS] Non periodic chartesian directions (X,Y,Z,XY...)',non_periodic_directions,verb_level=V_resp)
 call it(defs,'IkSigLim','[KPT] QP K-points indices range',QP_states_k,verb_level=V_kpt)
 call it(defs,'IkXLim',  '[KPT] X grid last k-point index',nXkibz,verb_level=V_kpt)
 call it(defs,'Nelectro','Electrons number',nel,verb_level=V_general)
 call it(defs,'ElecTemp','Electronic Temperature',Tel,T_unit,verb_level=V_general)
 call it(defs,'OccTresh','Occupation treshold (metallic bands)',filled_tresh,verb_level=V_general)
 call it(defs,'BoseTemp','Bosonic Temperature',Bose_Temp,T_unit,verb_level=V_general)
 call it(defs,'BoseCut', '[BOSE] Finite T Bose function cutoff',bose_E_cut,verb_level=V_general)
#if defined _ELPH 
 call it('f',defs,'MinusQ', '[KPT] Use -{q} grid',verb_level=V_kpt)
 call it(defs,'ElPhRndNq',  '[ELPH] Read random Q-points',elph_nDBs_used,verb_level=V_kpt) 
#endif
 !
 ! S_xc
 !
 call it(defs,'LifeTrCG', '[GW] [o/o] Lifetime transitions reduction',QP_cg_percent)
 call it(defs,'EXXRLvcs', '[XX] Exchange RL components',QP_ng_Sx,G_unit)
 call it(defs,'GbndRnge', '[GW] G[W] bands range',QP_n_G_bands)
 call it(defs,'GDamping', '[GW] G[W] damping',QP_G_damp,E_unit)
 call it(defs,'GDmRnge',  '[GW] G_gw damping range',QP_G_dr,E_unit)
 call it(defs,'dScStep',  '[GW] Energy step to evalute Z factors',QP_dSc_delta,E_unit)
 call it(defs,'DysSolver','[GW] Dyson Equation solver (`n`,`s`,`g`)',QP_solver,protect=.FALSE.)
 call it(defs,'GEnSteps',   '[GW] G_gw energy steps',QP_Sc_steps)
 call it(defs,'GEnRnge',    '[GW] G_gw energy range (centered in the bare energy)',QP_G_er,E_unit)
 call it('f',defs,'GWTerm',   '[GW] Use a terminator for the self-energy',verb_level=V_qp)
 call it(defs,'GwEnComp',   '[GW] If <= 0 use 0-order EET ; > 0 the Bruneval-Gonze terminator', &
&                        GW_en_comp,E_unit,verb_level=V_qp)
 call it('f',defs,'NewtDchk',   '[F GW] Test dSc/dw convergence',verb_level=V_qp)
 call it('f',defs,'ExtendOut',  '[F GW] Print all variables in the output file',verb_level=V_qp)
 !
 ! BSE
 !
 call it(defs,'BSEmode', '[BSE] resonant/causal/coupling',BSE_mode)
 call it(defs,'BSEBands','[BSK] Bands range',BS_bands)
 call it(defs,'BSENGBlk','[BSK] Screened interaction block size',BS_n_g_W,G_unit)
 call it(defs,'BSENGexx','[BSK] Exchange components',BS_n_g_exch,G_unit)
 call it(defs,'BSEEhEny','[BSK] Electron-hole energy range',BS_eh_en,E_unit,verb_level=V_resp)
 call it(defs,'BSSmod',  '[BSS] Solvers `h/d/i/t`',BSS_mode,protect=.FALSE.)
 call it(defs,'BLongDir','[BSS] [cc] Electric Field',BSS_q0)
 call it(defs,'BEnRange','[BSS] Energy range',BSS_er,E_unit)
 call it(defs,'BDmRange','[BSS] Damping range',BSS_dr,E_unit)
 call it(defs,'BSHayTrs','[BSS] [o/o] Haydock treshold. Strict(>0)/Average(<0)',&
&              Haydock_treshold,verb_level=V_resp)
 call it(defs,'BEnSteps','[BSS] Energy steps',BSS_n_freqs)
 call it('f',defs,'Wehdiag',  '[BSK] the eh interaction is diagonal',verb_level=V_resp)
 call it('f',defs,'WRbsWF',  '[BSS] Write to disk excitonic the FWs',verb_level=V_resp)
 call it('f',defs,'ALLGexx', '[BSS] Force the use use all RL vectors for the exchange part',verb_level=V_resp)
 call it('f',defs,'BSHayTer','[BSS] Terminate Haydock continuos fraction',verb_level=V_resp)
 !
 ! F_xc
 !
 call it(defs,'FxcGRLc',  '[TDDFT] XC-kernel RL size',FXC_n_g_corr,G_unit)
 call it(defs,'LRC_alpha','[TDDFT] LRC alpha factor',FXC_LRC_alpha)
 call it(defs,'LRC_beta', '[TDDFT] LRC beta factor',FXC_LRC_beta,verb_level=V_resp)
 !
 ! Xs Xd Xp
 !
 call Xload(X(2),Xw(2))
 call Xload(X(3),Xw(3))
 call Xload(X(4),Xw(4))
 !
 ! Optics: large Q momenta
 !
 call it(defs,'Qdirection', '[Xd] Transferred momentum direction (iku)',q_plus_G_direction,verb_level=V_resp)
 call it(defs,'QShiftOrder','[Xd] Pick-up the (QShiftOrder)th q+G vector',Q_Shift_Order,verb_level=V_resp)
 !
 ! RIM
 !
 call it(defs,'Em1Anys', '[RIM] X Y Z Static Inverse dielectric matrix',&
&                         RIM_epsm1,verb_level=V_RL)
 call it(defs,'IDEm1Ref','[RIM] Dielectric matrix reference component 1(x)/2(y)/3(z)',&
&                         RIM_id_epsm1_reference,verb_level=V_RL)
 call it(defs,'RandQpts','[RIM] Number of random q-points in the BZ',RIM_n_rand_pts)
 call it(defs,'RandGvec','[RIM] Coulomb interaction RS components',RIM_ng,G_unit)
 call it('f',defs,'QpgFull', '[F RIM] Coulomb interaction: Full matrix',verb_level=V_RL)
 !
 ! CUTOFF
 !
 call it(defs,'CUTGeo',   '[CUT] Coulomb Cutoff geometry: box/cylinder/sphere',cut_geometry)
 call it(defs,'CUTBox',   '[CUT] [au] Box sides',box_length)
 call it(defs,'CUTRadius','[CUT] [au] Sphere/Cylinder radius',cyl_ph_radius)
 call it(defs,'CUTCylLen','[CUT] [au] Cylinder length',cyl_length)
 call it('f',defs,'CUTCol_test','[CUT] Perform a cutoff test in R-space',verb_level=V_RL)
 !
 ! GPL_EXCLUDE_START
 !
 !===================
 ! NON GPL VARIABLES
 !===================
 !
 ! S_xc
 !
 call it(defs,'GWoIter',    '[GW] GWo SC iterations',GWo_iterations,verb_level=V_qp)
 call it(defs,'GreenFTresh','[GW] [o/o] Treshold to define the new zoomed energy range',QP_G_Zoom_treshold,verb_level=V_qp)
 call it('f',defs,'OnMassShell','[F GW] On mass shell approximation',verb_level=V_qp)
 call it('f',defs,'GreenF2QP',  '[F GW] Use real axis Green`s function to define the QPs',verb_level=V_qp)
 !
 ! GPL_EXCLUDE_END
 !
 ! GPL_EXCLUDE_START SC_IGNORE
 !
 call it('f',defs,'UseEbands', '[GW] Force COHSEX to use empty bands',verb_level=V_qp)
 !
 ! Real Time dynamics
 !
#if defined _RT
 call it(defs,'RTstep',    '[RT] Real Time step length',RT_step,unit=Time_unit)
 call it(defs,'NEsteps',   '[RT] Non-equilibrium Time steps',NE_steps)
 call it(defs,'MemTresh',  '[RT] Treshold on the decay of the retarded GF',NE_MEM_Treshold)
 call it(defs,'HXCjumps',  '[RT] Steps jumped between two Hartree+XC updates',NE_Hxc_jumps,Verb_level=V_real_time)
 call it(defs,'NETime',    '[RT] Non-equilibrium  max Time (alternative to NEsteps)',NE_tot_time,unit=Time_unit)
 call it(defs,'ThermSteps','[RT Therm] Thermal Steps',Thermal_steps,Verb_level=V_real_time)
 call it(defs,'Integrator',   '[RT] Integrator (RK2 | EULER | EULEREXP | RK2EXP ) ',Integrator_name)
 call it(defs,'RADLifeTime',  '[RT] Radiative life-time',RAD_LifeTime,unit=Time_unit)
 call it(defs,'PhLifeTime',   '[RT] Phase Relaxation Time',Phase_LifeTime,unit=Time_unit)
 call it('f',defs,'RTfreezeH', '[RT] Hartree not updated during RT')
 call it('f',defs,'RTfreezeXC','[RT] XC potential/self-energy not updated during RT')
 !
 ! Other parameters of RT (to be switched on using the verbosity)
 !
 call it(defs,'SwitchOnTime', '[RT Therm] Adiabatic switch on time ',SwitchOnTime,verb_level=V_real_time,unit=Time_unit)
 call it(defs,'RTDiagoSteps', '[RT] Digonalize the Hamiltonian',RT_diagonalization_steps,verb_level=V_real_time)
 call it(defs,'RTAveDeph','[RT] Dephasing for all elements not included in RTDePhMatrix' &
&                        ,RT_ave_dephasing,verb_level=V_real_time,unit=Time_unit)
 call it('f',defs,'Diamagnetic', '[RT] Include the diamagnetic term in current and coupling',verb_level=V_real_time)
 call it('f',defs,'InducedField', '[RT] Include induced field in coupling and current',verb_level=V_real_time)
 call it(defs,'DiskSteps',     '[RT] Numer of steps stored in memory before I/O',max_rt_steps_disk,verb_level=V_real_time)
 !
 ! External fields
 !
 call Afield(1) ! Probe
 call Afield(2) ! Pump
 !
#endif
 !
#if defined _MAGNETIC
 call it(defs,'Hamiltonian',   '[MAG] Hamiltonian kind [pauli,landau]',MAG_hamiltonian_type,protect=.FALSE.)
 call it(defs,'B_Field',       '[MAG] Magnetic field modulus',MAG_B,unit=Bfield_unit)
 call it(defs,'B_psi',         '[MAG] Magnetic field psi angle',MAG_psi,unit=Angle_unit,verb_level=V_sc)
 call it(defs,'B_theta',       '[MAG] Magnetic field theta angle',MAG_theta,unit=Angle_unit,verb_level=V_sc)
 call it(defs,'B_Gauge',       '[MAG] Gauge ("SYMM"etric, "X_ASYMM", "Y_ASYMM" or "Z_ASYMM"etric)',MAG_gauge,verb_level=V_sc)
 call it('f',defs,'PhaseTrick','[MAG] Phase trick for a better diagonalization',verb_level=V_sc)
 call it(defs,'B_radius',      '[MAG] Magnetic flux radius',MAG_radius,verb_level=V_sc)
#endif
 !
 ! GPL_EXCLUDE_END SC_IGNORE
 !
 ! QP ctl 
 !
 call QP_ctl_load(defs,1)
 call QP_ctl_load(defs,2)
 call QP_ctl_load(defs,3)
 !
 ! GPL_EXCLUDE_START
 !
 ! Occupations (NEGF) loader
 !
 call it(defs,'OCCTime','NEGF Occupations loading time',OCC_T_ref,verb_level=V_real_time,unit=Time_unit)
 !
 ! F_xc
 !
 call it(defs,'FxcMEStps','[TDDFT] [o/o] Memory energy steps',&
&                         FXC_per_memstps,verb_level=V_resp)
 call it(defs,'FxcSVdig','[TDDFT] Keep SV that are within FxcSVdig digits',&
&                         FXC_SVD_digits,verb_level=V_resp)
 call it('f',defs,'FxcCausal', '[TDDFT] Causal TDDFT kernel',verb_level=V_resp)
 !
 ! BSE
 !
 call it(defs,'BSehWind','[BSK] [o/o] E/h coupling pairs energy window',&
&        BS_eh_win,verb_level=V_resp)
 call it(defs,'BSEQptR', '[BSK] Transferred momenta range',BS_q,verb_level=V_resp)
 call it(defs,'BSEClmns','[BSK] Kernel Columns',BS_columns,verb_level=V_resp)
 call it(defs,'BDmERef', '[BSS] Damping energy reference',BSS_damp_reference,&
&                        E_unit,verb_level=V_resp)
 call it('f',defs,'SkipFullInv',  '[BSS] Skip full invertion in perturbative solver',verb_level=V_resp)
 call it(defs,'DrudeW',  '[BSE] Drude plasmon',X(2)%Wd,E_unit,verb_level=V_resp)
 !
 ! Xx
 !
 call Xload(X(1),Xw(1))
 !
 ! ACFDT
 !
 call it(defs,'AC_n_LAM', '[ACFDT] Coupling Costant GL-grid points',ACFDT_n_lambda)
 call it(defs,'AC_n_FR',  '[ACFDT] Integration frequency points',ACFDT_n_freqs)
 call it(defs,'AC_E_Rng', '[ACFDT] Imaginary axis 2nd & 3rd energy points',ACFDT_E_range,E_unit)
 !
 ! Optics
 !
 call it(defs,'ShiftedPath',    '[Xd] Shifted grids path',grid_path,verb_level=V_resp)
 !
 ! GPL_EXCLUDE_END
 !
 ! El-Ph
 !
#if defined _ELPH 
 call it(defs,'GphBRnge',  '[ELPH] G[W] bands range',QP_PH_n_G_bands)
 call it(defs,'ElPhModes', '[ELPH] Phonon modes included',elph_branches)
 !
 ! GPL_EXCLUDE_START
 !
 call it(defs,'ElPhHBRnge','[ELPH] Hamiltonian bands range',elph_Ham_bands)
 call it(defs,'ElPhHKpt',  '[ELPH] Hamiltonian k-point',elph_Ham_ik)
 !
 ! GPL_EXCLUDE_END
 !
 call it('f',defs,'WRgFsq','[ELPH] Dump on file gFsq coefficients')
#endif
 ! 
 ! SC
 !
#if defined _SC 
 call it(defs,'SCIter',    '[SC] SC Iterations',SC_iterations)
 call it(defs,'SCEtresh',  '[SC] Energy convergence threshold',SC_E_threshold,unit=E_unit)
 call it(defs,'SCRhoTresh','[SC] Rho convergence threshold',SC_rho_threshold)
 call it('f',defs,'TF_precondition','[SC] Thomas Fermi precondition in density',verb_level=V_sc)
 call it(defs,'SCUpWIter', "[SC] Update W(q,G,G') every SCUpWIter iteractions",SC_up_W_iters) 
 call it('f',defs,'MeanPotential','[SC] Real-space Mean Potential',verb_level=V_sc)
 call it('f',defs,'SCnlMix','[SC] Use SC non-local mixing',verb_level=V_sc)
 ! 
 ! SC [common with RT]
 !
 call it(defs,'Gauge',     '[Xd,RT] Gauge type (length|velocity)',global_gauge,verb_level=V_real_time)
 call it(defs,'SCBands',   '[SC] Bands',SC_bands)
 call it(defs,'SCmixing',  '[SC] SC Cycle Mixing (< 1.)',SC_cycle_mixing,verb_level=V_sc)
 call it(defs,'Potential', '[SC] SC Potential',SC_potential,protect=.FALSE.)
 call it(defs,'BandMix',   '[SC] Band mixing',SC_band_mixing)
 call it(defs,'EXXCut',    '[SC,RT] Cutoff on the exchange, 0=full 1=none',COLLISIONS_cutoff,verb_level=V_sc)
 call it(defs,'CollCache', '[SC,RT] Cache[Gb] for collisions (negative value = automatic)',COLLISIONS_cache_size,verb_level=V_sc)
 call it(defs,'CollPath',  '[SC,RT] Path to the collisions databases',COLLISIONS_path)
 call it('f',defs,'OEPItSolver','[SC] Iterative solution instead of inversion of OEP')
 call it(defs,'OEPapprox','[SC] OEP approximation: n=none s=Slater k=KLI c=CED +w=Weighted',OEP_approx)
 call it('f',defs,'SCdiag','[SC] Diagonal approximation for the self-energy(WF unchaged)',verb_level=V_sc)
#endif
 ! 
 contains 
   !
#if defined _RT
   !
   subroutine Afield(i_field)
     character(5) :: name
     integer      :: verbosity_here,i_field
     verbosity_here=0
     if (i_field==1) name='Probe'
     if (i_field==2)  name='Pump'
     if (i_field==2) verbosity_here=V_real_time
     call it(defs,trim(name)//'_Freq',            '[RT '//trim(name)//'] Frequency',&
&    Efield(i_field)%frequency,unit=E_unit)
     call it(defs,trim(name)//'_FrStep',          '[RT '//trim(name)//'] Frequency step',&
&    Efield(i_field)%W_step,unit=E_unit)
     call it(defs,trim(name)//'_Int',             '[RT '//trim(name)//'] Intensity',&
&    Efield(i_field)%intensity,unit=I_unit)
     call it(defs,trim(name)//'_Damp',             '[RT '//trim(name)//'] Damping',&
&    Efield(i_field)%damping,unit=Time_unit,verb_level=V_real_time)
     call it(defs,trim(name)//'_kind',            '[RT '//trim(name)//'] (SIN|RES|ANTIRES|GAUSS|DELTA|QSSIN|PULSE)',&
&    Efield(i_field)%ef_name,verb_level=0)
     call it(defs,trim(name)//'_Dir',             '[RT '//trim(name)//'] Versor',&
&    Efield(i_field)%versor,verb_level=0)
     call it(defs,trim(name)//'_Tstart',        '[RT '//trim(name)//'] Initial Time',&
&    Efield(i_field)%t_initial,unit=Time_unit,verb_level=V_real_time)
   end subroutine
   !
#endif
   !
   subroutine Xload(X,wv)
     !
     type(X_t):: X
     type(w_samp):: wv
     ! 
     ! Work Space
     ! 
     integer           ::i1
     integer,parameter ::n_des=14
     character(8)      ::nms(n_des),sfx
     character(40)     ::des(n_des)
     character(schlen) ::lch1,lch2
     !
     nms=(/'GrFnTp','EnRnge','DmRnge','CGrdSp','ETStps','EMStps',&
&          'DrudeW','LongDr','EhEngy','PPAPnt',&
&          'NGsBlk','QpntsR','BndsRn','DmERef'/)
     des(1) ='Green`s function t/c/r/a'
     des(2) ='Energy range'
     des(3) ='Damping range'
     des(4) ='[o/o] Coarse grid controller'
     des(5) ='Total Energy steps'
     des(6) ='[o/o] Memory Energy steps'
     des(7) ='Drude plasmon'
     des(8) ='[cc] Electric Field'
     des(9) ='Electron-hole energy range'
     des(10) ='PPA imaginary energy'
     des(11)='Response block size'
     des(12)='Transferred momenta'
     des(13)='Polarization function bands'
     des(14)='Damping reference energy'
     !
     do i1=1,n_des
       lch1=des(i1);lch2=nms(i1)
       if (X%whoami==1) sfx='Xx'
       if (X%whoami==2) sfx='Xs'
       if (X%whoami==3) sfx='Xd'
       if (X%whoami==4) sfx='Xp'
       write (des(i1),'(4a)')  '[',trim(sfx),'] ',trim(lch1)
       write (nms(i1),'(2a)')  lch2(1:6),trim(sfx)
     enddo
     !
     call it(defs,nms( 1),des( 1),X%ordering,verb_level=V_resp)
     call it(defs,nms( 2),des( 2),wv%er,E_unit,verb_level=0) 
     call it(defs,nms( 3),des( 3),wv%dr,E_unit,verb_level=0)
     call it(defs,nms( 4),des( 4),X%cg_percentual,verb_level=V_resp)
     call it(defs,nms( 5),des( 5),wv%n(1),verb_level=0)
     call it(defs,nms( 6),des( 6),wv%per_memstps,verb_level=1)
     call it(defs,nms( 7),des( 7),X%Wd,E_unit,verb_level=1) 
     call it(defs,nms( 8),des( 8),X%q0,verb_level=0)
     call it(defs,nms( 9),des( 9),X%ehe,E_unit,verb_level=V_resp)
     call it(defs,nms(10),des(10),X%ppaE,E_unit,verb_level=0)
     call it(defs,nms(11),des(11),X%ng,G_unit,verb_level=0) 
     call it(defs,nms(12),des(12),X%iq,verb_level=0)
     call it(defs,nms(13),des(13),X%ib,verb_level=0)
     call it(defs,nms(14),des(14),wv%damp_reference,E_unit,verb_level=V_resp)
     !
   end subroutine
   !
end subroutine
