!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine INIT_load(defs,en,q,k,X,Xw)
 !
 use pars,          ONLY:schlen,SP
 use electrons,     ONLY:levels,nel,filled_tresh
 use frequency,     ONLY:w_samp
 use it_m,          ONLY:it,initdefs,E_unit,G_unit,T_unit,Bfield_unit,MEM_unit,&
&                        Time_unit,I_unit,Angle_unit,V_parallel,initmode,&
&                        V_RL,V_kpt,V_sc,V_qp,V_io,V_general,V_resp,V_real_time,V_nl_optics
 use X_m,           ONLY:Chi_mode,X_t,q_plus_G_direction,Q_Shift_Order,Dipole_Energy_treshold,&
&                        global_gauge,Dipole_approach,Chi_linalg_mode, &
&                        X_terminator_Kind,X_terminator_E
 use com,           ONLY:grid_paths
 use stderr,        ONLY:slash
 use QP_m,          ONLY:QP_cg_percent,QP_G_damp,QP_solver,&
&                        QP_n_G_bands,QP_ng_Sx,QP_ng_Sc,QP_ng_SH,GW_terminator_E,GW_terminator_Kind,&
&                        QP_G_er,QP_G_dr,QP_Sc_steps,GWo_iterations,&
&                        QP_dSc_delta,QP_G_Zoom_treshold,GF_energy_range_mode
 use LIVE_t,        ONLY:nhash
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:Tel,non_periodic_directions,Bose_Temp
 use R_lattice,     ONLY:ng_closed,QP_states_k,nXkibz,&
&                        bz_samp,RIM_ng,RIM_epsm1,RIM_id_epsm1_reference,&
&                        RIM_n_rand_pts,cyl_ph_radius,box_length,cyl_length,cut_geometry
 use BS,            ONLY:BSE_mode,BSK_mode,BS_eh_en,BS_eh_win,BS_q,BS_bands,&
&                        BS_n_g_W,BS_n_g_exch
 use BS_solvers,    ONLY:BSS_mode,Haydock_treshold,BSS_n_freqs,&
&                        BSS_dr,BSS_er,BSS_q0,BSS_damp_reference,BSS_inversion_mode,&
&                        BSS_Wd,K_INV_EPS,K_INV_PL
 use TDDFT,         ONLY:FXC_n_g_corr,FXC_per_memstps,FXC_LRC_alpha,FXC_LRC_beta, &
&                        FXC_SVD_digits,FXC_PF_alpha
 use ACFDT,         ONLY:ACFDT_n_lambda,ACFDT_n_freqs,ACFDT_E_range
 use functions,     ONLY:bose_E_cut
 use parallel_m,    ONLY:n_log_CPUs
 use IO_m,          ONLY:DBs_IO_control_string,DBs_FRAG_control_string
#if defined _QED
 use QP_m,          ONLY:QP_QED_ng
#endif
#if defined _SC || defined _RT || defined _QED
 use hamiltonian,   ONLY:H_ref_bands,H_potential
 use collision_ext, ONLY:COLLISIONS_cutoff,COLL_bands
#endif
#if defined _SC
 use QP_m,          ONLY:SC_band_mixing,SC_E_threshold
 use SC,            ONLY:SC_iterations,SC_up_W_iters,&
&                        SC_rho_threshold,SC_cycle_mixing,&
&                        SC_bands,SC_preconditioning,OEP_approx
#endif
#if defined _RT
 use RT_control,    ONLY:JPC_RT_IO_t,OUTPUT_RT_IO_t,GF_RT_IO_t
 use stderr,        ONLY:intc
 use real_time,     ONLY:RT_step, Integrator_name, RAD_LifeTime, Phase_LifeTime, &
&                        NE_tot_time,RT_life_extrap_times_SAVE,RT_bands,         &
&                        RT_ave_dephasing,RT_eh_en,RT_scatt_tresh,Gr_kind,       &
&                        NE_MEM_treshold,NE_time_step_update_jump_SAVE,          &
&                        two_alpha,DbGd_EE_percent,NE_time_step_update_jump,     &
&                        NE_initial_time_step_update,NE_step_update_treshold,    &
&                        RT_MAX_step
 use fields,        ONLY:Efield,n_ext_fields_max,n_ext_fields
 use plasma,        ONLY:PLASMA_redux_percent
#endif
#if defined _NL
 use nl_optics,     ONLY:NL_bands,NL_damping,NL_correlation,NL_er,n_frequencies,NL_LRC_alpha
#endif 
#if defined _NL | defined _ELECTRIC
 use fields,        ONLY:Efield
#endif 
#if defined _MAGNETIC
 use magnetic,      ONLY:MAG_B,MAG_hamiltonian_type,MAG_radius,MAG_gauge,MAG_psi,MAG_theta
#endif
#if defined _ELPH 
 use ELPH,          ONLY:QP_PH_n_G_bands,elph_nDBs_used,elph_Ham_ik,elph_Ham_bands,elph_branches,&
&                               RES_tresh,FAN_deltaE_treshold,DW_deltaE_treshold
#endif
#if defined _OPENMP
 use openmp,        ONLY:n_threads_X,n_threads_SE,n_threads_RT,n_threads_DIP,n_threads_K,n_threads_NL
#endif
#if defined _SCALAPACK
 use SLK_m,         ONLY:SLK_test_H_dim
#endif
#if defined _PL
 use PHOTOLUM,      ONLY:PL_weights
#endif
#if defined _MEM_CHECK
 use memory,        ONLY:MEM_treshold
#endif
 !
 implicit none
 !
 type(initdefs)::defs
 type(levels)  ::en          
 type(bz_samp) ::q,k
 type(X_t)     ::X(4)
 type(w_samp)  ::Xw(4)
 !
 ! Work Space
 !
#if defined _RT
 integer  :: i_field
#endif
 real(SP) :: IO_times(3)
 !
 !===============
 ! GPL RUNLEVELS
 !===============
 call it('r',defs,'setup',   '[R INI] Initialization') 
 call it('r',defs,'optics',  '[R OPT] Optics')
 call it('r',defs,'chi',     '[R CHI] Dyson equation for Chi.')
 call it('r',defs,'bse',     '[R BSE] Bethe Salpeter Equation.')
 call it('r',defs,'bsk',     '[R BSK] Bethe Salpeter Equation kernel')
 call it('r',defs,'bss',     '[R BSS] Bethe Salpeter Equation solver')
 call it('r',defs,'tddft',   '[R   K] Use TDDFT kernel')
 call it('r',defs,'em1s',    '[R Xs] Static Inverse Dielectric Matrix')
 call it('r',defs,'em1d',    '[R Xd] Dynamical Inverse Dielectric Matrix')
 call it('r',defs,'ppa',     '[R Xp] Plasmon Pole Approximation')
 call it('r',defs,'HF_and_locXC',   '[R XX] Hartree-Fock Self-energy and Vxc')
 call it('r',defs,'gw0',     '[R GW] GoWo Quasiparticle energy levels')
 call it('r',defs,'life',    '[R GW] GoWo Quasiparticle lifetimes')
 call it('r',defs,'rim_cut', '[R RIM CUT] Coulomb potential')
 call it('r',defs,'cohsex',  '[R Xp] COlumb Hole Screened EXchange')
 !
 ! GPL_EXCLUDE_START
 !
 !===================
 ! NON GPL RUNLEVELS
 !===================
 call it('r',defs,'acfdt',   '[R ETOT] ACFDT Total Energy')
#if defined _ELPH 
 call it('r',defs,'ElPhHam', '[R] Frohlich Hamiltonian') 
#endif
 !
 ! GPL_EXCLUDE_END
 !
#if defined _SC && !defined _RT
 call it('r',defs,'scrun',      '[R] Self-Consistent single-particle calculation') 
#endif
#if defined _SC || defined _RT
 call it('r',defs,'collisions', '[R] Eval the extended Collisions')
#endif
#if defined _PL
 call it('r',defs,'photolum',   '[R] Photo-Luminescence') 
#endif
#if defined _RT
 call it('r',defs,'negf',       '[R] Real-Time dynamics') 
 call it('r',defs,'el_el_scatt','[R] Electron-Electron Scattering')   
 call it('r',defs,'el_ph_scatt','[R] Electron-Phonon   Scattering')   
#endif
#if defined _NL
 call it('r',defs,'nloptics',   '[R NL] Non-linear optics') 
#endif
#if defined _RT 
 call it('r',defs,'el_photon_scatt','[R] Electron-Photon   Scattering')   
#endif
#if defined _MAGNETIC
 call it('r',defs,'magnetic','[R] Magnetic fields')
#endif
#if defined _ELECTRIC
 call it('r',defs,'electric','[R] Static Electric Field')
#endif
#if defined _ELPH 
 call it('r',defs,'el_ph_corr',    '[R] Electron-Phonon Correlation')   
#endif
#if defined _SC
 call it('r',defs,'el_el_corr',    '[R] Electron-Electron Correlation') 
#endif
#if defined _QED
 call it('r',defs,'el_photon_corr','[R] Electron-Photon Correlation') 
#endif
#if defined _SCALAPACK
 call it('r',defs,'slktest','[R] ScaLapacK test') 
#endif
 !
 !=======
 ! CPU's
 !=======
 !
#if defined _MPI 
 call CPU_structure_load( )
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
 call it('f',defs,'BSEscatt', '[KPT] Compute extended k/q scatering',verb_level=V_kpt)
 call it(defs,'ElPhRndNq',  '[ELPH] Read random Q-points',elph_nDBs_used,verb_level=V_kpt) 
#endif
 call it('f',defs,'WFbuffIO','[IO] Wave-functions buffered I/O',verb_level=V_io) 
 !
 ! Parallel Setup
 !
#if defined _OPENMP
 call it(defs,'K_Threads',       '[OPENMP/BSK] Number of threads for response functions',n_threads_K)
 call it(defs,'X_Threads',       '[OPENMP/X] Number of threads for response functions',n_threads_X)
 call it(defs,'DIP_Threads',     '[OPENMP/X] Number of threads for dipoles',n_threads_DIP)
 call it(defs,'SE_Threads',      '[OPENMP/GW] Number of threads for self-energy',n_threads_SE)
 call it(defs,'RT_Threads',      '[OPENMP/RT] Number of threads for real-time',n_threads_RT)
 call it(defs,'NL_Threads',      '[OPENMP/NL] Number of threads for nl-optics',n_threads_NL)
#endif
#if defined _MPI 
 call it(defs,'NLogCPUs',     '[PARALLEL] Live-timing CPU`s (0 for all)',n_log_CPUs,verb_level=V_parallel)
#endif
 !
 ! I/O
 !
 call it(defs,'DBsIOoff', &
&             '[IO] Space-separated list of DB with NO I/O. DB=(DIP,X,HF,COLLs,J,GF,CARRIERs,W,SC,BS,ALL)',&
&             DBs_IO_control_string,verb_level=V_io)
 call it(defs,'DBsFRAGpm', &
&             '[IO] Space-separated list of +DB to FRAG and -DB to NOT FRAG. DB=(DIP,X,W,HF,COLLS,K,BS,QINDX,RT,ELPH,SC,ALL)',&
&             DBs_FRAG_control_string,verb_level=V_io)
 !
 ! S_xc
 !
 call it(defs,'LifeTrCG', '[GW] [o/o] Lifetime transition reduction',QP_cg_percent)
 call it(defs,'HARRLvcs', '[HA] Hartree  RL components',QP_ng_SH,G_unit)
 call it(defs,'EXXRLvcs', '[XX] Exchange RL components',QP_ng_Sx,G_unit)
 call it(defs,'CORRLvcs', '[GW] Correlat RL components',QP_ng_Sc,G_unit)
#if defined _QED
 call it(defs,'QEDRLvcs', '[QED] Vector-Potential G-vectors components',QP_QED_ng,G_unit)
#endif
#if defined _SC
 call it(defs,'GbndRnge', '[GW] G[W] bands range',QP_n_G_bands,verb_level=V_sc)
 call it('f',defs,'UseEbands', '[GW] Force COHSEX to use empty bands',verb_level=V_sc)
 call it('f',defs,'ALLGexx', '[XX] Force the use use all RL vectors for the exchange part',verb_level=V_sc)
 call it('f',defs,'ALLGHAR', '[HA] Force the use use all RL vectors for the Hartree potential',verb_level=V_sc)
#else
 call it(defs,'GbndRnge', '[GW] G[W] bands range',QP_n_G_bands)
 call it('f',defs,'UseEbands', '[GW] Force COHSEX to use empty bands',verb_level=V_qp)
#endif
 call it(defs,'GDamping', '[GW] G[W] damping',QP_G_damp,E_unit)
 call it(defs,'GDmRnge',  '[GW] G_gw damping range',QP_G_dr,E_unit)
 call it(defs,'dScStep',  '[GW] Energy step to evaluate Z factors',QP_dSc_delta,E_unit)
 call it(defs,'DysSolver','[GW] Dyson Equation solver ("n","s","g")',QP_solver,protect=.FALSE.)
 call it(defs,'GEnSteps', '[GW] Green`s Function (GF) energy steps',QP_Sc_steps)
 call it(defs,'GEnRnge',  '[GW] GF energy range',QP_G_er,E_unit)
 call it(defs,'GEnMode',  '[GW] GF energy mode ("centered","absolute"). "Centered" around the bare energy',&
&        GF_energy_range_mode,verb_level=V_qp)
 call it(defs,'GTermKind','[GW] GW terminator ("none","BG" Bruneval-Gonze)', GW_terminator_Kind,verb_level=V_qp)
 call it(defs,'GTermEn',  '[GW] GW terminator energy (only for kind="BG")', GW_terminator_E,E_unit,verb_level=V_qp)
 call it('f',defs,'NewtDchk',  '[GW] Test dSc/dw convergence',verb_level=V_qp)
 call it('f',defs,'ExtendOut', '[GW] Print all variables in the output file',verb_level=V_qp)
 !
 ! Xs Xd Xp
 !
 call it(defs,'Chimod', '[X] IP/Hartree/ALDA/LRC/PF/BSfxc',Chi_mode,protect=.FALSE.)
 call it(defs,'ChiLinAlgMod', '[X] inversion/lin_sys',Chi_linalg_mode,verb_level=V_resp)
 call Xload(X(2),Xw(2))
 call Xload(X(3),Xw(3))
 call Xload(X(4),Xw(4))
 call it(defs,'XTermKind','[X] X terminator ("none","BG" Bruneval-Gonze)',X_terminator_Kind,verb_level=V_resp)
 call it(defs,'XTermEn',  '[X] X terminator energy (only for kind="BG")',X_terminator_E,E_unit,verb_level=V_resp)
 call it('f',defs,'DrClassic', '[X] Use a classical model for the drude term',verb_level=V_resp)
 !
 ! BSE/BSK
 !
 call it(defs,'BSEmod',  '[BSE] resonant/retarded/coupling',BSE_mode)
 call it(defs,'BSEBands','[BSK] Bands range',BS_bands)
 call it(defs,'BSENGBlk','[BSK] Screened interaction block size',BS_n_g_W,G_unit)
 call it(defs,'BSENGexx','[BSK] Exchange components',BS_n_g_exch,G_unit)
 call it(defs,'BSEEhEny','[BSK] Electron-hole energy range',BS_eh_en,E_unit,verb_level=V_resp)
 !
 !GPL_INCLUDE_START
 !
 !call it(defs,'BSKmod',  '[BSE] IP/Hartree/HF/ALDA/SEX',BSK_mode,protect=.FALSE.)
 !
 ! GPL_INCLUDE_END
 !
 ! GPL_EXCLUDE_START
 !
 call it(defs,'BSKmod',  '[BSE] IP/Hartree/HF/ALDA/SEX/BSfxc',BSK_mode,protect=.FALSE.)
 call it('f',defs,'NoCondSumRule' ,'[BSE] Do not impose the conductivity sum rule in velocity gauge',verb_level=V_resp)
 !
 ! GPL_EXCLUDE_END
 !
 call it(defs,'Gauge' ,  '[BSE] Gauge (length|velocity)',global_gauge,verb_level=V_resp)
 call it('f',defs,'MetDamp' ,      '[BSE] Define '//slash//'w+=sqrt('//slash//'w*('//slash//'w+i'//slash//'eta))',verb_level=V_resp)
 call it(defs,'BSSmod',    '[BSS] (h)aydock/(d)iagonalization/(i)nversion/(t)ddft`',BSS_mode,protect=.FALSE.)
 call it(defs,'BSSInvMode','[BSS] Inversion solver modality `(f)ull/(p)erturbative`',BSS_inversion_mode)
 call it(defs,'BSSInvPFratio','[BSS] Inversion solver. Ratio between the number of frequencies solved pert/full',&
&        K_INV_EPS%PERT_FULL_ratio)
 call it(defs,'BLongDir',  '[BSS] [cc] Electric Field',BSS_q0)
 call it(defs,'BEnRange',  '[BSS] Energy range',BSS_er,E_unit)
 call it(defs,'BDmRange',  '[BSS] Damping range',BSS_dr,E_unit)
 call it(defs,'BSHayTrs',  '[BSS] Relative [o/o] Haydock treshold. Strict(>0)/Average(<0)',Haydock_treshold)
 call it(defs,'BSEPSInvTrs', '[BSS EPS] Inversion treshold. Relative[o/o](>0)/Absolute(<0)',K_INV_EPS%treshold)
 call it(defs,'BSPLInvTrs',  '[BSS PL] Inversion treshold',K_INV_PL%treshold)
 call it(defs,'BEnSteps',  '[BSS] Energy steps',BSS_n_freqs)
 call it(defs,'DrudeWBS',  '[BSE] Drude plasmon',BSS_Wd,E_unit,verb_level=V_resp)
 call it('f',defs,'WehDiag', '[BSK] diagonal (G-space) the eh interaction',verb_level=V_resp)
 call it('f',defs,'WehCpl',  '[BSK] eh interaction included also in coupling')
 call it('f',defs,'WRbsWF',  '[BSS] Write to disk excitonic the WFs',verb_level=V_resp)
#if !defined _SC
 call it('f',defs,'ALLGexx', '[BSS] Force the use use all RL vectors for the exchange part',verb_level=V_resp)
#endif
 call it('f',defs,'BSHayTer','[BSS] Terminate Haydock continuos fraction',verb_level=V_resp)
 call it('f',defs,'Reflectivity', '[BSS] Compute reflectivity at normal incidence',verb_level=V_resp)
 call it('f',defs,'BSSPertWidth', '[BSS] Include QPs lifetime in a perturbative way',verb_level=V_resp)
 call it('f',defs,'BSSInvKdiag','[BSS] In the inversion solver keep the diagonal kernel in place',verb_level=V_resp)
 !
 ! F_xc
 !
 call it(defs,'FxcGRLc',  '[TDDFT] XC-kernel RL size',FXC_n_g_corr,G_unit)
 call it(defs,'LRC_alpha','[TDDFT] LRC alpha factor',FXC_LRC_alpha)
 call it(defs,'PF_alpha','[TDDFT] PF alpha factor approximation: CUR/EMP/RBO/JGM',FXC_PF_alpha)
 call it(defs,'LRC_beta', '[TDDFT] LRC beta factor',FXC_LRC_beta,verb_level=V_resp)
 !
 ! Optics: large Q momenta
 !
 call it(defs,'Qdirection', '[Xd] Transferred momentum direction (iku)',q_plus_G_direction,verb_level=V_resp)
 call it(defs,'QShiftOrder','[Xd] Pick-up the (QShiftOrder)th q+G vector',Q_Shift_Order,verb_level=V_resp)
 !
#if defined _PL
 call it(defs,'PL_weights','[PL] [cc] Weights of the carthesian components of the emitted radiation',PL_weights)
#endif
 !
#if defined _KERR
 !
 ! BSE: Kerr
 !
 call it('f',defs,'EvalKerr', '[BSE] Compute the Kerr effect')
 call it('f',defs,'AnHall'  , '[BSE] Add the anomalous Hall effect to eps if using length gauge')
 !
#endif
 !
#if defined _RT
 !
 ! BSE: Real-Time
 !
 call it('f',defs,'ForceEqTrans','[RT-BSE] Use only equilibrium transitions',verb_level=V_real_time)
 !
#endif
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
 call it(defs,'CUTGeo',   '[CUT] Coulomb Cutoff geometry: box/cylinder/sphere X/Y/Z/XY..',cut_geometry)
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
 call it('f',defs,'QPExpand',   '[F GW] The QP corrections are expanded all over the BZ',verb_level=V_qp)
 call it('f',defs,'GreenF2QP',  '[F GW] Use real axis Green`s function to define the QPs',verb_level=V_qp)
 !
 ! GPL_EXCLUDE_END
 !
 call it('f',defs,'OnMassShell','[F GW] On mass shell approximation',verb_level=V_qp)
 !
#if defined _RT
 !
 ! Real Time dynamics
 !
 call it(defs,'RTBands',    '[RT] Bands',RT_bands)
 !
 call TMP_mirror_bands( )
 !
 call it(defs,'TwoAlpha',   '[RT] C_nk ~ alpha*Gamma_nk^2 ',two_alpha,Verb_level=V_real_time)
 call it(defs,'GrKind',     '[RT] G-ret kind: Lorentzian (QP)/ Hyperbolic QP_secant (HS)',Gr_kind,Verb_level=V_real_time)
 call it(defs,'RADLifeTime','[RT] Radiative life-time',RAD_LifeTime,unit=Time_unit(1),Verb_level=V_real_time)
 call it(defs,'PhLifeTime', '[RT] Phase Relaxation Time',Phase_LifeTime,unit=Time_unit(1),Verb_level=V_real_time)
 !
 ! Dynamics
 !
 call it(defs,'RTstep',      '[RT] Real Time step length',RT_step,unit=Time_unit(3))
 call it(defs,'NETime',      '[RT] Simulation Time',NE_tot_time,unit=Time_unit(2))
 call it(defs,'dTupdateTime','[RT] Initial Time for deltaT update (active only if non-zero) ',NE_initial_time_step_update,&
&                            unit=Time_unit(1),verb_level=V_real_time)
 call it(defs,'dTupdateJump','[RT] Time betweem two deltaT updates',  NE_time_step_update_jump_SAVE,&
&                            unit=Time_unit(1),verb_level=V_real_time)
 call it(defs,'dTupdateTresh','[RT][o/o] Treshold of deltaT updates',  NE_step_update_treshold ,verb_level=V_real_time)
 call it(defs,'dT_MAX',      '[RT] Maximum value for the time-dependent dT',  RT_MAX_step ,verb_level=V_real_time,unit=Time_unit(1))
 call it(defs,'Integrator',  '[RT] Integrator. Use keywords space separated  ( "EULER/RK2/HEUN" "EXP" "RWA") ',&
&                            Integrator_name)
 !
 IO_times=(/JPC_RT_IO_t%INTERVAL_time_SAVE,GF_RT_IO_t%INTERVAL_time_SAVE,OUTPUT_RT_IO_t%INTERVAL_time_SAVE/)
 call it(defs,'IOtime',      '[RT] Time between to consecutive I/O (J,P,CARRIERs - GF - OUTPUT)',IO_times,unit=Time_unit(1))
 JPC_RT_IO_t%INTERVAL_time_SAVE   =IO_times(1)
 GF_RT_IO_t%INTERVAL_time_SAVE    =IO_times(2)
 OUTPUT_RT_IO_t%INTERVAL_time_SAVE=IO_times(3)
 !
 call it(defs,'RTehEny',     '[RT] Electron-hole energy range',RT_eh_en,unit=E_unit,verb_level=V_real_time)
 call it('f',defs,'RTskipImposeN', '[RT] Conservation of N, dN  imposed by hand on-the-fly',Verb_level=V_real_time)
 call it('f',defs,'SaveGhistory',  '[RT] Save the history of the green function',Verb_level=V_real_time)
 !
 ! Scattering
 !
 call it('f',defs,'LifeExtrapolation',    '[RT] Skipped Lifetimes are extrapolated')
 call it(defs,'LifeExtrapSteps',   '[RT] Step length between and inside two consecutive groups of lifetimes',&
&                                  RT_life_extrap_times_SAVE,unit=Time_unit(1))
 call it(defs,'ScattTresh', '[RT] Treshold on the eh energy to compute the scattering',RT_scatt_tresh,&
&                           unit=E_unit,verb_level=V_real_time)
 ! 
 ! BGR
 !
 call it('f',defs,'BGR',     '[RT] Evaluate the Band-Gap Renormalization consistent with the applied potential',&
&                            verb_level=V_real_time)
 !
 ! EE scattering
 !
 call it(defs,'PlasmaPerc', '[RT] Plasma approx (0-100): % of eh pair considered',PLASMA_redux_percent,Verb_level=V_real_time)
 call it(defs,'EERimPerc',  '[RT] EE Double Grid (0-100): % of the points used in EE scatt',DbGd_EE_percent,Verb_level=V_real_time)
 call it('f',defs,'RTskipImposeE',   '[RT] Conservation of E (e-e channel) imposed by hand on-the-fly')
 !
 ! ELPH
 !
 call it(defs,'MemTresh',   '[RT] Treshold on the decay of the retarded GF',NE_MEM_treshold,verb_level=V_real_time)
 !call it('f',defs,'UseDebyeE',   '[RT] Use a single Debye energy for all phonon modes',verb_level=V_real_time)
 call it('f',defs,'RT_T_evol',   '[RT] Use a complete Time evolution instead of the CCA',verb_level=V_real_time)
 !
 ! External fields
 !
 do i_field=1,n_ext_fields_max
   call Afield(i_field)
 enddo
 !
 ! OLD / EXPERIMENTAL
 !
 ! 1 - Induced Field
 !
 call it('f',defs,'InducedField','[RT] Include induced field in coupling and current',verb_level=V_real_time)
 !
 ! 2 - Other
 !
 call it(defs,'RTAveDeph',    '[RT] Dephasing for all elements not included in RTDePhMatrix', &
&                             RT_ave_dephasing,verb_level=V_real_time,unit=Time_unit(1))
 call it('f',defs,'LifeFitTemp','[RT] Fit on the fly  lifetimes ratio to a Fermi distribution',verb_level=V_real_time)
 !
#endif
 !
 ! Non-linear Optics
 !
#if defined _NL
 !
 ! Non liner optics
 !
 call it(defs,'NLBands',      '[NL] Bands',NL_bands)
 call it(defs,'NLstep',       '[NL] Real Time step length',RT_step,unit=Time_unit(1),Verb_level=V_nl_optics)
 call it(defs,'NLtime',       '[NL] Simulation Time',NE_tot_time,unit=Time_unit(1),Verb_level=V_nl_optics)
 call it(defs,'NLintegrator', '[NL] Integrator ("EULEREXP/RK2/RK4/RK2EXP/HEUN/INVINT/CRANKNIC")',Integrator_name,&
&     Verb_level=V_nl_optics)
 call it(defs,'NLCorrelation','[NL] Correlation ("IPA/HARTREE/TDDFT/LRC/LRW/JGM/SEX") ',NL_correlation)
 call it(defs,'NLLrcAlpha',   '[NL] Long Range Correction',NL_LRC_alpha)
 call it(defs,'NLDamping',    '[NL] Damping',NL_damping,unit=E_unit)
 call it(defs,'NLEnRange',    '[NL] Energy range',NL_er,E_unit)
 call it(defs,'NLEnSteps',    '[NL] Energy steps',n_frequencies)
 call it('f',defs,'UseDipoles','[NL] Use Covariant Dipoles (just for test purpose)',verb_level=V_nl_optics)
 call it('f',defs,'FrSndOrd','[NL] Force second order in Covariant Dipoles',verb_level=V_nl_optics)
 !
 call it(defs,'ExtF_Dir',             '[NL ExtF] Versor',Efield(1)%versor)
 call it(defs,'ExtF_Int',             '[NL ExtF] Intensity',Efield(1)%intensity,unit=I_unit  ,verb_level=V_nl_optics)
 call it(defs,'ExtF_FrStep',          '[NL ExtF] Frequency step',Efield(1)%W_step,unit=E_unit,verb_level=V_nl_optics)
 call it(defs,'ExtF_Damp',            '[NL ExtF] FWHM',Efield(1)%FWHM,unit=Time_unit(1),verb_level=V_nl_optics)
 call it(defs,'ExtF_kind',            '[NL ExtF] Kind(SIN|SOFTSIN|RES|ANTIRES|GAUSS|DELTA|QSSIN)',Efield(1)%ef_name,& 
&    verb_level=V_nl_optics)
 call it(defs,'ExtF_Tstart',          '[NL ExtF] Initial Time',Efield(1)%t_initial,unit=Time_unit(1),               &
&    verb_level=V_nl_optics)
 !
#endif
 !
#if defined _ELECTRIC
 call it(defs,'ExtF_Dir',             '[NL ExtF] Versor',Efield(1)%versor)
 call it(defs,'ExtF_Int',             '[NL ExtF] Intensity',Efield(1)%intensity,unit=I_unit)
 call it('f',defs,'FrSndOrd','[NL] Force second order in Covariant Dipoles',verb_level=V_sc)
#endif
#if defined _SCALAPACK
 !
 ! SLK test
 !
 call it(defs,'SLKdim',  '[SLK] Matrix Dimension',SLK_test_H_dim)
#endif
 !
#if defined _MAGNETIC
 call it(defs,'Hamiltonian',   '[MAG] Hamiltonian kind [pauli,landau,all]',MAG_hamiltonian_type,protect=.FALSE.)
 call it(defs,'B_Field',       '[MAG] Magnetic field modulus',MAG_B,unit=Bfield_unit)
 call it(defs,'B_psi',         '[MAG] Magnetic field psi angle',MAG_psi,unit=Angle_unit(1),verb_level=V_sc)
 call it(defs,'B_theta',       '[MAG] Magnetic field theta angle',MAG_theta,unit=Angle_unit(1),verb_level=V_sc)
 call it(defs,'B_Gauge',       '[MAG] Gauge ("SYMM"etric, "X_ASYMM", "Y_ASYMM" or "Z_ASYMM"etric)',MAG_gauge,verb_level=V_sc)
 call it('f',defs,'PhaseTrick','[MAG] Phase trick for a better diagonalization',verb_level=V_sc)
 call it(defs,'B_radius',      '[MAG] Magnetic flux radius',MAG_radius,verb_level=V_sc)
#endif
 !
#if defined _MEM_CHECK
 !
 ! Memory
 !
 call it(defs,'MEM_tresh',     '[MEMORY] Treshold on traced memory allocations/deallocations',MEM_treshold,verb_level=V_general,&
&                                                                                             unit=MEM_unit(1))
 !
#endif
 !
 ! QP ctl 
 !
 call INIT_QP_ctl_load(defs,1)
 call INIT_QP_ctl_load(defs,2)
 call INIT_QP_ctl_load(defs,3)
 !
#if defined _RT
 !
 ! RT ctl 
 !
 call INIT_RT_ctl_load(defs,1)
 call INIT_RT_ctl_load(defs,2)
 call INIT_RT_ctl_load(defs,3)
 call INIT_RT_ctl_load(defs,4)
 !
#endif
 !
 ! GPL_EXCLUDE_START
 !
 ! F_xc
 !
 call it(defs,'FxcMEStps','[TDDFT] [o/o] Memory energy steps',&
&                         FXC_per_memstps,verb_level=V_resp)
 call it(defs,'FxcSVdig','[TDDFT] Keep SV that are within FxcSVdig digits',&
&                         FXC_SVD_digits,verb_level=V_resp)
 call it('f',defs,'FxcRetarded', '[TDDFT] Retarded TDDFT kernel',verb_level=V_resp)
 !
 ! BSE
 !
 call it(defs,'BSehWind','[BSK] [o/o] E/h coupling pairs energy window',&
&        BS_eh_win,verb_level=V_resp)
 call it(defs,'BSEQptR', '[BSK] Transferred momenta range',BS_q,verb_level=V_resp)
 call it(defs,'BDmERef', '[BSS] Damping energy reference',BSS_damp_reference,&
&                        E_unit,verb_level=V_resp)
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
 ! DIPOLES
 !
 call it(defs,'DipoleEtresh',   '[Xd] Treshold in the definition of R=P/deltaE',Dipole_Energy_treshold,&
&                                                                               verb_level=V_resp,unit=E_unit)
#if defined _NL
 call it(defs,'DipApproach',    '[Xd] [G-space v/R-space x/Covariant/Shifted grids]',Dipole_approach,verb_level=V_resp)
 call it('f',defs,'EvPolarization',    '[Xd] Evaluate Polarization (require DipApproach=Covariant)',verb_level=V_resp)
#else
 call it(defs,'DipApproach',    '[Xd] [G-space v/R-space x/Shifted grids]',Dipole_approach,verb_level=V_resp)
#endif
 call it(defs,'ShiftedPaths',   '[Xd] Shifted grids paths (separated by a space)',grid_paths,verb_level=V_resp)
 call it('f',defs,'DipPDirect', '[Xd] Directly compute <v> also when using other approaches for dipoles',verb_level=V_resp)
 !
 ! GPL_EXCLUDE_END
 !
 ! El-Ph
 !
#if defined _ELPH 
 call it(defs,'GphBRnge',  '[ELPH] G[W] bands range',QP_PH_n_G_bands)
 call it(defs,'ElPhModes', '[ELPH] Phonon modes included',elph_branches)
 call it(defs,'FANdEtresh','[ELPH] Energy treshold for Fan denominator',FAN_deltaE_treshold,verb_level=V_qp,unit=E_unit)
 call it(defs,'DWdEtresh', '[ELPH] Energy treshold for DW denominator',DW_deltaE_treshold,verb_level=V_qp,unit=E_unit)
 !
 ! GPL_EXCLUDE_START
 !
 call it(defs,'ElPhHBRnge','[ELPH] Hamiltonian bands range',elph_Ham_bands)
 call it(defs,'ElPhHKpt',  '[ELPH] Hamiltonian k-point',elph_Ham_ik)
 call it(defs,'REStresh',  '[ELPH] Residual treshold to report in output files',RES_tresh)
 !
 ! GPL_EXCLUDE_END
 !
 call it('f',defs,'WRgFsq','[ELPH] Dump on file gFsq coefficients')
#endif
 ! 
 ! SC
 !
#if defined _SC
 !
 call it(defs,'SCBands',   '[SC] Bands',SC_bands)
 !
 call TMP_mirror_bands( )
 !
 call it(defs,'SCIter',    '[SC] SC Iterations',SC_iterations)
 call it(defs,'SCEtresh',  '[SC] Energy convergence threshold',SC_E_threshold,unit=E_unit)
 call it(defs,'SCRhoTresh','[SC] Rho convergence threshold',SC_rho_threshold)
 call it(defs,'SC_precondition','[SC] Kind of preconditionin: thomas-fermi, simple, none',SC_preconditioning,verb_level=V_sc)
 call it(defs,'SCUpWIter', "[SC] Update W(q,G,G') every SCUpWIter iteractions",SC_up_W_iters,verb_level=V_qp) 
 call it('f',defs,'Mean_Potential','[SC] Real-space Mean Potential',verb_level=V_sc)
 call it('f',defs,'SCnlMix','[SC] Use SC non-local mixing',verb_level=V_sc)
 call it('f',defs,'FrozeDensity','[NL] Do not update density (for testing purposes)',verb_level=V_sc)
 ! 
 ! SC [common with RT]
 !
 call it(defs,'BandMix',   '[SC] Band mixing',SC_band_mixing)
 call it(defs,'SCmixing',  '[SC] SC Cycle Mixing (< 1.)',SC_cycle_mixing,verb_level=V_sc)
 call it('f',defs,'SCdiag','[SC] Diagonal approximation for the self-energy(WF unchaged)',verb_level=V_sc)
 !
#endif
 ! 
#if defined _SC || defined _RT || defined _QED
 !
 ! SC and RT common
 !
 call it(defs,'COLLBands',   '[COLL] Bands for the collisions',COLL_bands)
 !
 call TMP_mirror_bands( )
 !
 call it(defs,'HXC_Potential', '[SC] SC HXC Potential',H_potential,protect=.FALSE.)
 call it(defs,'EXXCut',    '[SC,RT] Cutoff on the exchange, 0=full 1=none',COLLISIONS_cutoff,verb_level=V_sc)
 !
#endif
 !
#if defined _RT
 call TMP_mirror_bands( )
#endif
 !
#if defined _SC
 call it('f',defs,'OEPItSolver','[SC] Iterative solution instead of inversion of OEP')
 call it(defs,'OEPapprox','[SC] OEP approximation: n=none s=Slater k=KLI c=CED +w=Weighted',OEP_approx)
#endif

 ! 
 contains 
   !
#if defined _RT
   !
   subroutine Afield(i_field)
     use parser_m, ONLY  : parser
     character(10)      :: name
     integer            :: i_field
     logical            :: is_def
     !
     name='Field'//trim(intc(i_field))
     !
     call it(defs,trim(name)//'_Freq',            '[RT '//trim(name)//'] Frequency',&
&            Efield(i_field)%frequency,unit=E_unit)
     call it(defs,trim(name)//'_DFreq',           '[RT '//trim(name)//'] Frequency step',&
&            Efield(i_field)%W_step,unit=E_unit,verb_level=V_real_time)
     call it(defs,trim(name)//'_Int',             '[RT '//trim(name)//'] Intensity',&
&            Efield(i_field)%intensity,unit=I_unit)
     call it(defs,trim(name)//'_Width',           '[RT '//trim(name)//'] Width',&
&            Efield(i_field)%width,unit=Time_unit(1))
     call it(defs,trim(name)//'_FWHM',            '[RT '//trim(name)//'] Full Width at Half Maximum (alternative to Width)',&
&            Efield(i_field)%FWHM,unit=Time_unit(1),verb_level=V_real_time)
     call it(defs,trim(name)//'_kind',            '[RT '//trim(name)//'] Kind(SIN|RES|ANTIRES|GAUSS|DELTA|QSSIN)',&
&            Efield(i_field)%ef_name,verb_level=0)
     call it(defs,trim(name)//'_pol',             '[RT '//trim(name)//'] Pol(linear|circular)',&
&            Efield(i_field)%ef_pol,verb_level=0)
     call it(defs,trim(name)//'_Dir',             '[RT '//trim(name)//'] Versor',&
&            Efield(i_field)%versor,verb_level=0)
     call it(defs,trim(name)//'_Dir_circ',        '[RT '//trim(name)//'] Versor_circ',&
&            Efield(i_field)%versor_circ,verb_level=0)
     call it(defs,trim(name)//'_Tstart',          '[RT '//trim(name)//'] Initial Time',&
&            Efield(i_field)%t_initial,unit=Time_unit(1))
     !
     if (initmode==4) then
       call parser(trim(name)//'_kind',is_def)
       if (is_def.and.n_ext_fields==0) n_ext_fields=n_ext_fields+1
       call parser(trim(name)//'_FWHM',is_def)
       if (is_def) then
         Efield(i_field)%width=Efield(i_field)%FWHM/(2.*sqrt(2.*log(2.)))
       else
         Efield(i_field)%FWHM=Efield(i_field)%width*2.*sqrt(2.*log(2.))
       endif
     endif
     !
   end subroutine
   !
#endif
   !
#if defined _MPI 
   !
   subroutine CPU_structure_load()
     use parallel_m, ONLY:n_CPU_str_max,CPU_structure,PARALLEL_default_mode
     integer           :: i_s
     character(schlen) :: name,possible_fields
     !
     call it(defs,"PAR_def_mode",'[PARALLEL] Default distribution mode ("balanced"/"memory"/"workload")',&
&            PARALLEL_default_mode,verb_level=V_parallel)
     !
     do i_s=1,n_CPU_str_max 
       !
       if (i_s==1) possible_fields='(k,c,v)'   ! X_q_0
       if (i_s==2) possible_fields='(q,k,c,v)' ! X_finite_q
       if (i_s==3) possible_fields='(q,k,c,v)' ! X_all_q
       if (i_s==4) possible_fields='(k,eh,t)'  ! BS
       if (i_s==5) possible_fields='(q,qp,b)'  ! SE
       if (i_s==6) possible_fields='(k,b,q,qp)'! RT
       if (i_s==7) possible_fields='(p,d) '    ! SLK
       !
       if (len_trim(CPU_structure(i_s)%Short_Description)==0) cycle
       name=trim(CPU_structure(i_s)%Short_Description)//"_CPU"
       call it(defs,trim(name),'[PARALLEL] CPUs for each role',&
&              CPU_structure(i_s)%CPU_string,verb_level=V_parallel)
       name=trim(CPU_structure(i_s)%Short_Description)//"_ROLEs"
       call it(defs,trim(name),'[PARALLEL] CPUs roles '//trim(possible_fields),&
&              CPU_structure(i_s)%ROLE_string,verb_level=V_parallel)
       name=trim(CPU_structure(i_s)%Short_Description)//"_nCPU_LinAlg_INV"
       call it(defs,trim(name),'[PARALLEL] CPUs for Linear Algebra',&
&              CPU_structure(i_s)%nCPU_lin_algebra_INV,verb_level=V_parallel)
       name=trim(CPU_structure(i_s)%Short_Description)//"_nCPU_LinAlg_DIAGO"
       call it(defs,trim(name),'[PARALLEL] CPUs for Linear Algebra',&
&              CPU_structure(i_s)%nCPU_lin_algebra_DIAGO,verb_level=V_parallel)
     enddo
     !
   end subroutine
   !
#endif
   !
   subroutine Xload(X,wv)
     !
     use pars,   ONLY:SP
     use units,  ONLY:HA2EV
     !
     type(X_t):: X
     type(w_samp):: wv
     ! 
     ! Work Space
     ! 
     integer           ::i1,V_iq
     integer,parameter ::n_des=14
     character(8)      ::nms(n_des),sfx
     character(100)    ::des(n_des)
     character(schlen) ::lch1,lch2
     !
     nms=(/'GrFnTp','EnRnge','DmRnge','CGrdSp','ETStps','EMStps',&
&          'DrudeW','LongDr','EhEngy','PPAPnt',&
&          'NGsBlk','QpntsR','BndsRn','DmERef'/)
     des(1) ='Green`s function (T)ordered,(R)etarded,(r)senant,(a)ntiresonant [T, R, r, Ta, Ra]'
     des(2) ='Energy range'
     des(3) ='Damping range'
     des(4) ='[o/o] Coarse grid controller'
     des(5) ='Total Energy steps'
     des(6) ='[o/o] Memory Energy steps'
     des(7) ='Drude plasmon'
     des(8) ='[cc] Electric Field'
     des(9) ='Electron-hole energy range'
     des(10)='PPA imaginary energy'
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
     V_iq  =0
     !
     if (.not.X%whoami==3) V_iq  =V_resp 
     !
     call it(defs,nms( 1),des( 1),X%ordering,verb_level=V_resp)
     call it(defs,nms( 2),des( 2),wv%er,E_unit,verb_level=0) 
     call it(defs,nms( 3),des( 3),wv%dr,E_unit,verb_level=0)
     call it(defs,nms( 4),des( 4),X%cg_percentual,verb_level=V_resp)
     call it(defs,nms( 5),des( 5),wv%n_freqs,verb_level=0)
     call it(defs,nms( 6),des( 6),wv%per_memstps,verb_level=V_resp)
     call it(defs,nms( 7),des( 7),X%Wd,E_unit,verb_level=V_resp)
     call it(defs,nms( 8),des( 8),X%q0,verb_level=0)
     call it(defs,nms( 9),des( 9),X%ehe,E_unit,verb_level=V_resp)
     call it(defs,nms(10),des(10),X%ppaE,E_unit,verb_level=0)
     call it(defs,nms(11),des(11),X%ng,G_unit,verb_level=0)
     call it(defs,nms(12),des(12),X%iq,verb_level=V_iq)
     call it(defs,nms(13),des(13),X%ib,verb_level=0)
     call it(defs,nms(14),des(14),wv%damp_reference,E_unit,verb_level=V_resp)
     !
   end subroutine
   !
#if defined _SC || defined _RT || defined _QED
   !TMP>
   subroutine TMP_mirror_bands( )
     use parser_m,       ONLY:parser
     logical :: is_def(4)
     integer :: bands_(2)
     call parser('SCbands',  is_def(1))
     call parser('RTbands',  is_def(2))
     call parser('COLLbands',is_def(3))
     call parser('NLbands',  is_def(4))
#endif
     !
#if defined _SC 
     if (is_def(1)) bands_=SC_bands
#endif
#if defined _RT || defined _QED
     if (is_def(2)) bands_=RT_bands
#endif
#if defined _SC || defined _RT || defined _QED || defined _NL
     if (is_def(3)) bands_=COLL_bands
#endif
#if defined _RT || defined _QED
     if (is_def(4)) bands_=NL_bands
#endif
     !
#if defined _SC 
     SC_bands  =bands_
#endif
#if defined _RT || defined _QED
     RT_bands  =bands_
#endif
#if defined _NL
     NL_bands  =bands_
     RT_bands  =bands_
#endif
#if defined _SC || defined _RT || defined _QED || defined _NL
     COLL_bands=bands_
     H_ref_bands=bands_     
     !
   end subroutine
   !TMP<
#endif
   !
end subroutine
