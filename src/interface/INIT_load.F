!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_load(defs,en,q,k,X,Xw,Dip)
 !
 use drivers,       ONLY:infile_editing
 use pars,          ONLY:schlen,SP
 use electrons,     ONLY:levels,nel,filled_tresh
 use zeros,         ONLY:G_mod_zero
 use frequency,     ONLY:w_samp
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,E_unit,G_unit,T_unit,Bfield_unit,MEM_unit,&
&                        Time_unit,I_unit,Angle_unit,V_parallel,initmode,V_ph,&
&                        V_RL,V_kpt,V_sc,V_qp,V_io,V_general,V_resp,V_real_time,V_nl_optics
 use X_m,           ONLY:Chi_mode,X_t,q_plus_G_direction,Q_Shift_Order,&
&                        global_gauge,Chi_linalg_mode, &
&                        X_terminator_Kind,X_terminator_E,X_DbGd_percentual
 use DIPOLES,       ONLY:DIPOLE_t
 use com,           ONLY:grid_paths
 use stderr,        ONLY:slash
 use QP_m,          ONLY:QP_cg_percent,QP_G_damp,QP_solver,&
&                        QP_n_G_bands,QP_ng_Sx,QP_ng_Sc,QP_ng_SH,QP_ng_Vxc,GW_terminator_E,GW_terminator_Kind,&
&                        QP_G_er,QP_G_ir,QP_G_dr,QP_Sc_steps,QP_G_solver,&
&                        QP_dSc_delta,QP_G_Zoom_treshold,GF_energy_range_mode
 use LIVE_t,        ONLY:nhash
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:Tel,non_periodic_directions,molecule_position,Bose_Temp
 use R_lattice,     ONLY:ng_closed,QP_states_k,nXkibz,k_GRIDS_string,RIM_W_ng,rimw_type,&
&                        bz_samp,RIM_ng,RIM_epsm1,RIM_id_epsm1_reference,&
&                        RIM_n_rand_pts,cyl_ph_radius,box_length,cyl_length,cut_geometry,ws_cutoff
 use BS,            ONLY:BSE_mode,BSE_prop,BSK_mode,BS_eh_en,BS_eh_win,BS_q,BS_bands,&
&                        BS_n_g_W,BS_n_g_exch,BS_n_g_fxc,BSE_L_kind,BS_K_cutoff,BSK_IO_mode,BSE_dipole_geometry
 use BS_solvers,    ONLY:BSS_mode,Haydock_threshold,Haydock_iterIO,BSS_n_freqs,&
&                        BSS_dr,BSS_er,BSS_E_dir,BSS_Q_dir,BSS_damp_reference,BSS_inversion_mode,&
&                        BSS_Wd,K_INV_EPS,K_INV_PL,BSS_n_eig,Haydock_iterMAX
#if defined _SLEPC
 use BS_solvers,    ONLY:BSS_slepc_target_E,BSS_slepc_extraction,BSS_slepc_ncv,BSS_slepc_tol,BSS_slepc_maxit,&
 &                       BSS_slepc_precondition,BSS_slepc_approach,BSS_slepc_mpd,BSS_slepc_matrix_format
#endif
 use TDDFT,         ONLY:FXC_n_g_corr,FXC_per_memstps,FXC_LRC_alpha,FXC_LRC_beta, &
&                        FXC_SVD_digits,FXC_PF_alpha,FXC_mode
 use ACFDT,         ONLY:ACFDT_n_lambda,ACFDT_n_freqs,ACFDT_E_range
 use functions,     ONLY:bose_E_cut
 use parallel_m,    ONLY:n_log_CPUs
 use IO_m,          ONLY:DBs_IO_control_string,DBs_FRAG_control_string
 use QP_m,          ONLY:SC_E_threshold
#if defined _OPENMP
 use openmp,        ONLY:n_threads_X,n_threads_SE,n_threads_RT,n_threads_DIP,n_threads_K,n_threads_NL, &
&                        n_threads_OSCLL
#endif
#if defined _SCALAPACK
 use SLK_m,         ONLY:SLK_test_H_dim
#endif
 use gpu_m,         ONLY:GPU_test_dim
 use PHOTOLUM,      ONLY:PL_weights
#if defined _MEM_CHECK
 use y_memory,      ONLY:MEM_treshold
#endif
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
 ! ... list of USER fields 
 call LEVELS_mirror("USER_fields",en,X,Dip,l_USER_fields)
 !
 ! ... from C_driver
 !
 call INIT_C_driver_runlevels(defs)
 !
 ! ... derived
 !=============
 call it('r',defs,'chi',     '[R][CHI] Dyson equation for Chi.')
 call it('r',defs,'bse',     '[R][BSE] Bethe Salpeter Equation.')
 call it('r',defs,'tddft',   '[R][K] Use TDDFT kernel')
 call it('r',defs,'cohsex',  '[R][Xp] COlumb Hole Screened EXchange')
 call it('r',defs,'Xx',       '[R][Xx] Dynamical Response Function')
 call it('r',defs,'em1s',    '[R][Xs] Statically Screened Interaction')
 call it('r',defs,'em1d',    '[R][X] Dynamically Screened Interaction')
 call it('r',defs,'ppa',     '[R][Xp] Plasmon Pole Approximation for the Screened Interaction')
 call it('r',defs,'mpa',     '[R][Xm] Multi Pole Approximation for the Screened Interaction')
 !
 call it('r',defs,'el_el_corr',    '[R] Electron-Electron Correlation') 
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
 call it(defs,'Gthresh' ,'[INI] Accuracy for energy shell separation [-1. automatic]',G_mod_zero,verb_level=V_RL)
 call it(defs,'FFTGvecs','[FFT] Plane-waves',wf_ng,G_unit,verb_level=V_RL) 
 call it(defs,'NonPDirs','[X/BSS] Non periodic chartesian directions (X,Y,Z,XY...)',non_periodic_directions,verb_level=V_resp)
 call it(defs,'MolPos',  '[X/BSS] Molecule coord in supercell, 0.5 is the middle',molecule_position,verb_level=V_resp)
 call it(defs,'K_grids', '[KPT] Select the grids (X=response, S=sigma, C=collisions, B=bse) [default X S C B]',&
&                        k_GRIDS_string,verb_level=V_kpt)
 call it(defs,'IkSigLim','[KPT] QP K-points indices range',QP_states_k,verb_level=V_kpt)
 call it(defs,'IkXLim',  '[KPT] X grid last k-point index',nXkibz,verb_level=V_kpt)
 call it(defs,'Nelectro','Electrons number',nel,verb_level=V_general)
 call it(defs,'ElecTemp','Electronic Temperature',Tel,T_unit,verb_level=V_general)
 call it(defs,'OccTresh','Occupation treshold (metallic bands)',filled_tresh,verb_level=V_general)
 call it(defs,'BoseTemp','Bosonic Temperature',Bose_Temp,T_unit,verb_level=V_general)
 call it(defs,'BoseCut', '[BOSE] Finite T Bose function cutoff',bose_E_cut,verb_level=V_general)
 call it('f',defs,'WFbuffIO','[IO] Wave-functions buffered I/O',verb_level=V_io) 
 call it('f',defs,'NoDiagSC','New setup for non-diagonal supercells',verb_level=V_general) 
 call it('f',defs,'DegFix','Force the code to impose the energy levels to respect their degeneracy') 
 !
 ! Parallel Setup
 !
#if defined _OPENMP
 call it(defs,'K_Threads',       '[OPENMP/BSK] Number of threads for response functions',n_threads_K)
 call it(defs,'X_Threads',       '[OPENMP/X] Number of threads for response functions',n_threads_X)
 call it(defs,'DIP_Threads',     '[OPENMP/X] Number of threads for dipoles',n_threads_DIP)
 call it(defs,'OSCLL_Threads',    '[OPENMP/X] Number of threads for Oscillators',n_threads_OSCLL)
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
&             '[IO] Space-separated list of DB with NO I/O. DB=(DIP,X,HF,COLLs,J,GF,CARRIERs,OBS,W,SC,BS,ALL)',&
&             DBs_IO_control_string,verb_level=V_io)
 call it(defs,'DBsFRAGpm', &
&             '[IO] Space-separated list of +DB to FRAG and -DB to NOT FRAG. DB=(DIP,X,W,HF,COLLS,K,BS,QINDX,RT,ELPH,SC,ALL)',&
&             DBs_FRAG_control_string,verb_level=V_io)
 !
 ! S_xc
 !
 call it(defs,'LifeTrCG', '[GW] [o/o] Lifetime transition reduction',QP_cg_percent)
 call it(defs,'HARRLvcs', '[HA] Hartree     RL components',QP_ng_SH,G_unit)
 call it(defs,'EXXRLvcs', '[XX] Exchange    RL components',QP_ng_Sx,G_unit)
 call it('f',defs,'UseNLCC',  '[XC] If present, add NLCC contributions to the charge density ',verb_level=V_qp)
 call it(defs,'VXCRLvcs', '[XC] XCpotential RL components',QP_ng_Vxc,G_unit)
 call it(defs,'CORRLvcs', '[GW] Correlation RL components',QP_ng_Sc,G_unit)
 call it(defs,'GbndRnge', '[GW] G[W] bands range',QP_n_G_bands)
 call it('f',defs,'UseEbands', '[GW] Force COHSEX to use empty bands',verb_level=V_qp)
 call it(defs,'GDamping', '[GW] G[W] damping',QP_G_damp,E_unit,verb_level=V_qp)
 call it(defs,'GDmRnge',  '[GW] G_gw damping range',QP_G_dr,E_unit)
 call it(defs,'dScStep',  '[GW] Energy step to evaluate Z factors',QP_dSc_delta,E_unit,verb_level=V_qp)
 call it(defs,'DysSolver','[GW] Dyson Equation solver ("n","s","g","q")',QP_solver,protect=.FALSE.,case="a")
 call it(defs,'GsampType','[GW] Type of sampling ("ra" FF-RA, "1l" MPA-1l, "2l" MPA-2l)',QP_G_solver)
 call it(defs,'GEnSteps', '[GW] Green`s Function (GF) energy steps',QP_Sc_steps)
 call it(defs,'GEnRnge',  '[GW] GF energy range',QP_G_er,E_unit)
 call it(defs,'GImRnge',  '[GW] GF imaginary range',QP_G_ir,E_unit)
 call it(defs,'GEnMode',  '[GW] GF energy mode ("centered","absolute"). "Centered" around the bare energy',&
&        GF_energy_range_mode,verb_level=V_qp)
 call it(defs,'GTermKind','[GW] GW terminator ("none","BG" Bruneval-Gonze,"BRS" Berger-Reining-Sottile)', GW_terminator_Kind)
 call it(defs,'GTermEn',  '[GW] GW terminator energy (only for kind="BG")', GW_terminator_E,E_unit,verb_level=V_qp)
 call it('f',defs,'NewtDchk',  '[GW] Test dSc/dw convergence',verb_level=V_qp)
 call it('f',defs,'ExtendOut', '[GW] Print all variables in the output file',verb_level=V_qp)
 call it('f',defs,'QPsymmtrz', '[GW] Force symmetrization of states with the same energy',verb_level=V_qp)
 !
 ! Xx Xs Xd Xp Xm
 !
 call it(defs,'Chimod',       '[X] IP/Hartree/ALDA/LRC/PF/BSfxc',Chi_mode,protect=.FALSE.,case="A")
 call it(defs,'ChiLinAlgMod', '[X] inversion/lin_sys,cpu/gpu',Chi_linalg_mode,verb_level=V_resp,case="A")
 call Xload(X(2),Xw(2))
 call Xload(X(3),Xw(3))
 call Xload(X(4),Xw(4))
 call Xload(X(5),Xw(5))
 call it(defs,'XTermKind','[X] X terminator ("none","BG" Bruneval-Gonze)',X_terminator_Kind,verb_level=V_resp)
 call it(defs,'XTermEn',  '[X] X terminator energy (only for kind="BG")',X_terminator_E,E_unit,verb_level=V_resp)
 call it('f',defs,'DrClassic', '[X] Use a classical model for the drude term',verb_level=V_resp)
 call it('f',defs,'mpERdb',  '[Xm] Write to disk MPA poles and residues')
 call it('f',defs,'WriteXo', '[X] Write on the-fly the IP response function',verb_level=V_resp)
 !
 ! BSE/BSK
 !
 call it(defs,'BSEmod',  '[BSE] resonant/retarded/coupling',BSE_mode,case="a")
 call it(defs,'Lkind',   '[BSE,X] bar(default)/full/tilde',BSE_L_kind,verb_level=V_resp)
 call it(defs,'BSEBands','[BSK] Bands range',BS_bands)
 call it(defs,'BSENGBlk','[BSK] Screened interaction block size [if -1 uses all the G-vectors of W(q,G,Gp)]',BS_n_g_W,G_unit)
 call it(defs,'BSENGexx','[BSK] Exchange components',BS_n_g_exch,G_unit)
 call it(defs,'BSENGfxc','[BSK] Fxc components', BS_n_g_fxc,G_unit,verb_level=V_resp)
 call it(defs,'FxcMode', '[TDDFT] ("G-XXX" or "R-XXX" with XXX=def/full/cut_Gmax/cut_GmGp") ',FXC_mode,verb_level=V_resp)
 call it(defs,'BSEEhEny','[BSK] Electron-hole energy range',BS_eh_en,E_unit,verb_level=V_resp)
 call it(defs,'BSKCut',  '[BSK] Cutoff on the BSE Kernel, 0=full 1=none',BS_K_cutoff,verb_level=V_resp)
 call it(defs,'BSKIOmode','[BSK] ("1D_linear"/"2D_standard" + "norestart")',BSK_IO_mode,verb_level=V_resp)
 call it(defs,'BSKmod',  '[BSE] IP/Hartree/HF/ALDA/SEX/BSfxc',BSK_mode,protect=.FALSE.,case="A")
 call it(defs,'Gauge' ,  '[BSE/X] Gauge (length|velocity)',global_gauge,verb_level=V_resp)
 call it('f',defs,'NoCondSumRule' ,'[BSE/X] Do not impose the conductivity sum rule in velocity gauge',verb_level=V_resp)
 call it('f',defs,'MetDamp' ,      '[BSE] Define '//slash//'w+=sqrt('//slash//'w*('//slash//'w+i'//slash//'eta))',verb_level=V_resp)
 call it(defs,'BSSmod',    '[BSS] (h)aydock/(d)iagonalization/(s)lepc/(i)nversion/(t)ddft`',BSS_mode,protect=.FALSE.)
 call it(defs,'BSEprop',   '[BSS] Can be any among abs/jdos/kerr/asymm/anHAll/magn/dich/photolum/esrt',BSE_prop)
 call it(defs,'BSEdips',   '[BSS] Can be "trace/none" or "xy/xz/yz" to define off-diagonal rotation plane',BSE_dipole_geometry)
 call it(defs,'BSSInvMode','[BSS] Inversion solver modality `(f)ull/(p)erturbative`',BSS_inversion_mode)
 call it(defs,'BSSInvPFratio','[BSS] Inversion solver. Ratio between the number of frequencies solved pert/full',&
&             K_INV_EPS%PERT_FULL_ratio)
 call it(defs,'BLongDir',  '[BSS] [cc] Electric Field versor',BSS_E_dir)
 !call it(defs,'EFieldDir',  '[BSS] [cc] Electric Field versor',BSS_E_dir)
 call it(defs,'QPropDir',   '[BSS] [cc] Propagation versor Field',BSS_Q_dir,verb_level=V_resp)
 call it(defs,'BEnRange',  '[BSS] Energy range',BSS_er,E_unit)
 call it(defs,'BDmRange',  '[BSS] Damping range',BSS_dr,E_unit)
 call it(defs,'BSHayTrs',  '[BSS] Relative [o/o] Haydock threshold. Strict(>0)/Average(<0)',Haydock_threshold)
 call it(defs,'BSHayItrMAX','[BSS] MaX number of Haydock iterations.',Haydock_iterMAX,verb_level=V_resp)
 call it(defs,'BSHayItrIO', '[BSS] Iterations between IO for Haydock restart.',Haydock_iterIO,verb_level=V_resp)
 call it(defs,'BSEPSInvTrs', '[BSS EPS] Inversion treshold. Relative[o/o](>0)/Absolute(<0)',K_INV_EPS%treshold)
 call it(defs,'BSPLInvTrs',  '[BSS PL] Inversion treshold',K_INV_PL%treshold)
 call it(defs,'BEnSteps',  '[BSS] Energy steps',BSS_n_freqs)
 call it(defs,'DrudeWBS',  '[BSE] Drude plasmon',BSS_Wd,E_unit,verb_level=V_resp)
 call it('f',defs,'FxcLibxc','[BSK] force computing Fxc via libxc',verb_level=V_resp)
 call it('f',defs,'WehDiag', '[BSK] diagonal (G-space) the eh interaction',verb_level=V_resp)
 call it('f',defs,'WehCpl',  '[BSK] eh interaction included also in coupling')
 call it('f',defs,'WRbsWF',  '[BSS] Write to disk excitonic the WFs')
#if defined _SLEPC
 call it(defs,'BSSSlepcApproach','[SLEPC] Approach ("Krylov-Schur","Generalized-Davidson","Jacobi-Davidson")',&
&                                                                          BSS_slepc_approach,verb_level=V_resp)
 call it(defs,'BSSNEig',       '[SLEPC] Number of eigenvalues to compute',BSS_n_eig)
 call it(defs,'BSSEnTarget',   '[SLEPC] Target energy to find eigenvalues',BSS_slepc_target_E,E_unit)
 call it(defs,'BSSSlepcMaxIt', '[SLEPC] Maximum number of iterations',BSS_slepc_maxit)
 call it(defs,'BSSSlepcPrecondition','[SLEPC] Precondition technique (none|preonly+jacobi|bcgs+jacobi)',&
 &                                                                              BSS_slepc_precondition,verb_level=V_resp)
 call it(defs,'BSSSlepcExtraction','[SLEPC] Extraction technique (ritz|harmonic)',BSS_slepc_extraction,verb_level=V_resp)
 call it(defs,'BSSSlepcNCV',   '[SLEPC] Dimension of the subspace',BSS_slepc_ncv,verb_level=V_resp)
 call it(defs,'BSSSlepcMPD',   '[SLEPC] Maximum allowed dimention',BSS_slepc_mpd,verb_level=V_resp)
 call it(defs,'BSSSlepcTol',   '[SLEPC] Tolerance for the iterative solver',BSS_slepc_tol,verb_level=V_resp)
 call it(defs,'BSSSlepcMatrixFormat','[SLEPC] Matrix type (shell|explicit|explicit-debug)',&
 &                                                BSS_slepc_matrix_format,verb_level=V_resp)
#endif
 call it('f',defs,'ALLGexx', '[BSS] Force the use use all RL vectors for the exchange part',verb_level=V_resp)
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
 ! Optics: Q=0 directions average
 !
 call it('f',defs,'OptDipAverage', '[Xd] Average Xd along the non-zero Electric Field directions',verb_level=V_resp)
 !
 ! Optics: large Q momenta
 !
 call it(defs,'Qdirection', '[Xd] Transferred momentum direction (iku)',q_plus_G_direction,verb_level=V_resp)
 call it(defs,'QShiftOrder','[Xd] Pick-up the (QShiftOrder)th q+G vector',Q_Shift_Order,verb_level=V_resp)
 !
 ! BSE properties: kerr, magnons, absorption, ...
 !
 call it(defs,'PL_weights','[PL] [cc] Weights of the carthesian components of the emitted radiation',PL_weights,verb_level=V_resp)
 !
 ! Double Grid(s)
 !
 call it(defs,'DbGdQsize', '[X,DbGd][o/o] Percentual of the total DbGd transitions to be used',X_DbGd_percentual,verb_level=V_resp)
 !
 ! RIM
 !
 call it(defs,'Em1Anys', '[RIM] X Y Z Static Inverse dielectric matrix Anysotropy',RIM_epsm1,verb_level=V_RL)
 call it(defs,'IDEm1Ref','[RIM] Dielectric matrix reference component 1(x)/2(y)/3(z)',&
&                         RIM_id_epsm1_reference,verb_level=V_RL)
 call it(defs,'RandQpts','[RIM] Number of random q-points in the BZ',RIM_n_rand_pts)
 call it(defs,'RandGvec','[RIM] Coulomb interaction RS components',RIM_ng,G_unit)
 call it('f',defs,'QpgFull', '[F RIM] Coulomb interaction: Full matrix',verb_level=V_RL)
 !
 ! RIM-W
 call it(defs,'RandGvecW','[RIM-W] Screened interaction RS components',RIM_W_ng,G_unit)
 call it(defs,'rimw_type','[RIM-W] Screened interaction limit metal/semiconductor/graphene',rimw_type)
 !
 ! CUTOFF
 !
 call it(defs,'CUTGeo',   '[CUT] Coulomb Cutoff geometry: box/cylinder/sphere/ws/slab X/Y/Z/XY..',cut_geometry)
 call it(defs,'CUTBox',   '[CUT] [au] Box sides',box_length)
 call it(defs,'CUTRadius','[CUT] [au] Sphere/Cylinder radius',cyl_ph_radius)
 call it(defs,'CUTCylLen','[CUT] [au] Cylinder length',cyl_length)
 call it(defs,'CUTwsGvec','[CUT] WS cutoff: number of G to be modified',ws_cutoff)
 call it('f',defs,'CUTCol_test','[CUT] Perform a cutoff test in R-space',verb_level=V_RL)
 !
 ! S_xc
 !
 call it(defs,'GreenFTresh','[GW] [o/o] Treshold to define the new zoomed energy range',QP_G_Zoom_treshold,verb_level=V_qp)
 call it('f',defs,'QPExpand',   '[F GW] The QP corrections are expanded all over the BZ',verb_level=V_qp)
 call it('f',defs,'GreenF2QP',  '[F GW] Use real axis Green`s function to define the QPs',verb_level=V_qp)
 call it('f',defs,'OnMassShell','[F GW] On mass shell approximation',verb_level=V_qp)
 !
#if defined _SCALAPACK
 !
 ! SLK test
 !
 call it(defs,'SLKdim',  '[SLK] Matrix Dimension',SLK_test_H_dim)
#endif
 !
 ! GPU test
 !
 call it(defs,'GPUdim',  '[GPU] Matrix Dimension',GPU_test_dim)
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
 call it(defs,'BSehWind','[BSK] [o/o] E/h coupling pairs energy window',BS_eh_win,verb_level=V_resp)
 call it(defs,'BSEQptR', '[BSK] Transferred momenta range',BS_q,verb_level=0)
 call it(defs,'BDmERef', '[BSS] Damping energy reference',BSS_damp_reference,E_unit,verb_level=V_resp)
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
 call it(defs,'DipBands',       '[DIP] Bands range for dipoles',Dip%ib,verb_level=V_resp)
 call it(defs,'DipQpts',        '[DIP] Qpts range for dipoles',Dip%iq,verb_level=V_resp)
 call it(defs,'DipoleEtresh',   '[DIP] Treshold in the definition of R=P/deltaE',Dip%Energy_treshold,&
&                                                                               verb_level=V_resp,unit=E_unit)
 call it(defs,'DipApproach',    '[DIP] [G-space v/R-space x/Covariant/Shifted grids]',Dip%approach,verb_level=V_resp)
 call it(defs,'DipComputed',    '[DIP] [default R P V; extra P2 Spin Orb]',Dip%computed,verb_level=V_resp)
 call it(defs,'ShiftedPaths',   '[DIP] Shifted grids paths (separated by a space)',grid_paths,verb_level=V_resp)
 call it('f',defs,'DipPDirect', '[DIP] Directly compute <v> also when using other approaches for dipoles',verb_level=V_resp)
 call it('f',defs,'DipBandsALL','[DIP] Compute all bands range, not only valence and conduction',verb_level=V_resp)
 !
 call it(defs,'SCEtresh',  '[SC] Energy convergence threshold for SC-GW',SC_E_threshold,unit=E_unit,verb_level=V_qp)
 !
 if (initmode==4) then
   call LEVELS_mirror("MIRROR_q",en,X,Dip,l_USER_fields)
   call LEVELS_mirror("MIRROR_bands",en,X,Dip,l_USER_fields)
 endif
 ! 
 contains 
   !
#if defined _MPI 
   !
   subroutine CPU_structure_load()
     use parallel_m, ONLY:n_CPU_str_max,CPU_structure,PARALLEL_default_mode
     integer           :: i_s
     character(schlen) :: name,possible_fields
     !
     call it(defs,"PAR_def_mode",'[PARALLEL] Default distribution mode ("balanced"/"memory"/"workload"/"KQmemory")',&
&            PARALLEL_default_mode,verb_level=V_parallel)
     !
     do i_s=1,n_CPU_str_max 
       !
       if (i_s==1) possible_fields='(k,c,v)'     ! DIP
       if (i_s==2) possible_fields='(q,g,k,c,v)' ! X_and_IO
       if (i_s==3) possible_fields='(q,g,k,c,v)' ! X
       if (i_s==4) possible_fields='(k,eh,t)'    ! BS
       if (i_s==5) possible_fields='(q,g,qp,b)'  ! SE
       if (i_s==6) possible_fields='(k,b,q,qp)'  ! RT
       if (i_s==7) possible_fields='(p,d) '      ! SLK
       if (i_s==8) possible_fields='(w,k) '      ! NL
       if (i_s==9) possible_fields='(k) '        ! BZ
       if (i_s==11)possible_fields='(k,b) '      ! OSCLL
       !
       if (len_trim(CPU_structure(i_s)%Short_Description)==0) cycle
       name=trim(CPU_structure(i_s)%Short_Description)//"_CPU"
       call it(defs,trim(name),'[PARALLEL] CPUs for each role',&
&              CPU_structure(i_s)%CPU_string,verb_level=V_parallel)
       name=trim(CPU_structure(i_s)%Short_Description)//"_ROLEs"
       call it(defs,trim(name),'[PARALLEL] CPUs roles '//trim(possible_fields),&
&              CPU_structure(i_s)%ROLE_string,verb_level=V_parallel)
       name=trim(CPU_structure(i_s)%Short_Description)//"_nCPU_LinAlg_INV"
       call it(defs,trim(name),'[PARALLEL] CPUs for Linear Algebra (if -1 it is automatically set)',&
&              CPU_structure(i_s)%nCPU_lin_algebra_INV,verb_level=V_parallel)
       name=trim(CPU_structure(i_s)%Short_Description)//"_nCPU_LinAlg_DIAGO"
       call it(defs,trim(name),'[PARALLEL] CPUs for Linear Algebra (if -1 it is automatically set)',&
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
     integer           ::i1,V_iq,V_DmRnge
     integer,parameter ::n_des=18
     character(8)      ::nms(n_des),sfx
     character(100)    ::des(n_des)
     character(schlen) ::lch1,lch2
     !
     nms=(/'GrFnTp','EnSamp','EnGrid','EnRnge','ImRnge','DmRnge','CGrdSp','ETStps','IntSol','EMStps',&
&          'DrudeW','LongDr','EhEngy','PPAPnt',&
&          'NGsBlk','QpntsR','BndsRn','DmERef'/)
     des(1) ='Green`s function (T)ordered,(R)etarded,(r)senant,(a)ntiresonant [T, R, r, Ta, Ra]'
     des(2) ='Frequency sampling in the complex plane ("1l" one line, "2l" two lines)'
     des(3) ='Partition along the real axis ("ho" homogeneous, "lP" linear, "qP" quadratic, "cP" cubic)'
     des(4) ='Energy range'
     des(5) ='Imaginary range'
     des(6) ='Damping range'
     des(7) ='[o/o] Coarse grid controller'
     des(8) ='Total Energy steps'
     des(9) ='MPA interpolation solver ("LA" linear algebra, "PT" Pade-Thiele)'
     des(10)='[o/o] Memory Energy steps'
     des(11)='Drude plasmon'
     des(12)='[cc] Electric Field'
     des(13)='Electron-hole energy range'
     des(14)='PPA imaginary energy'
     des(15)='Response block size'
     des(16)='Transferred momenta'
     des(17)='Polarization function bands'
     des(18)='Damping reference energy'
     !
     do i1=1,n_des
       lch1=des(i1)
       lch2=nms(i1)
       if (X%whoami==1) sfx='Xx'
       if (X%whoami==2) sfx='Xs'
       if (X%whoami==3) sfx='Xd'
       if (X%whoami==4) sfx='Xp'
       if (X%whoami==5) sfx='Xm'
       write (des(i1),'(4a)')  '[',trim(sfx),'] ',trim(lch1)
       write (nms(i1),'(2a)')  lch2(1:6),trim(sfx)
     enddo
     !
     V_iq     = 0
     V_DmRnge = 0
     !
     if (.not.X%whoami==3.and.(.not.X%whoami==5.or.X%whoami==1)) then
       V_iq     = V_resp 
       V_DmRnge = V_resp
     endif
     !
     call it(defs,nms( 1),des( 1),X%ordering,verb_level=V_resp)
     call it(defs,nms( 2),des( 2),wv%samp_type,verb_level=0)
     call it(defs,nms( 3),des( 3),wv%samp_grid,verb_level=0) 
     call it(defs,nms( 4),des( 4),wv%er,E_unit,verb_level=0) 
     call it(defs,nms( 5),des( 5),wv%ir,E_unit,verb_level=0) 
     call it(defs,nms( 6),des( 6),wv%dr,E_unit,verb_level=V_DmRnge)
     call it(defs,nms( 7),des( 7),X%cg_percentual,verb_level=V_resp)
     call it(defs,nms( 8),des( 8),wv%n_freqs,verb_level=0)
     call it(defs,nms( 9),des( 9),wv%mpa_solver,verb_level=0) 
     call it(defs,nms(10),des(10),wv%per_memstps,verb_level=V_resp)
     call it(defs,nms(11),des(11),X%Wd,E_unit,verb_level=V_resp)
     call it(defs,nms(12),des(12),X%q0,verb_level=0)
     call it(defs,nms(13),des(13),X%ehe,E_unit,verb_level=V_resp)
     call it(defs,nms(14),des(14),X%ppaE,E_unit,verb_level=0)
     call it(defs,nms(15),des(15),X%ng,G_unit,verb_level=0)
     call it(defs,nms(16),des(16),X%iq,verb_level=V_iq)
     call it(defs,nms(17),des(17),X%ib,verb_level=0)
     call it(defs,nms(18),des(18),wv%damp_reference,E_unit,verb_level=V_resp)
     !
   end subroutine
   !
end subroutine
