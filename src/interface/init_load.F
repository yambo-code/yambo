!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
 use it_m,          ONLY:it,initdefs,E_unit,G_unit,T_unit,B_unit,&
&                        Time_unit,I_unit,&
&                        V_RL,V_kpt,V_sc,V_qp,V_io,V_general,V_resp 
 use X_m,           ONLY:X_t,long_path,q_plus_G_direction,Q_Shift_Order
 use QP_m,          ONLY:QP_cg_percent,QP_G_damp,QP_solver,&
&                        QP_n_G_bands,QP_ng_Sx,Sx_low_band,&
&                        QP_G_er,QP_G_dr,QP_Sc_steps,GWo_iterations,SC_band_mixing,&
&                        SC_E_threshold,QP_dSc_delta
 use timing,        ONLY:nhash
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:Tel,non_periodic_directions
 use R_lattice,     ONLY:ng_closed,QP_states_k,nXkibz,&
&                        bz_samp,RIM_ng,RIM_epsm1,RIM_id_epsm1_reference,&
&                        RIM_n_rand_pts,cyl_ph_radius,box_length,cyl_length,cut_geometry
 use BS,            ONLY:BS_res_mode,BS_eh_en,BS_eh_win,BS_q,BS_bands,BS_columns,&
&                        BS_n_g_W,BS_n_g_exch,BSS_mode,Haydock_treshold,BSS_n_freqs,&
&                        BSS_dr,BSS_er,BSS_q0,BS_cpl_mode,BSS_damp_reference
 use TDDFT,         ONLY:FXC_n_g_corr,FXC_per_memstps,FXC_LRC_alpha,&
&                        FXC_SVD_digits
 use ACFDT,         ONLY:ACFDT_n_lambda,ACFDT_n_freqs,ACFDT_E_range
 use com,           ONLY:more_io_path,com_path
 use functions,     ONLY:bose_E_cut
#if defined _SC || defined _RT || defined _MAGNETIC
 use SC,            ONLY:SC_bands,SC_iterations,SC_potential,&
&                        SC_rho_threshold,SC_cycle_mixing
#endif
#if defined _RT
 use real_time,     ONLY:RT_step, Tot_steps, Thermal_steps, Integrator_name, LifeTime, &
&                        DiagH_Iter,Thermal_eta,EField,max_rt_steps_disk

#endif
#if defined _MAGNETIC
 use magnetic,      ONLY:MAG_B_z,MAG_hamiltonian_type,MAG_radius,MAG_GAUGE
#endif
#if defined _RAS
 use bulkeps,              ONLY : init_bulk
 use eels_kinematics,      ONLY : init_kinematics
 use eels_detector,        ONLY : init_detector
 use optcut,               ONLY : init_cutoff
 use surface_geometry,     ONLY : init_surface
 use analyse_peaks,        ONLY : init_analyse_peaks
 use model_loss_function,  ONLY : init_loss_function
 use convolute,            ONLY : init_convolute
 use ras_module,           ONLY : locfac, loctype
#endif
#if defined _REELS
 use bulkeps,              ONLY : init_bulk
 use eels_kinematics,      ONLY : init_kinematics
 use eels_detector,        ONLY : init_detector
 use surface_geometry,     ONLY : init_surface
 use convolute,            ONLY : init_convolute
 use reels_module,         ONLY : init_reels
 use eels_integration,     ONLY : init_reels_integration
#endif
#if defined _ELPH 
 use ELPH,                 ONLY : QP_PH_n_G_bands,elph_nq_used
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
 call it('r',defs,'bse',     '[R BSK] Bethe Salpeter Equation.')
 call it('r',defs,'HF_and_locXC',   '[R XX] Hartree-Fock Self-energy and Vxc')
 call it('r',defs,'em1s',    '[R Xs] Static Inverse Dielectric Matrix')
 call it('r',defs,'em1d',    '[R Xd] Dynamical Inverse Dielectric Matrix')
 call it('r',defs,'ppa',     '[R Xp] Plasmon Pole Approximation')
 call it('r',defs,'gw0',     '[R GW] GoWo Quasiparticle energy levels')
 call it('r',defs,'bss',     '[R BSS] Bethe Salpeter Equation solver')
 call it('r',defs,'alda_fxc','[R TDDFT] The ALDA TDDFT kernel')
 call it('r',defs,'lrc_fxc', '[R TDDFT] The LRC TDDFT kernel')
 call it('r',defs,'rim_cut', '[R RIM CUT] Coulomb interaction')
 call it('r',defs,'tdhf',    '[R TDHF] Unscreened e-h interaction: TD Hartree-Fock')
 !
 ! GPL_EXCLUDE_START SC_IGNORE
 !
 call it('r',defs,'chosex',  '[R Xp] COlumb Hole Screened EXchange')
 !
 ! GPL_EXCLUDE_END SC_IGNORE
 !
 ! GPL_EXCLUDE_START
 !
 !===================
 ! NON GPL RUNLEVELS
 !===================
 call it('r',defs,'life',    '[R GW] GoWo Quasiparticle lifetimes')
 call it('r',defs,'acfdt',   '[R ETOT] ACFDT Total Energy')
 call it('r',defs,'bs_fxc',  '[R TDDFT] The BSE TDDFT kernel')
 !
 ! GPL_EXCLUDE_END
 !
#if defined _SC || defined _RT || defined _MAGNETIC
 call it('r',defs,'scpot',   '[R] Self-Consistent potentials') 
#endif
#if defined _RT
 call it('r',defs,'negf',    '[R] Real-Time dynamics') 
 call it('r',defs,'rhoIO',   '[R] Extended oscillators IO')
#endif
#if defined _MAGNETIC
 call it('r',defs,'magnetic','[R] Magnetic fields')
#endif
#if defined _RAS
 call it('r',defs,'sursp',  '[R] Surface Spectroscopy ') 
 call it('r',defs,'reels',  '[R] Reflection EELS (3-layer model) ') 
 call it('r',defs,'ras',    '[R] Reflectance anisotropy spectroscopy (RAS)')   
#endif
#if defined _REELS
 call it('r',defs,'reels',  '[R] Surface Spectroscopy (REELS)') 
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
 call it(defs,'MaxGvecs','[INI] Max number of G-vectors planned to use',&
&                        ng_closed,unit=G_unit,verb_level=V_RL)
 call it(defs,'FFTGvecs','[FFT] Plane-waves',wf_ng,G_unit,verb_level=V_RL)
 call it(defs,'NonPDirs','[X/BSS] Non periodic chartesian directions (X,Y,Z,XY...)',non_periodic_directions,Verb_level=V_resp)
 call it(defs,'IkSigLim','[KPT] QP K-points indices range',QP_states_k,verb_level=V_kpt)
 call it(defs,'IkXLim',  '[KPT] X grid last k-point index',nXkibz,verb_level=V_kpt) 
 !
 ! S_xc
 !
 call it(defs,'EXXRLvcs', '[XX] Exchange RL components',QP_ng_Sx,G_unit)
 call it(defs,'GbndRnge', '[GW] G[W] bands range',QP_n_G_bands)
 call it(defs,'GDamping', '[GW] G[W] damping',QP_G_damp,E_unit)
 call it(defs,'GDmRnge',  '[GW] G_gw damping range',QP_G_dr,E_unit)
 call it(defs,'dScStep',  '[GW] Energy step to evalute Z factors',QP_dSc_delta,E_unit)
 !
 ! GPL_EXCLUDE_START GW_REAL_AXIS_IGNORE
 !
 call it(defs,'DysSolver','[GW] Dyson Equation solver (`n`,`s`,`g`)',QP_solver,protect=.FALSE.)
 !
 ! GPL_EXCLUDE_END GW_REAL_AXIS_IGNORE
 !
 !
 ! BSE
 !
 call it(defs,'BSresKmod',  '[BSK] Resonant Kernel mode. (`x`;`c`;`d`)',BS_res_mode)
 call it(defs,'BSEBands','[BSK] Bands range',BS_bands)
 call it(defs,'BSENGBlk','[BSK] Screened interaction block size',BS_n_g_W,G_unit)
 call it(defs,'BSENGexx','[BSK] Exchange components',BS_n_g_exch,G_unit)
 call it(defs,'BSSmod',  '[BSS] Solvers `h/d/i/t`',BSS_mode,protect=.FALSE.)
 call it(defs,'BLongDir','[BSS] [cc] Electric Field',BSS_q0)
 call it(defs,'BEnRange','[BSS] Energy range',BSS_er,E_unit)
 call it(defs,'BDmRange','[BSS] Damping range',BSS_dr,E_unit)
 call it(defs,'BSHayTrs','[BSS] [o/o] Haydock treshold. Strict(>0)/Average(<0)',&
&              Haydock_treshold,verb_level=V_resp)
 call it(defs,'BEnSteps','[BSS] Energy steps',BSS_n_freqs)
 call it('f',defs,'WRbsWF', '[BSS] Write to disk excitonic the FWs',verb_level=V_resp)
 !
 ! F_xc
 !
 call it(defs,'FxcGRLc',  '[TDDFT] XC-kernel RL size',FXC_n_g_corr,G_unit)
 call it(defs,'LRC_alpha','[TDDFT] LRC alpha factor',FXC_LRC_alpha)
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
 call it(defs,'QShiftOrder', '[Xd] Pick-up the (QShiftOrder)th q+G vector',Q_Shift_Order,verb_level=V_resp)
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
 ! I/O
 !
 call it(defs,'More_IO_Path', '[IO] Additional I/O directory',more_io_path,verb_level=V_io)
 call it(defs,'Com_Path',     '[IO] Communication directory',com_path,verb_level=V_io)
 !
 ! Basics
 !
 call it(defs,'Nelectro','Electrons number',nel,verb_level=V_general)
 call it(defs,'ElecTemp','Electronic Temperature',Tel,T_unit,verb_level=V_general)
 call it(defs,'OccTresh','Occupation treshold (metallic bands)',filled_tresh,verb_level=V_general)
 call it(defs,'BoseCut', '[BOSE] Finite Tel Bose function cutoff',bose_E_cut,verb_level=V_general)
#if defined _ELPH 
 call it('f',defs,'MinusQ', '[KPT] Use -{q} grid',verb_level=V_kpt)
 call it(defs,'ElPhRndNq',  '[ELPH] Read random Q-points',elph_nq_used,verb_level=V_kpt) 
#endif
 !
 ! S_xc
 !
 call it(defs,'SxLowBnd', '[XX] Exchange summation low band',Sx_low_band,verb_level=V_qp)
 call it(defs,'LifeTrCG', '[GW] [o/o] Lifetime transitions reduction',QP_cg_percent)
 call it(defs,'GEnSteps', '[GW] G_gw energy steps',QP_Sc_steps)
 call it(defs,'GEnRnge',  '[GW] G_gw energy range (centered in the bare energy)',QP_G_er,E_unit)
 call it(defs,'GWoIter',  '[GW] GWo SC iterations',GWo_iterations,verb_level=V_qp)
 call it('f',defs,'NewtDchk',   '[F GW] Test dSc/dw convergence',verb_level=V_qp)
 call it('f',defs,'OnMassShell','[F GW] On mass shell approximation',verb_level=V_qp)
 call it('f',defs,'ForceNewton','[F GW] Uses Newton expression for the QP energy',verb_level=V_qp)
 call it('f',defs,'GreenF2QP',  '[F GW] Use real axis Green`s function to define the QPs',verb_level=V_qp)
 call it('f',defs,'ExtendOut',  '[F GW] Print all variables in the output file',verb_level=V_qp)
 !
 ! GPL_EXCLUDE_END
 !
 ! GPL_EXCLUDE_START SC_IGNORE
 !
 call it('f',defs,'UseEbands', '[GW] Force CHOSEX to use empty bands',verb_level=V_qp)
 !
 ! Real Time dynamics
 !
#if defined _RT
 call it(defs,'RTstep',    '[RT] Real Time step',RT_step,unit=Time_unit)
 call it(defs,'RTsteps',   '[RT] Real Time steps',Tot_steps)
 call it(defs,'ThermStps', '[RT] Thermal Steps',Thermal_steps)
 !
 ! External field number 1
 !
 call it(defs,'ExtFiFreq1', '[RT] External Field Frequency',EField(1)%frequency,unit=E_unit)
 call it(defs,'ExtFInt1',  '[RT] External Field Intensity',EField(1)%intensity,unit=I_unit)
 call it(defs,'ExtField1',  '[RT] External Field (SINUSOIDAL | RESONANT | DELTA | STEP)', &
&        EField(1)%ef_name)
 call it(defs,'ELongDir1',  '[RT] Electric Field Versor',Efield(1)%versor)
 call it(defs,'ExtFTinitial1',  '[RT] Initial Time',EField(1)%t_initial,unit=Time_unit)
 call it('f',defs,'EFNoIntraband1', '[RT] Do not include intraband coupling',verb_level=V_sc)
 !
 ! External field number 2
 !
 call it(defs,'ExtFiFreq2', '[RT] External Field Frequency',EField(2)%frequency,unit=E_unit,verb_level=V_sc)
 call it(defs,'ExtFInt2',  '[RT] External Field Intensity',EField(2)%intensity,verb_level=V_sc,unit=I_unit)
 call it(defs,'ExtField2',  '[RT] External Field',EField(2)%ef_name,verb_level=V_sc)
 call it(defs,'ELongDir2',  '[RT] Electric Field Versor',Efield(2)%versor,verb_level=V_sc)
 call it(defs,'ExtFTinitial2',  '[RT] Initial Time',EField(2)%t_initial,unit=Time_unit,verb_level=V_sc)
 call it('f',defs,'EFNoIntraband2', '[RT] Do not include intraband coupling',verb_level=V_sc)
 !
 call it(defs,'Integrator','[RT] Integrator (RK2 | EULER | EXACT | RK2EXACT) ',Integrator_name)
 call it(defs,'ThermEta',  '[RT] Adiabatic switching characteristic time  1/(ThermStps*ThermEta)',Thermal_eta,verb_level=V_sc)
 call it(defs,'DiagHIter', '[RT] Digaonalize the Hamiltonian',DiagH_Iter,verb_level=V_sc)
 call it(defs,'LifeTime',  '[RT] Quasi-particle Relaxation Time',LifeTime,verb_level=V_sc,unit=Time_unit)
 call it('f',defs,'RTQpAnalyze', '[RT] Analyze quasi-particle dynamics in real-time',verb_level=V_sc)
 call it('f',defs,'KeepVxc', '[RT] Do not subtract Vxc from the self-energy',verb_level=V_sc)
 call it('f',defs,'NoDiamagnetic', '[RT] Exclude the diamagnetic term in current and coupling',verb_level=V_sc)
 call it(defs,'CacheSize', '[RT] Numer of steps stored in memory before I/O',max_rt_steps_disk,verb_level=V_sc)
#endif
 !
#if defined _MAGNETIC
 call it(defs,'Hamiltonian','[MAG] Hamiltonian kind [pauli,landau]',MAG_hamiltonian_type,protect=.FALSE.)
 call it(defs,'B_z_Field',  '[MAG] Magnetic field z component',MAG_B_z,unit=B_unit)
 call it(defs,'B_radius',   '[MAG] Magnetic flux radius',MAG_radius)
 call it(defs,'Gauge',      '[MAG] Gauge ("SYMM"etric, "X_ASYMM"etric and "Y_ASYMM"etric)',MAG_gauge)
#endif
 !
 ! GPL_EXCLUDE_END SC_IGNORE
 !
 ! QP ctl 
 !
 call QP_ctl_load(1)
 call QP_ctl_load(2)
 call QP_ctl_load(3)
 !
 ! GPL_EXCLUDE_START
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
 call it(defs,'BScplKmod',  '[BSK] Coupling Kernel mode. (`x`;`c`;`d`;`u`)',BS_cpl_mode)
 call it(defs,'BSEEhEny','[BSK] Electron-hole energy range',&
&        BS_eh_en,E_unit,verb_level=V_resp)
 call it(defs,'BSehWind','[BSK] [o/o] E/h coupling pairs energy window',&
&        BS_eh_win,verb_level=V_resp)
 call it(defs,'BSEQptR', '[BSK] Transferred momenta range',BS_q,verb_level=V_resp)
 call it(defs,'BSEClmns','[BSK] Kernel Columns',BS_columns,verb_level=V_resp)
 call it(defs,'BDmERef', '[BSS] Damping energy reference',BSS_damp_reference,&
&                        E_unit,verb_level=V_resp)
 call it('f',defs,'SkipFullInv',  '[BSS] Skip full invertion in perturbative solver',verb_level=V_resp)
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
 call it(defs,'LongPath',    '[Xd] Longitudinal gauge path',long_path,verb_level=V_resp)
 !
 ! GPL_EXCLUDE_END
 !
 ! El-Ph
 !
#if defined _ELPH 
 call it(defs,'GphBRnge',  '[ELPH] G[W] bands range',QP_PH_n_G_bands)
 call it('f',defs,'WRgFsq','[ELPH] Dump on file gFsq coefficients')
#endif
 ! 
 ! SC
 !
#if defined _SC || defined _MAGNETIC
 call it(defs,'SCIter',    '[SC] SC Iterations',SC_iterations)
 call it(defs,'SCEtresh',  '[SC] Energy convergence threshold',SC_E_threshold,unit=E_unit)
 call it(defs,'SCRhoTresh','[SC] Rho convergence threshold',SC_rho_threshold)
 call it('f',defs,'TF_precondition','[SC] Thomas Fermi precondition in density',verb_level=V_sc)
 call it('f',defs,'UseRhoIO',    '[SC] Use Extended Oscillators',verb_level=V_sc)
#endif
 ! 
 ! SC [common with RT]
 !
#if defined _SC || defined _RT || defined _MAGNETIC
 call it(defs,'SCBands',   '[SC] Bands',SC_bands)
 call it(defs,'SCmixing',  '[SC] SC Cycle Mixing (< 1.)',SC_cycle_mixing,verb_level=V_sc)
 call it(defs,'SCPot',     '[SC] SC Potential',SC_potential,protect=.FALSE.)
 call it(defs,'BandMix',   '[SC] Band mixing',SC_band_mixing)
#endif
 ! 
 ! RAS
 !
#if defined _RAS
 call init_bulk(defs)
 call init_surface(defs)
 call init_analyse_peaks(defs)
 call init_cutoff(defs)
 call init_kinematics(defs) 
 call init_loss_function(defs)
 call init_detector(defs)
 call init_convolute(defs)
 call it(defs,'LocType','[RAS] Transition filter (ss/sb/bs/bb|no)',loctype,verb_level=V_resp)
 call it(defs,'LocLimit','[RAS] Factor defining state localization',locfac,verb_level=V_resp)
#endif
 ! 
 ! REELS
 !
#if defined _REELS
 call init_bulk(defs)
 call init_surface(defs) 
 call init_kinematics(defs)
 call init_detector(defs)
 call init_reels(defs)
 call init_reels_integration(defs)
 call init_convolute(defs)
#endif
 !
 contains 
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
   subroutine QP_ctl_load(i_type)
     !
     ! Note that this routine is called in 'load' mode BEFORE
     ! the DB1 is read, so before n_sp_pol is set to the correct value.
     ! 
     ! Thus we must call it on the the no spin, spin + and spin - cases 
     ! because the code does not know if there is
     ! spin unless the DB1 is read. 
     !
     use electrons,     ONLY:n_sp_pol
     use it_m,          ONLY:initmode
     use QP_m,          ONLY:QP_ctl_E,QP_ctl_db,QP_ctl_interp_neigh,QP_ctl_W,QP_ctl_Z
     integer           ::i_type
     ! 
     ! Work Space
     ! 
     integer           ::i_spin,i1,i1_range(2)
     character(1)      ::prefix(3)
     character(3)      ::spin_ch(3)
     character(7)      ::description_ch(3)
     character(9)      ::spin_description_ch(3)
     !
     spin_ch=(/'   ','_up','_dn'/)
     spin_description_ch=(/'       ','spin up','spin dn'/)
     prefix=(/'X','K','G'/)
     description_ch(1)='Xd'
     description_ch(2)='BSK BSS'
     description_ch(3)='G'
     !
     if (initmode==0) i1_range=(/1,3/)
     if (initmode/=0.and.n_sp_pol==1) i1_range=1
     if (initmode/=0.and.n_sp_pol==2) i1_range=(/2,3/)
     !
     do i1=i1_range(1),i1_range(2)
       ! 
       if (i1==1) i_spin=1
       if (i1> 1) i_spin=i1-1
       !
       call it(defs,&
&              prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_E',&
&              '[EXTQP '//trim(description_ch(i_type))//'] E parameters '//&
&              trim(spin_description_ch(i1))//' (c/v)',&
&              QP_ctl_E(i_type,:,i_spin),verb_level=V_qp)
       !
       call it(defs,&
&              prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_W',&
&              '[EXTQP '//trim(description_ch(i_type))//'] W parameters '//&
&              trim(spin_description_ch(i1))//' (c/v)',&
&              QP_ctl_W(i_type,:,i_spin),verb_level=V_qp)
       !
       call it(defs,&
&              prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_Z',&
&              '[EXTQP '//trim(description_ch(i_type))//'] Z factor '//&
&              trim(spin_description_ch(i1))//' (c/v)',&
&              QP_ctl_Z(i_type,i_spin),verb_level=V_qp)
       !
     enddo
     !
     call it(defs,prefix(i_type)//'fnQPdb',&
&            '[EXTQP '//trim(description_ch(i_type))//'] Database',&
&            QP_ctl_db(i_type),verb_level=V_qp)
     !
     call it(defs,prefix(i_type)//'fnQP_N',&
&            '[EXTQP '//trim(description_ch(i_type))//'] Interpolation neighbours',&
&            QP_ctl_interp_neigh(i_type),verb_level=V_qp)
     !
   end subroutine
   !
end subroutine
