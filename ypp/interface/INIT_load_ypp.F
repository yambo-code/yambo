!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_ypp_load(defs)
 !
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,E_unit,G_unit,V_general,V_RL,&
&                        Time_unit,Bfield_unit,V_real_time,T_unit,V_nl_optics,V_io,V_sc
 use wave_func,     ONLY:wf_ng
 use LIVE_t,        ONLY:nhash
 use D_lattice,     ONLY:Tel,Bose_Temp
 use R_lattice,     ONLY:BZ_FineGd_mode
 use electrons,     ONLY:n_spinor
 use interpolate,   ONLY:INTERP_shell_factor,INTERP_mode,NN_n_of_nearest
 use YPPm,          ONLY:E_field,alat_used_for_output,B_field,B_psi,B_theta,BANDS_bands,BANDS_path,&
&                        BZ_FineGd_nkpts_to_use,BZ_random_nkpts,CIRCUIT_E_DB_path,coo_in,coo_out,current_dir,&
&                        deg_energy,BANDS_steps,DOS_bands,DOS_broadening,PROJECT_path,QP_DB_to_expand,&
&                        DOS_E_range,DOS_E_steps,EXCITON_E_treshold,EXCITON_Res_treshold,EXCITON_weight_treshold,&
&                        INTERP_grid,mag_dir,N_path_pts,ncell,p_dir,p_format,PDOS_atoms,&
&                        PDOS_j,PDOS_wfcs,PDOS_kinds,PDOS_l,PDOS_m,perturbative_SOC_path,PtsPath,r_hole,seed_name,&
&                        SOC_bands_buffer,SOC_bands_to_map,SOC_split,state_ctl,AVE_exc,BSiq_via_command_line,&
&                        WF_multiplier,WF_ref,what_to_write,BSiq,PROJECT_mode,PROJECT_N_perp,PROJECT_steps,PROJECT_threshold,&
&                        DIPs_direction,DIPs_E_range,DIPs_C_bands,DIPs_V_bands,DIPs_kind
 use YPP_symm,      ONLY:wf_ng_cut
#if defined _YPP_RT
 use YPPm,          ONLY:Nel_fac
 use YPP_real_time, ONLY:RT_conf,Eh_pumped_Nel,Eh_pumped_pair_deltaE,Rho_deph,&
&                        Eh_pumped_pair_width,Probe_Keyword,Pump_Keyword,X_order,X_kind,Eh_pumped_pair_energy,&
&                        Eh_pumped_pair_BZ_width,Eh_mu,Eh_temp,h_mu_autotune_thr,TRabsWHAT,TRabsDIP_plane,&
&                        TRabsMODE,TRabsDIP_dir,TRabs_Eeh_treshold,Pump_path,RT_pol_mode
 use real_time,     ONLY:RT_bands
#endif
#if defined _YPP_SC
 use YPPm,          ONLY:V_value
 use SC,            ONLY:SC_neq_kind
#endif
#if defined _YPP_ELPH
 use YPP_ELPH,      ONLY:elph_dbs_path,elph_Ef,elph_gamma_broad,elph_residual_treshold,elph_steps,&
&                        ph_broad,ph_freqs_file,ph_modes_file
 use ELPH,          ONLY:EkplusQ_mode,elph_branches
#endif
 !
 implicit none
 !
 type(initdefs) :: defs
 !
 ! RunLevels...
 !==============
 !
 ! ... from C_driver
 !
 call INIT_C_driver_runlevels(defs)
 !
 ! ... derived
 !=============
 !
 call it('r',defs,'K_grid',    '[R] K-grid analysis') 
 call it('r',defs,'Q_grid',    '[R] Q-grid analysis') 
 call it('r',defs,'dos',       '[R] DOS')
 call it('r',defs,'Shifted_Grid','[R] Shifted grids') 
 call it('r',defs,'Random_Grid', '[R] Random grids')
 call it('r',defs,'High_Symm',   '[R] High_Symmetry points and paths') 
 call it('r',defs,'Regular_Grid','[R] Regular grid')
 call it('r',defs,'amplitude',   '[R] Amplitude')
 call it('r',defs,'plot',        '[R] Plot')
 call it('r',defs,'density',     '[R] Density')
 call it('r',defs,'current',     '[R] Current')
 call it('r',defs,'bnds',        '[R] Bands')
 call it('r',defs,'wavefunction','[R] Wavefunction')
 call it('r',defs,'interpolate', '[R] Interpolate')
#if defined _YPP_ELPH
 call it('r',defs,'eliashberg', '[R] Eliashberg')
 call it('r',defs,'gkkp_db',    '[R] GKKP database')
 call it('r',defs,'gkkp_dg',    '[R] GKKP double grid')
 call it('r',defs,'gkkp_plot',  '[R] GKKP plot')
 call it('r',defs,'ph_ass_dos', '[R] Phonon-assisted DOS')
#endif
#if defined _YPP_SC
 call it('r',defs,'angular_m',    '[R] Canonical angular momentum')
 call it('r',defs,'position',     '[R] Average <r^n>, n given in the input')
#endif
 call INIT_QP_ctl_load(defs,3)
 call it('r',defs,'spin',         '[R] Spin')
 call it('r',defs,'magnetization','[R] Magnetization')
 call it('r',defs,'WFs_map',      '[R] Creat a SOC/WFs <-> noSOC/WFs map')
 call it('r',defs,'QPDB_edit',    '[R] Generation/editing')
 call it('r',defs,'QPDB_merge',   '[R] Mergering')
 call it('r',defs,'QPDB_expand',  '[R] Symmetry expand')
#if defined _YPP_RT
 call it('r',defs,'RealTime',     '[R] TD Post-Processing')
 call it('r',defs,'RT_X',         '[R] Response functions Post-Processing')
 call it('r',defs,'RT_ph_osc',    '[R] Coherent phonon oscillations')
 !
 call it('r',defs,'Select_energy','[R] NEQ DBs from input energy window')
 call it('r',defs,'Select_manual','[R] NEQ DBs manually created')
 call it('r',defs,'Select_Fermi', '[R] NEQ DBs according to Fermi distribution')
 !
 call it('r',defs,'RTabs',        '[R] Transient absorption')
 call it('r',defs,'RToccupations','[R] Analize time-dependent occupations')
 call it('r',defs,'RTlifetimes',  '[R] Analize time-dependent lifetimes')
 call it('r',defs,'RTpol',        '[R] Analize time-dependent polarization')
 call it('r',defs,'RTdeltaRho',   '[R] Analize the density')
 call it('r',defs,'RTGtwotimes',  '[R] Construct G</G>/G(r)/G(a)/A(t,tp) from rho within GKBA')
 !
 call it('r',defs,'RTfitbands',   '[R] Post-Processing kind: fit on bands')
 call it('r',defs,'RTtime',       '[R] Post-Processing kind: function of time')
 call it('r',defs,'RTenergy',     '[R] Post-Processing kind: function of energy')
 call it('r',defs,'RTdos',        '[R] Post-Processing kind: 2D DOS, function of time and energy')
#endif
 !
#if defined _YPP_NL
 call it('r',defs,'NL_X',     '[R] NonLinear Optics Response Post-Processing')
 call it('r',defs,'NL_exc',   '[R] NonLinear Optics Excitation Post-Processing')
#endif
 !
 ! Variables...
 !==============
 call it(defs,'StdoHash','[IO] Live-timing Hashes',nhash,verb_level=V_io)
 !
#if defined _YPP_NL
 call it('f',defs,'PrtPwrSpec','Print power spectrum',verb_level=V_nl_optics)
 call it(defs,'PumpPATH',"Path of the simulation with the Pump only",Pump_path)
#endif
 !
 ! Temperatures
 !
 call it(defs,'ElecTemp','Electronic Temperature',Tel,T_unit,verb_level=V_general)
 call it(defs,'BoseTemp','Bosonic Temperature',Bose_Temp,T_unit,verb_level=V_general)
 !
 ! DOS
 !
 call it(defs,'DOS_broad', 'Broadening of the DOS',DOS_broadening,E_unit) 
 call it(defs,'DOS_bands', 'DOS bands',DOS_bands) 
 call it(defs,'DOSERange', 'Energy range',DOS_E_range,unit=E_unit) 
 call it(defs,'DOSESteps', 'Energy steps',DOS_E_steps) 
 call it('f',defs,'DOSUsesOcc', 'Weight the DOS with the occupation factors') 
 !
 call it(defs,'PDOS_atoms', 'Select atom number range',PDOS_atoms,verb_level=V_general)
 call it(defs,'PDOS_kinds', 'Select atom kind   range',PDOS_kinds,verb_level=V_general)
 call it(defs,'PDOS_wfcs',  'Select projector   range',PDOS_wfcs, verb_level=V_general)
 call it(defs,'PDOS_l',     'Select angular momentum l range',PDOS_l,verb_level=V_general)
 call it(defs,'PDOS_j',     'Select angular momentum j range',PDOS_j,verb_level=V_general)
 call it(defs,'PDOS_m',     'Select angular momentum m range',PDOS_m,verb_level=V_general)
 !
 call it(defs,'WFReference', 'band and kpt index of reference WF for spin direction',WF_ref,verb_level=V_general)
 !
#if defined _YPP_RT
 !
 ! RT DBs create
 !
 call it(defs,'RTBands',    '[RT] Bands range',RT_bands)
 call it(defs,'RTpumpNel',  '[RT] Number of electrons excited by hand per unit cell',Eh_pumped_Nel)
 call it(defs,'RTpumpBZWd', '[RT][iku] K region size',Eh_pumped_pair_BZ_width)
 call it(defs,'RTpumpEhWd', '[RT] Max Energy window used in the levels pick-up',Eh_pumped_pair_width,unit=E_unit)
 ! Method 1
 call it(defs,'RTpumpEhEn', '[RT] Energy (relative to VBM/CBM) of the levels to be inverted by hand',&
&                            Eh_pumped_pair_energy,unit=E_unit)
 ! Method 2
 call it(defs,'RTpumpDE',   '[RT] Energy difference to be matched',Eh_pumped_pair_deltaE,unit=E_unit)
 !
 ! The idea for Fermi occupations is to start from the 'end' of a carrier relaxation run
 call it(defs,'RTmuEh', '[RT] Chemical potentials hole | electron',Eh_mu,unit=E_unit)
 call it(defs,'RTtempEh', '[RT] Effective temperature hole | electron',Eh_temp,unit=E_unit)
 call it(defs,'RTautotuneThr', '[RT] Threshold to match no. of pumped holes and electrons.',h_mu_autotune_thr)
 !
 call it('f',defs,'LoadGhistory','[NEGF] Build the NEQ density from G<(t,t)')
 call it('f',defs,'OCCgroup',    '[NEGF] Double grid states occupations are grouped')
 call it('f',defs,'IncludeEQocc','[NEGF] Include also equilibrium occupations')
 call it('f',defs,'BuildGles',   '[NEGF] Build G<(t,tp) with GKBA')
 call it('f',defs,'KeepCC',      '[NEGF] Use only the cc channel when reconstructuing G<(t,tp)')
 call it('f',defs,'NoOcc',       '[NEGF] Consider only the changes in the energy')
 call it('f',defs,'SkipFermiFIT','[NEGF] Do a Fermi Fit of occupations (and lifetimes ratio)')
 call it('f',defs,'SeparateEH',  '[NEGF] Separate electrons from holes in the data file') 
 !
 ! TRANSIENT ABSORPTION
 !
 call it(defs,'TRabsOBS',&
&             '[RT] RT obs. Use a combination of (abs)bsorption, (trans)mission, (refl)ectivity, (kerr)err',TRabsWHAT)
 call it(defs,'TRabsDIPplane', '[RT] Dipole rotation plane',TRabsDIP_plane)
 call it(defs,'TRabsDIPdir',   '[RT] Dipole reference direction. Defines the internal `x` axis',TRabsDIP_dir)
 call it(defs,'TRabsMODE',     '[RT] TRabs mode. It can be a combination of cv,vv,cc,ip,eq',TRabsMODE)
 call it(defs,'TRabsEtresh',   '[RT] Treshold imposed on the e/h pairs energy',TRabs_Eeh_treshold,unit=E_unit)
 !
#endif
 !
 ! BANDS
 !
 call it(defs,'BANDS_bands',     'Number of bands',BANDS_bands) 
 call it(defs,'BANDS_steps',     'Number of divisions',BANDS_steps) 
 call it(defs,'CIRCUIT_E_DB_path',   'SAVE obtained from the QE `bands` run (alternative to %BANDS_kpts)',CIRCUIT_E_DB_path) 
 call it(defs,'BANDS_path',      'High-Symmetry points labels (G,M,K,L...) also using composed positions (0.5xY+0.5xL).',BANDS_path)
 call it(defs,'PROJECT_mode',    'Instruct ypp how to project the DOS. ATOM, LINE, PLANE.',PROJECT_mode)
 call it(defs,'PROJECT_path',    'Project path. PLANE mode: 3 points in the plane. LINE mode: any series of points.',PROJECT_path)
 call it(defs,'PROJECT_steps',    'Project LINE/PLANE-axis steps.',PROJECT_steps)
 call it(defs,'PROJECT_depth',    'Project perpendicular direction divisions.',PROJECT_N_perp)
 call it(defs,'PROJECT_threshold','[o/o] Project threshold above which the project DOS is printed.',&
&        PROJECT_threshold,verb_level=V_general)
 call it('f',defs,'BANDS_built_in', 'Print the bands of the generating points of the circuit using the nearest internal point')
 call it(defs,'INTERP_mode',     'Interpolation mode (NN=nearest point, BOLTZ=boltztrap aproach)',INTERP_mode)
 call it(defs,'INTERP_NofNN',    'Number of Nearest sites in the NN method',NN_n_of_nearest)
 call it(defs,'INTERP_Grid',     'Interpolation BZ Grid',INTERP_grid)
 call it(defs,'INTERP_Shell_Fac','The bigger it is a higher number of shells is used',INTERP_shell_factor)
#if defined _YPP_SC
 call it(defs,'SCneqKIND',  '[SC] Options are [contrained-occ/constrained-mu/matsubara]',SC_neq_kind,verb_level=V_sc)
#endif
 !
#if defined _YPP_RT
 call it('f',defs,'NormN','Nel on the bands is renormalized to Nel in the IBZ')
 call it(defs,'NelFac'   ,'Rescale the total number of electrons',Nel_fac)
#endif
 !
 ! Wannier interface
 !
 call it(defs,'Seed','Seed name',seed_name)
 call it(defs,'WriteAMU','What to write (A, M, U)',what_to_write)
 !
 ! BZ grids
 !
 call it(defs,'cooIn','Points coordinates (in) cc/rlu/iku/alat',coo_in)
 call it(defs,'cooOut','Points coordinates (out) cc/rlu/iku/alat',coo_out)
 call it(defs,'PtsPath','Path points labels (G,M,K,L...)',PtsPath)
 call it(defs,'NPtsPath','Number of Points for each segment of the path',N_path_pts)
 call it(defs,'BZ_random_Nk','BZ Random points to be printed',BZ_random_nkpts)
 !
 call it('f',defs,'DoNotDistr',    ' Do not generate random points around the k-points of the BZ') 
 call it('f',defs,'NoWeights',    ' Do not print points weight') 
 call it('f',defs,'ForceUserPts', ' Do not check the correcteness of the user points') 
 call it('f',defs,'ListPts',      ' List the internal q/k points also in the parser format') 
 call it('f',defs,'ExpandPts',    ' Expand the internal q/k points in the BZ') 
 !
 ! DIPOLES
 !
 call it(defs,'DIP_kind',      'Kind of DIPOLES use (IP, EXCITONIC, MASK)',DIPs_kind,protect=.FALSE.)
 call it(defs,'DIP_E_range',   'E/h pairs energy window',DIPs_E_range,E_unit)
 call it(defs,'DIP_direction', 'Field direction versor',DIPs_direction)
 call it(defs,'DIP_C_bands',     'E/h pairs conduction bands selection',DIPs_C_bands,verb_level=V_general)
 call it(defs,'DIP_V_bands',     'E/h pairs valence bands selection',DIPs_V_bands,verb_level=V_general)
 !
 ! Excitons
 !
 call it(defs,'Res_treshold', 'Select states above this optical strength treshold (max normalized to 1.)',&
&              EXCITON_Res_treshold,verb_level=V_general)
 call it(defs,'En_treshold', 'Select states below this energy treshold',&
&              EXCITON_E_treshold,verb_level=V_general,unit=E_unit)
 call it(defs,'Weight_treshold', 'Print transitions above this weight treshold (max normalized to 1.)',&
&              EXCITON_weight_treshold,verb_level=V_general)
 call it(defs,'States','Index of the BS state(s)',state_ctl)
 call it(defs,'EHdensity', 'Calculate (h)ole/(e)lectron density from BSE wave-function',AVE_exc,case="a")
 call it(defs,'Cells', 'Number of cell repetitions in each direction (odd or 1)',ncell)
 call it(defs,'Hole', '[cc] Hole position in unit cell (positive)',r_hole)
 call it('f',defs,'PrtDOS', 'Print Exciton Density of States') 
 !
 ! WFs
 !
 call it(defs,'FFTGvecs','[FFT] Plane-waves',wf_ng,G_unit)
 call it(defs,'Degen_Step', 'Maximum energy separation of two degenerate states',deg_energy,E_unit)
#if defined _YPP_ELPH
 call it(defs,'ResTresh', '[o/o] Treshold on processed polaronic residuals',elph_residual_treshold)
#endif
 call it(defs,'WFMult', 'Multiplication factor to the excitonic wavefunction',WF_multiplier,verb_level=V_general)
 call it(defs,'BSQindex','Q-Index of the BS state(s)',BSiq,protect=.not.BSiq_via_command_line)
 !
#if defined _YPP_SC
 call it('f',defs,'PhaseTrick','[MAG] Phase trick for a better diagonalization')
#endif
 ! 
 ! PLOT
 !
 ! p_dir: plot cut in the a1,a2,a3 basis
 ! 
 ! p_format: (c)ube/(g)nuplot/(x)crysden
 !
 call it(defs,'Format', 'Output format [(c)ube/(g)nuplot/(x)crysden]',p_format)
 call it(defs,'Direction', '[rlu] [1/2/3] for 1d or [12/13/23] for 2d [123] for 3D',p_dir)
 call it(defs,'MagDir', 'Magnetization component',mag_dir)
 call it(defs,'CurrentDir', '[cc] Carthesian direction (used to average in 2D/3D plots or to plot in 1D)',current_dir)
 call it('f',defs,'NormToOne','Normalize to one the maximum value of the plot')
#if defined _YPP_SC
 call it('f',defs,'PhaseTrick','[MAG] Phase trick for a better diagonalization')
 call it(defs,'PotValue',  'REal/IMaginary part of the mean potential',V_value)
#endif
#if defined _YPP_ELPH
 !
 !EL-PH
 !
 call it(defs,'DBsPATH',  'Path to the PW el-ph databases',elph_dbs_path)
 call it(defs,'PHfreqF',  'PWscf format file containing the phonon frequencies',ph_freqs_file)
 call it(defs,'PHmodeF',  'PWscf format file containing the phonon modes',ph_modes_file)
 call it(defs,'ElPhModes', '[ELPH] Phonon modes included',elph_branches)
 call it(defs,'EkplusQmode',  'E(k+q) energies calculation mode (interp | dftp)',EkplusQ_mode)
 call it(defs,'EfGbroad', 'Fermi surface broadening (Gamma)',elph_gamma_broad,E_unit)
 call it(defs,'PhBroad','Phonon broadening (Eliashberg & DOS)',ph_broad,E_unit)
 call it(defs,'EE_Fermi','External Fermi Level (Eliashberg)',elph_Ef,E_unit)
 call it(defs,'PhStps', 'Energy steps',elph_steps)
 call it('f',defs,'TestPHDGrid','Test double-grid: set all values of the fine grid equal to the couse ones')
 call it('f',defs,'GkkpReadBare','Read the bare gkkp')
 call it('f',defs,'GkkpExpand','Expand the gkkp in the whole BZ')
 call it('f',defs,'UseQindxB','Use qindx_B to expand gkkp (for testing purposes)')
 !
#endif
 !
 !Fine Grid
 !
 call it(defs,'FineGd_mode',       'Fine Grid mode. Symmetry expanded, unexpanded or mixed. ',BZ_FineGd_mode)
 call it(defs,'BZ_DbGd_Nk',        'BZ Number of points in the fine grid',BZ_FineGd_nkpts_to_use)
 call it('f',defs,'SkipCheck' ,    'Skip internal consistency check on k-point grids')
 call it('f',defs,'SkipBorderPts' , 'Skip points in the Fine Grid that are on the surface of coarse gride smal BZ`s')
 !
 !WFs perturbative SOC mapping
 !
 call it(defs,'SOC_DBs',"Path to the folder containing the DB with SOC",perturbative_SOC_path)
 call it(defs,'bands_to_map',"Number of bands to map",SOC_bands_to_map)
 call it(defs,'bands_buffer',"Bands to be used only as a buffer",SOC_bands_buffer)
 call it(defs,'SOC_splitting',"SOC splitting between the levels",SOC_split,E_unit) 
 !
 call it(defs,'OutputAlat','[a.u.] Lattice constant used for "alat" ouput format',alat_used_for_output)
 !
 ! QP DB rotation and expanstion
 !
 call it(defs,'QPDB_file','QP database to rotate',QP_DB_to_expand) 
 !
 ! Remove Simmetries
 !
 if(n_spinor==2) call it('f',defs,'ExpandSymm' , 'Add inv and magnetic symms if possible')
 call it('f',defs,'RmAllSymm' , 'Remove all symmetries')
 call it('f',defs,'RmTimeRev' , 'Remove Time Reversal')
 call it('f',defs,'KeepKGrid' , 'Do not expand the k-grid')
 !
 call it(defs,'Efield1','First external Electric Field',E_field(1,:))
 call it(defs,'GvecsCut','Max number of G-vectors in the new wave-function',wf_ng_cut,unit=G_unit,verb_level=V_RL)
 !
 call it('f',defs,'RmSpaceInv' , 'Remove Spatial Inversion')
 call it(defs,'Efield2','Additional external Electric Field',E_field(2,:))
 call it(defs,'BField',    '[MAG] Magnetic field modulus',B_field,unit=Bfield_unit)
 call it(defs,'Bpsi',      '[MAG] Magnetic field psi angle [degree]',B_psi,unit='deg')
 call it(defs,'Btheta',    '[MAG] Magnetic field theta angle [degree]',B_theta,unit='deg')
 !
#if defined _YPP_RT
 !
 ! RT Post-Processing
 !
 call it(defs,'Xorder','Max order of the response/exc functions',X_order)
 call it(defs,'XKind' ,  'Which response function (rhorho|jj)',X_kind,verb_level=V_real_time)
 call it(defs,'EnRngeRt','Energy range',RT_conf%X_W_range,unit=E_unit)
 call it(defs,'CarrEnRnge','Carriers Energy range',RT_conf%Carriers_W_range,unit=E_unit,verb_level=V_real_time)
 call it(defs,'ETStpsRt','Total Energy steps',RT_conf%W_n_steps)
 call it(defs,'TimeRange','Time-window where processing is done',RT_conf%T_range,unit=Time_unit(1))
 call it(defs,'TimeStep','Time step',RT_conf%delta_T,unit=Time_unit(1))
 call it(defs,'DampMode','Damping type ( NONE | LORENTZIAN | GAUSSIAN )',RT_conf%damping)
 call it(defs,'DampFactor','Damping parameter',RT_conf%damp_factor,unit=E_unit) 
 call it(defs,'RhoDeph','Dephasing parameter for G_GKBA',Rho_deph,unit=E_unit) 
 call it(defs,'PolMode','Polarization analysis mode ("slice","transitions")',RT_pol_mode) 
 call it('f',defs,'UseFFT','Use Fast Fourier transform with EnRange taken from dynamics',verb_level=V_real_time)
 call it('f',defs,'PrintExc','Print excited states functions')
 !
 call it(defs,'ProbeKey','Keyword to select groups of PROBE databases',Probe_Keyword,verb_level=V_real_time)
 call it(defs,'PumpKey','Keyword to select groups of PUMP databases',Pump_Keyword,verb_level=V_real_time)
 !
 call it('f',defs,'FrMinDamp','Force minimal damping',verb_level=V_real_time)
 call it('f',defs,'SkipOBS_IO','Do not dump on file the RT observables (P(t),J(t),D(t)...)')
 call it('f',defs,'RmAsymLim','Remove Asymptotic limit from P or J',verb_level=V_real_time) 
 !
 call it('f',defs,'STD_style','Print occupations using the standard style',verb_level=V_real_time) 
 !
#endif
 !
end subroutine
