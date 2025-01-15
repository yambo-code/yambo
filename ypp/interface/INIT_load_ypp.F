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
 call INIT_QP_ctl_load(defs,3)
 call it('r',defs,'spin',         '[R] Spin')
 call it('r',defs,'magnetization','[R] Magnetization')
 call it('r',defs,'WFs_map',      '[R] Creat a SOC/WFs <-> noSOC/WFs map')
 call it('r',defs,'QPDB_edit',    '[R] Generation/editing')
 call it('r',defs,'QPDB_merge',   '[R] Mergering')
 call it('r',defs,'QPDB_expand',  '[R] Symmetry expand')
 !
 ! Variables...
 !==============
 call it(defs,'StdoHash','[IO] Live-timing Hashes',nhash,verb_level=V_io)
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
 call it(defs,'WFMult', 'Multiplication factor to the excitonic wavefunction',WF_multiplier,verb_level=V_general)
 call it(defs,'BSQindex','Q-Index of the BS state(s)',BSiq,protect=.not.BSiq_via_command_line)
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
end subroutine
