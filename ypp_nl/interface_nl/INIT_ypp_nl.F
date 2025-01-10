!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_ypp_nl(E,instr,FINALIZE)
 !
 use units,          ONLY:FS2AUT
 use YPPm,           ONLY:l_plot,l_SOC_map,l_map_kpts,l_mag,l_k_grid,l_high_sym_pts,BSiq,&
&                         l_free_hole,l_fix_syms,l_excitons,l_dipoles,l_exc_wf,DIPs_kind,&
&                         l_amplitude,l_bands,l_bz_grids,l_current,BANDS_bands,BSiq_via_command_line,&
&                         l_density,l_dos,l_electrons,l_q_grid,l_QP_DBs_create_and_modify,&
&                         l_QP_DBs_manipulate,l_random_grid,l_shifted_grid,l_sort,l_sp_wf,l_spin,&
&                         l_wannier,l_wavefunction,n_FineGd_DBs,n_u_interp_pts,n_u_k_pts,n_u_q_pts,&
&                         U_k_pts,U_interp_pts,U_q_pts,U_q_shifts,FineGd_DB1_paths,l_angular_momentum,l_position,&
&                         l_QP_DB_expand,l_average_eh,l_interp,l_regular_grid,DIPs
 use YPP_real_time,  ONLY:l_NL_exc,l_NL_x,RT_conf
 use pars,           ONLY:pi,schlen
 use it_tools,       ONLY:ofiles_append,infile_dump,runlevel_is_on
 use it_m,           ONLY:initdefs,initmode,infile,nrnlvls,rnlvls,infile_verbosity,&
                          V_general,V_qp,V_all,V_real_time,V_nl_optics
 use drivers,        ONLY:infile_editing
 use com,            ONLY:write_to_report,repfile,inpfile_unit,repfile_unit
 use stderr,         ONLY:STRING_split,write_to_log,log_as_a_file,logfile,STRING_match,STRING_same,logfile_unit 
 use electrons,      ONLY:n_spin,n_spinor,levels
 use parallel_m,     ONLY:master_cpu
 use parallel_int,   ONLY:PP_wait
 use LIVE_t,         ONLY:live_timing_is_on
 use parser_m,       ONLY:PARSER_array
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,DUMP,NONE
 use nl_optics,      ONLY:NL_damping,DephMult
 use pars,           ONLY:SP
#if defined _YAML_OUTPUT
 use pars,           ONLY:logfile_index,repfile_index
 use com,            ONLY:of_yaml_IDs
 use yaml_output,    ONLY:yaml_close_stream
#endif
 !
#include<y_memory.h>
 !
 type(levels)     ::E
 character(*)     ::instr
 logical          ::FINALIZE
 !
 ! Work Space
 !
 logical          ::l_init_QP,l_flag,l_rtplot,l_rtmode
 type(initdefs)   ::defs
 integer          ::i1,i2,ID,io_err,i_cycle
 character(schlen)::rstr_piece(60) 
 !
 integer, external::io_RIM,io_COL_CUT,io_ATMPROJ_pwscf,io_DIPOLES
 logical, external::file_exists 
 !
 integer, external::io_NL
 integer          ::io_Nonlinear
 !
 if (FINALIZE) then
   call call_INIT_load('Game_Over')
   if (master_cpu) then
     if(write_to_report) call initinfio(defs,repfile_unit)
     call ofiles_append(defs=defs)
     close(unit=repfile_unit)
   endif
   if(log_as_a_file) close(unit=logfile_unit)
#if defined _YAML_OUTPUT
   call yaml_close_stream(unit=of_yaml_IDs(repfile_index))
   if(log_as_a_file) call yaml_close_stream(unit=of_yaml_IDs(logfile_index))
   call f_lib_finalize()
#endif
   return
 endif
 !
 ! DATABASES 
 !===========
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_Nonlinear=io_NL("Nonlinear",ID)
 !
 if(io_Nonlinear==0) then
   if(NL_damping/=0._SP) RT_conf%T_range(:)  =(/2._SP/NL_damping*DephMult,-real(FS2AUT,SP)/)
 endif
 !
 ! ATOMIC PROJECTIONS pwscf
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=io_ATMPROJ_pwscf(ID)
 !
 ! rim
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=io_RIM(ID) 
 !
 ! cutoff
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=io_COL_CUT(ID) 
 !
 DIPs%ib= (/1,E%nb/)
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=io_DIPOLES(DIPs,ID)
 !
 ! Defaults->Defs + Initialization
 !
 call call_INIT_load('create_shadow_vars')
 !
 ! Dump internally the input file
 !
 if (file_exists(trim(infile))) then
   !
   call infile_dump()
   !
   call INIT_input_file( )
   !
 endif
 !
 !Setup on the basis of the DB read/stdin variables
 !Here I can propose values to be written in the input file
 !
 call before_infile_write_setup( )
 !
 l_rtplot=.FALSE.
 l_rtmode=.FALSE.
 !
 ! RunLevels on
 !
 call STRING_split(instr,rstr_piece)
 do i1=1,50
   if ( trim(rstr_piece(i1))=="jobstr" ) cycle
   if ( i1>1) then
     if (trim(rstr_piece(i1-1))=="jobstr" ) cycle
   endif
   !
   ! Verbosity
   !
   if (i1<50) then
     if( trim(rstr_piece(i1)) == 'infver' .and. STRING_same(rstr_piece(i1+1),'gen') ) infile_verbosity=V_general
     if( trim(rstr_piece(i1)) == 'infver' .and. STRING_same(rstr_piece(i1+1),'rt')  ) infile_verbosity=V_real_time
     if( trim(rstr_piece(i1)) == 'infver' .and. STRING_same(rstr_piece(i1+1),'nl')  ) infile_verbosity=V_nl_optics
     if( trim(rstr_piece(i1)) == 'infver' .and. STRING_same(rstr_piece(i1+1),'qp')  ) infile_verbosity=V_qp
     if( trim(rstr_piece(i1)) == 'infver' .and. STRING_same(rstr_piece(i1+1),'all') ) infile_verbosity=V_all
   endif
   !
   call initactivate(1,'ElecTemp BoseTemp StdoHash')
   !
   do i_cycle=1,2
     !
     do i2=1,nrnlvls
       if ( trim(rstr_piece(i1)) == trim(rnlvls(i2,1)) ) then
         infile_editing=.true.
         call initactivate(1,trim(rnlvls(i2,1)))
       endif
     enddo
     !
     if ( trim(rstr_piece(i1)) == 'bzgrids') then
       l_k_grid=STRING_match(rstr_piece(i1+1),'k')
       l_q_grid=STRING_match(rstr_piece(i1+1),'q')
       l_shifted_grid=STRING_match(rstr_piece(i1+1),'s')
       l_random_grid=STRING_match(rstr_piece(i1+1),'r')
       l_regular_grid=STRING_match(rstr_piece(i1+1),'e')
       l_high_sym_pts=STRING_match(rstr_piece(i1+1),'h')
       if (l_k_grid) call initactivate(1,"K_grid")
       if (l_q_grid) call initactivate(1,"Q_grid")
       if (l_shifted_grid) call initactivate(1,"Shifted_Grid")
       if (l_random_grid)  call initactivate(1,"Random_Grid")
       if (l_regular_grid)  call initactivate(1,"Regular_Grid")
       if (l_high_sym_pts) call initactivate(1,"High_Symm")
     endif
     !
     if ( trim(rstr_piece(i1)) == 'QPDBs') then
       call initactivate(1,'QPDBs')
       l_QP_DBs_create_and_modify=STRING_match(rstr_piece(i1+1),'g')
       l_QP_DBs_manipulate            =STRING_match(rstr_piece(i1+1),'m')
       l_QP_DB_expand                 =STRING_match(rstr_piece(i1+1),'e')
       if (l_QP_DBs_create_and_modify) call initactivate(1,'QPDB_edit')
       if (l_QP_DBs_manipulate)        call initactivate(1,'QPDB_merge')
       if (l_QP_DB_expand)             call initactivate(1,'QPDB_expand')
     endif
     !
     if ( trim(rstr_piece(i1)) == 'BSiq') then
       read(rstr_piece(i1+1),'(i10)') BSiq 
       BSiq_via_command_line=.TRUE.
       if (l_sort) infile_editing=.false.
     endif
     if ( trim(rstr_piece(i1)) == 'excitons') then
       l_wavefunction   =STRING_match(rstr_piece(i1+1),'w')
       l_sort           =STRING_match(rstr_piece(i1+1),'s').and..not.STRING_match(rstr_piece(i1+1),'sp')
       l_amplitude      =STRING_match(rstr_piece(i1+1),'a')
       !
       l_spin  =STRING_match(rstr_piece(i1+1),'sp').and.n_spin>1
       l_mag   =STRING_match(rstr_piece(i1+1),'m').and.n_spin>1
       !
       l_interp          =STRING_match(rstr_piece(i1+1),'i')
       !
     endif
     if ( trim(rstr_piece(i1)) == 'dipoles') then
       if (STRING_match(rstr_piece(i1+1),'ip') ) DIPs_kind="IP"
       if (STRING_match(rstr_piece(i1+1),'exc')) DIPs_kind="EXCITONIC"
       if (STRING_match(rstr_piece(i1+1),'m'))   DIPs_kind="MASK"
     endif
     if ( trim(rstr_piece(i1)) == 'electrons') then
       !
       l_current        =STRING_match(rstr_piece(i1+1),'c')
       l_density        =STRING_match(rstr_piece(i1+1),'d')
       l_dos            =STRING_match(rstr_piece(i1+1),'s')
       l_wavefunction   =STRING_match(rstr_piece(i1+1),'w')
       l_bands          =STRING_match(rstr_piece(i1+1),'b')
       l_mag            =STRING_match(rstr_piece(i1+1),'m').and.n_spin>1
       !
     endif
     !
     l_NL_X   =  trim(rstr_piece(i1)) == 'nonlinear'
     !    
   enddo
 enddo
 !
 call call_INIT_load('load_defaults')
 !
 if (l_amplitude)      call initactivate(1,'amplitude')
 if (l_spin)           call initactivate(1,'spin')
 if (l_mag)            call initactivate(1,'magnetization')
 if (l_wavefunction)   call initactivate(1,'wavefunction')
 if (l_interp)         call initactivate(1,'interpolate')
 !
 if (l_current)        call initactivate(1,'current')
 if (l_density)        call initactivate(1,'density')
 if (l_sort)           infile_editing=.false.
 if (l_dos)            call initactivate(1,'dos')
 if (l_bands)          call initactivate(1,'bnds')
 !
 if (l_NL_X)         call initactivate(1,'nonlinear')
 !
 l_flag=.not.l_sort
 if (l_flag) call call_INIT_load('parser_input_file')
 !
 l_bz_grids  = runlevel_is_on('bzgrids')
 l_k_grid    = runlevel_is_on('K_grid')
 l_q_grid    = runlevel_is_on('Q_grid')
 l_shifted_grid = runlevel_is_on('Shifted_Grid')
 l_random_grid  = runlevel_is_on('Random_Grid')
 l_regular_grid = runlevel_is_on('Regular_Grid')
 l_high_sym_pts = runlevel_is_on('High_Symm')
 l_dos       =runlevel_is_on('dos')
 l_bands     =runlevel_is_on('bnds')
 l_fix_syms  =runlevel_is_on('fixsyms')
 !
 l_NL_X   =runlevel_is_on('nonlinear')
 !
 l_dipoles =runlevel_is_on('dipoles')
 l_excitons =runlevel_is_on('excitons')
 l_electrons=runlevel_is_on('electrons')
 l_plot=any((/runlevel_is_on('wavefunction'),runlevel_is_on('magnetization'),&
&             runlevel_is_on('density'),runlevel_is_on('current')/))
 l_free_hole =runlevel_is_on('freehole')
 l_average_eh=runlevel_is_on('avehole')
 l_amplitude=runlevel_is_on('amplitude')
 l_exc_wf   =runlevel_is_on('wavefunction').and.runlevel_is_on('excitons')
 l_interp    =runlevel_is_on('interpolate').and.runlevel_is_on('excitons')
 l_sp_wf    =runlevel_is_on('wavefunction').and.runlevel_is_on('electrons')
 l_density  =runlevel_is_on('density').and.runlevel_is_on('electrons')
 l_current  =runlevel_is_on('current').and.runlevel_is_on('electrons')
 l_mag      =runlevel_is_on('magnetization')
 l_wannier  =runlevel_is_on('wannier')
 l_SOC_map  =runlevel_is_on('WFs_SOC_map')
 l_spin     =runlevel_is_on('spin')
 !
 if (.not.l_exc_wf) then
    l_free_hole =.false.
    l_average_eh=.false.
 endif
 !
 if (l_bz_grids) then
   call initactivate(1,"OutputAlat")
   if (l_k_grid)        call initactivate(1,"NoWeights cooIn cooOut ListPts ExpandPts PtsPath")
   if (l_q_grid)        call initactivate(1,"NoWeights cooIn cooOut ListPts ExpandPts ForceUserPts")
   if (l_high_sym_pts)  call initactivate(1,"cooOut PtsPath NPtsPath")
   if (l_shifted_grid)  call initactivate(1,"NoWeights cooIn cooOut")
   if (l_random_grid)   call initactivate(1,"NoWeights cooOut BZ_random_Nk GammaRadius DoNotDistr")
   if (l_regular_grid)  call initactivate(1,"NoWeights cooOut Shift01 INTERP_Grid")
 endif
 !
 if ((l_dos.or.l_bands).and.n_spinor==2) call initactivate(1,'WFReference')
 !	   
 l_map_kpts  = runlevel_is_on('kpts_map')
 !
 l_QP_DBs_create_and_modify = runlevel_is_on('QPDB_edit')
 l_QP_DBs_manipulate = runlevel_is_on('QPDB_merge')
 l_QP_DB_expand = runlevel_is_on('QPDB_expand')
 !
 if (l_fix_syms) call initactivate(1,"Efield1 Efield2 BField Bpsi Btheta RmAllSymm RmTimeRev RmSpaceInv GvecsCut KeepKGrid")
 !
 ! Wannier 90 interface
 if (l_wannier) call initactivate(1,"Seed WriteAMU QPdbPath")
 !
 if (l_map_kpts) call initactivate(1,"FineGd_mode BZ_DbGd_Nk BZ_FineGd_nkpts SkipCheck SkipBorderPts")
 !
 if (l_SOC_map)  call initactivate(1,"SOC_DBs SOC_splitting bands_to_map bands_buffer")
 !
 ! Projections
 l_flag=(l_dipoles.and.trim(DIPs_kind)=="IP").or.(l_electrons.and.l_dos)
 if (l_flag) then
   call initactivate(1,"PROJECT_mode PROJECT_path PROJECT_steps PROJECT_depth PROJECT_threshold")  
 endif
 if (l_electrons.and.l_bands)   call initactivate(1,"PROJECT_mode PROJECT_threshold")  
 !
 ! Interpolation
 l_flag=(l_dipoles.and.trim(DIPs_kind)=="IP").or.(l_electrons.and.l_dos).or.l_bands.or.(l_excitons.and.l_interp)
 if (l_flag) call initactivate(1,"INTERP_mode INTERP_Shell_Fac INTERP_NofNN")
 !
 l_flag=l_electrons.and.l_dos
 if (l_flag) call initactivate(1,"INTERP_Grid")
 !
 if (l_excitons.and.l_interp) call initactivate(1,"BANDS_steps cooIn cooOut")
 l_flag=l_bands
 if (l_flag) then
   call initactivate(1,"OutputAlat cooIn cooOut NormN NelFac")
   if (l_bands) call initactivate(1,"BANDS_bands")
   call initactivate(1,"CIRCUIT_E_DB_path BANDS_path BANDS_steps BANDS_built_in")
 endif
 if (l_flag) then
   call initactivate(1,"INTERP_mode OutputAlat cooIn cooOut NormN NelFac")
   if (l_bands) call initactivate(1,"BANDS_bands")
   call initactivate(1,"INTERP_Shell_Fac INTERP_NofNN")
 endif
 !
 ! Dipoles
 if (l_dipoles) then
   call initactivate(1,"DIP_kind cooOut")
   if (trim(DIPs_kind)=="EXCITONIC") then
     call initactivate(1,"States BSQindex Degen_Step Weight_treshold En_treshold Res_treshold")
   else
     call INIT_QP_ctl_switch('G')
     call initactivate(1,"DIP_E_range DIP_C_bands DIP_direction")
     call initactivate(1,"DIP_V_bands")
   endif
 endif
 !
 ! Electrons
 if (l_electrons) then
   if (l_dos.or.l_bands) call INIT_QP_ctl_switch('G')
   if (l_dos) then
     call initactivate(1,"DOSERange DOSESteps DOS_broad DOS_bands DOSUsesOcc")
     call initactivate(1,"PDOS_atoms PDOS_kinds PDOS_wfcs PDOS_l PDOS_j PDOS_m")
   endif
 endif
 !
 if(l_NL_X)   call initactivate(1,"Xorder TimeRange ETStpsRt EnRngeRt DampMode DampFactor PrtPwrSpec PumpPATH")
 if(l_NL_exc) call initactivate(1,"Xorder PrintExc")
 !
 if (l_plot) then
   call initactivate(1,"Format Direction FFTGvecs NormToOne") 
   if (l_sp_wf) call initactivate(1,"Degen_Step")  
   if (l_mag)   call initactivate(1,"MagDir") 
   if (l_current) call initactivate(1,'CurrentDir')
 endif
 !
 if (l_spin.and..not.l_excitons) call initactivate(1,"FFTGvecs")
 !
 if (l_excitons) then
   call initactivate(1,"States En_treshold Res_treshold BSQindex")
   if (l_amplitude) call INIT_QP_ctl_switch('G')
   if (l_exc_wf.or.l_amplitude) call initactivate(1,"Degen_Step Weight_treshold")
   if (l_exc_wf.and.(.not.l_free_hole.and..not.l_average_eh)) call initactivate(1,"Cells Hole Dimension") 
   if (l_exc_wf.and.(l_free_hole.or.l_average_eh)) call initactivate(1,"WFMult") 
   if (l_exc_wf.and.l_average_eh) call initactivate(1,"EHdensity")
   if (l_spin) call initactivate(1,"Degen_Step")
   if (l_interp) then
     call initactivate(-1,"BSQindex")
     call initactivate(1,"INTERP_mode INTERP_Grid BANDS_steps cooIn cooOut")
     call initactivate(1,"PrtDOS DOSERange DOSESteps DOS_broad")
   endif
 endif
 !
 ! Check for existing QP control fields to be proposed in the new input files
 !
 call QP_state_table_setup(E,1,1,.true.)
 !
 if (infile_editing) then
   inpfile_unit=12
   open(unit=inpfile_unit,file=trim(infile))
   call initinfio(defs,inpfile_unit)
   close(inpfile_unit)
   call PP_wait()
 endif
 !
 if (l_QP_DBs_create_and_modify.or.l_QP_DBs_manipulate) call QP_DBs_initialize()
 !
 if (l_QP_DB_expand) call initactivate(1,"QPDB_file") 
 !
 l_init_QP=l_plot.and.l_sp_wf
 if (l_init_QP) call INIT_QP(" ")
 !
 ! Check and fill all possible vectorial USER fields
 !
 l_flag=l_bands
 if (l_flag) call USER_vector_handle("BANDS_kpts",3)
 !
 if (l_bz_grids) then
   if (l_k_grid)       call USER_vector_handle("GWKpts",3)
   if (l_q_grid)       call USER_vector_handle("Qpts",3)
   if (l_shifted_grid) call USER_vector_handle("KShift",3)
 endif
 if (l_interp)         call USER_vector_handle("BANDS_kpts",3)
 !
 if (l_map_kpts) then
   call PARSER_array(mode="CH dimensions",VAR="FineGd_DB1_paths",N=n_FineGd_DBs,M=1)
   if (n_FineGd_DBs>0) then
     call PARSER_array(mode="read",VAR="FineGd_DB1_paths",N=n_FineGd_DBs,M=1,CH_v=FineGd_DB1_paths(:n_FineGd_DBs,:))
   else
     n_FineGd_DBs=1
     FineGd_DB1_paths(1,1)="none"
   endif
   call PARSER_array(mode="write",VAR="FineGd_DB1_paths",DESC="Paths to the SAVE folders with the Fine Grid DB1",&
&                    N=n_FineGd_DBs,M=1,CH_v=FineGd_DB1_paths(:n_FineGd_DBs,:))
 endif
 !
 ! Input file
 !
 if (infile_editing) then
   open(unit=inpfile_unit,file=trim(infile))
   call initinfio(defs,inpfile_unit)
   close(inpfile_unit)
   call PP_wait()
   return
 endif
 !
 ! LOG/REP files
 !
 live_timing_is_on =master_cpu.and..not.infile_editing
 write_to_report   =master_cpu.and..not.infile_editing
 write_to_log      =master_cpu.and..not.infile_editing
 !
 call INIT_report_and_log_files()
 !
 contains
   !
   subroutine call_INIT_load(mode) 
     character(*)::mode
     !
     if (mode=='create_shadow_vars') initmode=0
     if (mode=='load_defaults') initmode=1
     if (mode=='Close_G_vectors') initmode=2
     if (mode=='Game_Over') initmode=3
     if (mode=='parser_input_file') initmode=4
     call INIT_ypp_load(defs)
     !
   end subroutine
   !
   subroutine before_infile_write_setup 
     !
     use YPPm,           ONLY:DOS_bands,SOC_bands_to_map,DIPs_C_bands,DIPs_V_bands
     use electrons,      ONLY:n_bands
     !
     ! After DB reading/stdin logicals I propose here values for the input file
     ! 
     SOC_bands_to_map=(/ 1, n_bands /)
     BANDS_bands=(/1,n_bands/)
     DOS_bands=(/1,n_bands/)
     DIPs_C_bands=DIPs%ib
     DIPs_V_bands=DIPs%ib
     !
   end subroutine
   !
   subroutine USER_vector_handle(field,M)
     !
     use parser_m,       ONLY:PARSER_array
     !
     character(*)::field
     integer     ::M,N,nq_s
     !
     call PARSER_array(mode="REAL dimensions",VAR=field,N=N,M=M)
     !
     if (field=="GWKpts") then
       if (N> 0) n_u_k_pts=N
       if (N==0) n_u_k_pts=1
       YAMBO_ALLOC(U_k_pts,(n_u_k_pts,3))
       if (N> 0) call PARSER_array(mode="read",VAR=field,N=N,M=3,R_v=U_k_pts)
       if (N==0) U_k_pts=0.
       call PARSER_array(mode="write",VAR=field,DESC='K points list',N=n_u_k_pts,M=3,R_v=U_k_pts)
     else if (field=="BANDS_kpts") then
       if (N> 0) then 
         n_u_interp_pts=N
         YAMBO_ALLOC(U_interp_pts,(n_u_interp_pts,3))
         call PARSER_array(mode="read",VAR=field,N=N,M=3,R_v=U_interp_pts)
         call PARSER_array(mode="write",VAR=field,DESC='K points of the bands circuit',&
&                          N=n_u_interp_pts,M=3,R_v=U_interp_pts)
       else
         n_u_interp_pts=0
         call PARSER_array(mode="write",VAR=field,DESC='K points of the bands circuit',N=0,M=3)
       endif
     else if (field=="Qpts") then
       if (N> 0) n_u_q_pts=N
       if (N==0) n_u_q_pts=1
       YAMBO_ALLOC(U_q_pts,(n_u_q_pts,3))
       if (N> 0) call PARSER_array(mode="read",VAR=field,N=N,M=3,R_v=U_q_pts)
       if (N==0) U_q_pts=0.
       call PARSER_array(mode="write",VAR=field,DESC='Q points list',&
&                        N=n_u_q_pts,M=3,R_v=U_q_pts)
     else if (field=="KShift") then
       U_q_shifts=0.
       if (N> 0) call PARSER_array(mode="read",VAR=field,N=min(N,3),M=3,R_v=U_q_shifts)
       call PARSER_array(mode="write",VAR=field,DESC='K-grid shifts (max 3)',&
&                        N=3,M=3,R_v=U_q_shifts)
     endif
     !
   end subroutine
   !
end subroutine
