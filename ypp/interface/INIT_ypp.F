!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine INIT_ypp(E,instr,lnstr,FINALIZE)
 !
 use units,          ONLY:FS2AUT
 use YPP,            ONLY:l_plot,l_map_WFs,l_map_kpts,l_mag,l_k_grid,l_high_sym_pts,&
&                         l_free_hole,l_fix_syms,l_excitons,l_exc_wf,l_fix_save,l_mean_potential,&
&                         l_amplitude,l_bands,l_bxsf,l_bz_grids,l_current,BANDS_bands,&
&                         l_density,l_dos,l_electrons,l_q_grid,l_QP_DBs_create_and_modify,&
&                         l_QP_DBs_manipulate,l_random_grid,l_shifted_grid,l_sort,l_sp_wf,l_spin,&
&                         l_wannier,l_wavefunction,MAX_DbGd_DBs,n_DbGd_DBs,n_u_interp_pts,n_u_k_pts,n_u_q_pts,&
&                         U_k_pts,U_interp_pts,U_q_pts,U_q_shifts,DbGd_DB1_paths,l_angular_momentum,l_position
 use pars,           ONLY:pi,schlen
 use D_lattice,      ONLY:i_time_rev
 use it_m,           ONLY:initdefs,initmode,ofiles_append,&
&                         initinfio,infile,infile_dump,initactivate,&
&                         nrnlvls,rnlvls,runlevel_is_on,&
&                         infile_verbosity,V_general,V_qp,V_all,V_real_time,V_nl_optics
 use drivers,        ONLY:infile_editing
 use com,            ONLY:file_exists,write_to_report,repfile,write_the_logo
 use stderr,         ONLY:STRING_split,write_to_log  
 use electrons,      ONLY:n_spin,n_spinor,levels
 use parallel_m,     ONLY:master_cpu,PP_redux_wait
 use LIVE_t,         ONLY:what_is_running,live_timing_is_on
 use parser_m,       ONLY:PARSER_array
 use IO_m,           ONLY:rm_file,OP_RD_CL,DUMP,io_control,NONE
#if defined _YPP_RT || defined _YPP_SC
 use QP_m,           ONLY:QP_nb
 use electrons,      ONLY:levels,E_reset
#endif
#if defined _YPP_RT 
 use YPP_real_time,  ONLY:l_RealTime,l_RT_dos,l_RT_abs,l_RT_bands,l_RT_DBs,l_RT_density,&
&                         l_RT_energy,l_RT_lifetimes,l_RT_occupations,l_RT_time,l_RT_X,l_RTpump_Fermi,&
&                         l_RTpump_energy,l_RTpump_kspace,Eh_pumped_pair_n_BZ_regions,Eh_pumped_pair_BZ_regions,&
&                         l_NL_X,l_NL_exc,RT_conf
 use stderr,         ONLY:intc
 use real_time,      ONLY:RT_bands
#endif
#if defined _YPP_NL 
 use nl_optics,      ONLY:NL_damping
#endif
#if defined _YPP_ELPH
 use YPP_ELPH
#endif
 !
#include<memory.h>
 !
 type(levels)     ::E
 integer          ::lnstr
 character(lnstr) ::instr
 logical          ::FINALIZE
 !
 ! Work Space
 !
 logical          ::l_init_QP,l_flag
 type(initdefs)   ::defs
 integer, external::io_RIM,io_COL_CUT
 integer          ::i1,i2,ID,io_err
 character(schlen)::rstr_piece(60) 
 !
#if defined _YPP_RT
 integer, external::io_RT_components,io_KB_pwscf,io_KB_abinit
 integer          ::io_RT_G_err,io_RT_OBS_err,io_KB_pwscf_err,io_KB_abinit_err
#endif
#if defined _YPP_NL
 integer, external::io_NL
 integer          ::io_Nonlinear
#endif
 !
 ! What is running ?
 !
 what_is_running='YPP'
#if defined _YPP_ELPH
 what_is_running='YPP_PH'
#endif
#if defined _YPP_RT
 what_is_running='YPP_RT'
#endif
#if defined _YPP_NL
 what_is_running='YPP_NL'
#endif
 !
 if (FINALIZE) then
   call call_INIT_load('GameOver')
   if (master_cpu) call initinfio(defs,11)
   if (master_cpu) call ofiles_append(defs=defs)
   return
 endif
 !
 call YPP_SET_defaults()
 !
 ! DATABASES 
 !===========
 !
 !
#if defined _YPP_RT
 !
 ! 
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) ! Current response
 io_RT_OBS_err=io_RT_components('OBSERVABLES' ,ID)
 if (io_RT_OBS_err==0) QP_nb=RT_bands(2)
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_KB_pwscf_err=io_KB_pwscf(ID)
 ! 
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_KB_abinit_err=io_KB_abinit(ID)
 !
#endif        
 !
#if defined _YPP_NL
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_Nonlinear=io_NL(ID)
 !
 if(io_Nonlinear==0) then
   if(NL_damping/=0._SP) RT_conf%T_range(:)  =(/2._SP/NL_damping*5._SP,-real(FS2AUT,SP)/)
 endif
 !
#endif
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
 ! Defaults->Defs + Initialization
 !
 call call_INIT_load('load')
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
     if( trim(rstr_piece(i1)) == 'infver' .and. trim(rstr_piece(i1+1))=='gen' ) infile_verbosity=V_general
     if( trim(rstr_piece(i1)) == 'infver' .and. trim(rstr_piece(i1+1))=='rt' )  infile_verbosity=V_real_time
     if( trim(rstr_piece(i1)) == 'infver' .and. trim(rstr_piece(i1+1))=='nl' )  infile_verbosity=V_nl_optics
     if( trim(rstr_piece(i1)) == 'infver' .and. trim(rstr_piece(i1+1))=='qp' )  infile_verbosity=V_qp
     if( trim(rstr_piece(i1)) == 'infver' .and. trim(rstr_piece(i1+1))=='all' ) infile_verbosity=V_all
   endif
   !
   call initactivate(1,'ElecTemp BoseTemp')
   !
   do i2=1,nrnlvls
     if ( trim(rstr_piece(i1)) == trim(rnlvls(i2,1)) ) then
       infile_editing=.true.
       call initactivate(1,trim(rnlvls(i2,1)))
     endif
   enddo
   !
   if ( trim(rstr_piece(i1)) == 'bzgrids') then
     l_k_grid=trim(rstr_piece(i1+1))=='k'
     l_q_grid=trim(rstr_piece(i1+1))=='q'
     l_shifted_grid=trim(rstr_piece(i1+1))=='s'
     l_random_grid=trim(rstr_piece(i1+1))=='r'
     !
     !
     if (l_k_grid) call initactivate(1,"K_grid")
     if (l_q_grid) call initactivate(1,"Q_grid")
     if (l_shifted_grid) call initactivate(1,"Shifted_Grid")
     if (l_random_grid)  call initactivate(1,"Random_Grid")
     if (l_high_sym_pts) call initactivate(1,"High_Symm")
   endif
   !
   if ( trim(rstr_piece(i1)) == 'WFs') then
     call initactivate(1,'WFs')
     l_fix_save=trim(rstr_piece(i1+1))=='c'
     l_map_WFs  =trim(rstr_piece(i1+1))=='p'
     if (l_map_WFs) call initactivate(1,'WFs_map')
   endif
   !
   if ( trim(rstr_piece(i1)) == 'QPDBs') then
     call initactivate(1,'QPDBs')
     l_QP_DBs_create_and_modify=trim(rstr_piece(i1+1))=='g'
     l_QP_DBs_manipulate            =trim(rstr_piece(i1+1))=='m'
     if (l_QP_DBs_create_and_modify) call initactivate(1,'QPDB_edit')
     if (l_QP_DBs_manipulate)        call initactivate(1,'QPDB_merge')
   endif
   !
#if defined _YPP_ELPH
   if ( trim(rstr_piece(i1)) == 'phonons') then
     l_dos              =trim(rstr_piece(i1+1))=='d'
     l_eliashberg       =trim(rstr_piece(i1+1))=='e'
     l_atomic_amplitude =trim(rstr_piece(i1+1))=='a'
     if (l_atomic_amplitude) infile_editing=.false.
     if (l_eliashberg)   call initactivate(1,'eliashberg')
   endif
#endif
   if ( trim(rstr_piece(i1)) == 'excitons') then
     l_wavefunction   =trim(rstr_piece(i1+1))=='w'
     l_sort           =trim(rstr_piece(i1+1))=='s'
     l_amplitude      =trim(rstr_piece(i1+1))=='a'
#if defined _YPP_ELPH
     l_gkkp           =trim(rstr_piece(i1+1))=='g'
#endif
     !
     l_spin  =trim(rstr_piece(i1+1))=='sp'.and.n_spin>1
     l_mag   =trim(rstr_piece(i1+1))=='m'.and.n_spin>1
     !
     !
   endif
   if ( trim(rstr_piece(i1)) == 'electrons') then
     !
     l_current        =trim(rstr_piece(i1+1))=='c'
     l_density        =trim(rstr_piece(i1+1))=='d'
     l_dos            =trim(rstr_piece(i1+1))=='s'
     l_wavefunction   =trim(rstr_piece(i1+1))=='w'
     l_bands          =trim(rstr_piece(i1+1))=='b'
     l_mag            =trim(rstr_piece(i1+1))=='m'.and.n_spin>1
     !
#if defined _YPP_ELPH
     l_eliashberg       =trim(rstr_piece(i1+1))=='e'
#endif
     !
     !
   endif
   !
#if defined _YPP_RT
   !
   !
   if ( trim(rstr_piece(i1)) == 'RealTime') then
     !
     l_RT_X           =trim(rstr_piece(i1+1))=='X'
     !
     !
   endif
#endif
   !
#if defined _YPP_NL
   l_NL_X   =  trim(rstr_piece(i1)) == 'nonlinear'
#endif   
   !    
   if ( trim(rstr_piece(i1)) == 'excitons'.or. trim(rstr_piece(i1)) == 'electrons') then
     if (l_amplitude)      call initactivate(1,'amplitude')
     if (l_spin)           call initactivate(1,'spin')
     if (l_mag)            call initactivate(1,'magnetization')
     if (l_wavefunction)   call initactivate(1,'wavefunction')
#if defined _YPP_ELPH
     if (l_eliashberg)       call initactivate(1,'eliashberg')
     if (l_gkkp)             call initactivate(1,'gkkp')
#endif
     if (l_current)        call initactivate(1,'current')
     if (l_density)        call initactivate(1,'density')
     if (l_sort)           infile_editing=.false.
   endif
   if ( trim(rstr_piece(i1)) == 'phonons'.or. trim(rstr_piece(i1)) == 'electrons') then
     if (l_dos)            call initactivate(1,'dos')
   endif
   if ( trim(rstr_piece(i1)) == 'electrons') then
     if (l_bands)          call initactivate(1,'bnds')
   endif
#if defined _YPP_RT
   !
   !
   if ( trim(rstr_piece(i1)) == 'RealTime') then
     if (l_RT_X)           call initactivate(1,'RT_X')
     !
     !
   endif
#endif
 enddo
 !
#if defined _YPP_NL
 if (l_NL_X)         call initactivate(1,'nonlinear')
#endif
 !
 l_flag=.not.(l_sort.or.l_fix_save)
#if defined _YPP_ELPH
 l_flag=.not.l_sort.and..not.l_atomic_amplitude
#endif
 if (l_flag) call call_INIT_load('parserload')
 !
 l_bz_grids  = runlevel_is_on('bzgrids')
 l_k_grid    = runlevel_is_on('K_grid')
 l_q_grid    = runlevel_is_on('Q_grid')
 l_shifted_grid = runlevel_is_on('Shifted_Grid')
 l_random_grid  = runlevel_is_on('Random_Grid')
 l_high_sym_pts = runlevel_is_on('High_Symm')
 l_dos       =runlevel_is_on('dos')
 l_bands     =runlevel_is_on('bnds')
 l_fix_syms  =runlevel_is_on('fixsyms')
#if defined _YPP_ELPH
 l_eliashberg=runlevel_is_on('eliashberg')
 l_phonons   =runlevel_is_on('phonons')
 l_gkkp      =runlevel_is_on('gkkp')
#endif
#if defined _YPP_RT
 !
 l_RealTime         = runlevel_is_on('RealTime')
 !
 l_RT_X             = runlevel_is_on('RT_X')
 !
 !
#endif
#if defined _YPP_NL
 l_NL_X   =runlevel_is_on('nonlinear')
#endif
 l_excitons =runlevel_is_on('excitons')
 l_electrons=runlevel_is_on('electrons')
 l_plot=any((/runlevel_is_on('wavefunction'),runlevel_is_on('magnetization'),&
&             runlevel_is_on('density'),runlevel_is_on('current')/))
#if defined _YPP_RT
 l_plot=l_plot .or. runlevel_is_on('RTdeltaRho')
#endif
 l_free_hole=runlevel_is_on('freehole')
 l_amplitude=runlevel_is_on('amplitude')
 l_exc_wf   =runlevel_is_on('wavefunction').and.runlevel_is_on('excitons')
 l_sp_wf    =runlevel_is_on('wavefunction').and.runlevel_is_on('electrons')
 l_density  =runlevel_is_on('density').and.runlevel_is_on('electrons')
 l_current  =runlevel_is_on('current').and.runlevel_is_on('electrons')
 l_mag      =runlevel_is_on('magnetization')
 l_wannier  =runlevel_is_on('wannier')
 l_spin     =runlevel_is_on('spin')
 l_map_WFs  =runlevel_is_on('WFs_map')
 !
 if (.not.l_exc_wf) l_free_hole=.false.
 !
 if (l_bz_grids) then
   call initactivate(1,"OutputAlat")
   if (l_k_grid)        call initactivate(1,"NoWeights cooIn cooOut ListPts ExpandPts")
   if (l_q_grid)        call initactivate(1,"NoWeights cooIn cooOut ListPts ExpandPts ForceUserPts")
   if (l_high_sym_pts)  call initactivate(1,"cooOut PtsPath NPtsPath")
   if (l_shifted_grid)  call initactivate(1,"NoWeights cooIn cooOut")
   if (l_random_grid)   call initactivate(1,"NoWeights cooOut BZ_random_Nk GammaRadius")
 endif
 !
 if ((l_dos.or.l_bands).and.n_spinor==2) call initactivate(1,'WFReference')
 !
#if defined _YPP_RT
 if (l_bands.or.l_RT_bands) then
#else
 if (l_bands) then
#endif
   !
   call initactivate(1,"INTERP_mode OutputAlat cooIn cooOut NormN NelFac")
   !
   if (l_bands) call initactivate(1,"BANDS_bands")
   !
   call initactivate(1,"INTERP_Grid INTERP_Shell_Fac")
   call initactivate(1,"CIRCUIT_E_DB_path BANDS_path BANDS_steps BANDS_built_in")
   !
 endif
 !	   
 l_map_kpts  = runlevel_is_on('kpts_map')
 !
 l_bxsf      = runlevel_is_on('bxsf')
 !
 if (l_bxsf) call initactivate(1,"W90_fname WannBnds DgridOrd")
 !
 l_QP_DBs_create_and_modify = runlevel_is_on('QPDB_edit')
 l_QP_DBs_manipulate = runlevel_is_on('QPDB_merge')
 !
 !
 if (l_fix_syms.and.n_spinor==2.and.i_time_rev==0) call initactivate(1,"ExpandSymm")
 !
 if (l_fix_save) infile_editing=.false.
 !
 if (l_fix_syms) call initactivate(1,"Efield1 Efield2 BField Bpsi Btheta RmAllSymm RmTimeRev RmSpaceInv")
 !
#if defined _YPP_RT
 !
 if (l_RT_X) then 
   call initactivate(1,"Xorder ProbeKey PumpKey EnRngeRt ETStpsRt")
   call initactivate(1,"TimeRange DampMode DampFactor SkipOBS_IO FrMinDamp RmAsymLim UseFFT CheckPaths")
 endif
 !
#endif
 !
 !
 if (l_map_kpts) call initactivate(1,"BZ_DbGd_Nk SkipCheck noBZExpand NoWeights")
 !
 if (l_map_WFs)  call initactivate(1,"SOC_DBs SOC_splitting bands_to_map bands_buffer")
 !
 if (l_electrons) then
   if (l_dos.or.l_bands) call INIT_QP_ctl_switch('G')
   if (l_dos) then
     call initactivate(1,"DOSERange DOSESteps DOS_broad DOS_bands DOSUsesOcc")
     call initactivate(1,"INTERP_mode INTERP_Grid INTERP_Shell_Fac")
   endif
 endif
#if defined _YPP_RT
 if (l_RT_DBs)    then
   call initactivate(1,'RTpumpNel RTpumpSpin RTBands')
   if (l_RTpump_energy) call initactivate(1,'RTpumpEhEn RTpumpEhWd')
   if (l_RTpump_kspace) then
     call initactivate(1,'RTpumpDE RTpumpEhWd RTpumpBZWd cooIn')
     !
     call PARSER_array(mode="REAL dimensions",VAR="RTpumpBZ",N=Eh_pumped_pair_n_BZ_regions,M=3)
     if (Eh_pumped_pair_n_BZ_regions>0) then
       call PARSER_array(mode="read",VAR="RTpumpBZ",N=Eh_pumped_pair_n_BZ_regions,M=3,&
&                        R_v=Eh_pumped_pair_BZ_regions(:Eh_pumped_pair_n_BZ_regions,:))
     else
       Eh_pumped_pair_n_BZ_regions=1
       Eh_pumped_pair_BZ_regions(1,:)=-1.
     endif
     call PARSER_array(mode="write",VAR="RTpumpBZ",N=Eh_pumped_pair_n_BZ_regions,M=3,&
&                      DESC="[RT] K-points manually excited",&
&                      R_v=Eh_pumped_pair_BZ_regions(:Eh_pumped_pair_n_BZ_regions,:))
     !
   endif
   if (l_RTpump_Fermi) call initactivate(1, 'RTmuEh RTtempEh RTautotuneThr')
 endif
 if (l_RealTime) then
   !
   !
   if (l_RT_abs)    call initactivate(1,"EnRngeRt ETStpsRt DampFactor Transmission Reflectivity IncludeIP RTformat")
   !
 endif
#endif
 !
#if defined _YPP_NL
 if(l_NL_X) call initactivate(1,"Xorder TimeRange ETStpsRt EnRngeRt DampMode DampFactor PrtPwrSpec")
 if(l_NL_exc) call initactivate(1,"Xorder PrintExc")!  MaxContr")
#endif
 !
 !
 if (l_plot) then
   call initactivate(1,"Format Direction FFTGvecs") 
   if (l_sp_wf) call initactivate(1,"Degen_Step")  
#if defined _YPP_ELPH
   if (l_sp_wf) call initactivate(1,"ResTresh")  
#endif
   if (l_mag)   call initactivate(1,"MagDir") 
   if (l_current) call initactivate(1,'CurrentDir')
 endif
 !
 if (l_excitons) then
   call initactivate(1,"States")
   if (l_amplitude) then
     call INIT_QP_ctl_switch('G')
     call initactivate(1,"DipWeight")
   endif
   if (l_exc_wf.or.l_amplitude) call initactivate(1,"Degen_Step MinWeight")
   if (l_exc_wf.and..not.l_free_hole) call initactivate(1,"Cells Hole Dimension") 
   if (l_exc_wf.and.l_free_hole) call initactivate(1,"WFMult") 
   if (l_spin) call initactivate(1,"Degen_Step FFTGvecs")
#if defined _YPP_ELPH
   if (l_eliashberg)  call initactivate(1,"Degen_Step") 
#endif
 endif
 !
#if defined _YPP_ELPH
 if (l_gkkp.and..not.l_excitons)   call initactivate(1,"DBsPATH PHfreqF PHmodeF GkkpExpand GkkpConvert")
 if (l_phonons.and.l_eliashberg)   call initactivate(1,"EE_Fermi EfGbroad")
 if ( ((l_excitons.or.l_electrons).and.l_eliashberg) .or. (l_phonons.and.l_dos) ) call initactivate(1,"PhBroad PhStps")
#endif
 !
 ! Check for existing QP control fields to be proposed in the new input files
 !
 call QP_state_table_setup(E)
 !
 if (infile_editing) then
   open(unit=12,file=trim(infile))
   call initinfio(defs,12)
   close(12)
   call PP_redux_wait
 endif
 !
 if (l_QP_DBs_create_and_modify.or.l_QP_DBs_manipulate) call QP_DBs_initialize()
 !
 l_init_QP=l_plot.and.l_sp_wf
#if defined _YPP_ELPH
 if (.not.l_init_QP) l_init_QP=l_electrons.and.l_eliashberg
#endif
 !
 if (l_init_QP) call INIT_QP(" ")
 !
 ! Check and fill all possible vectorial USER fields
 !
 l_flag=l_bands
 !
 !
 if (l_flag) call USER_vector_handle("BANDS_kpts")
 !
 if (l_bz_grids) then
   if (l_k_grid)       call USER_vector_handle("GWKpts")
   if (l_q_grid)       call USER_vector_handle("Qpts")
   if (l_shifted_grid) call USER_vector_handle("KShift")
 endif
 !
 if (l_map_kpts) then
   call PARSER_array(mode="CH dimensions",VAR="DbGd_DB1_paths",N=n_DbGd_DBs,M=1)
   if (n_DbGd_DBs>0) then
     call PARSER_array(mode="read",VAR="DbGd_DB1_paths",N=n_DbGd_DBs,M=1,CH_v=DbGd_DB1_paths(:n_DbGd_DBs,:))
   else
     n_DbGd_DBs=1
     DbGd_DB1_paths(1,1)="none"
   endif
   call PARSER_array(mode="write",VAR="DbGd_DB1_paths",DESC="Paths to the SAVE folders with the Double Grid DB1",&
&                    N=n_DbGd_DBs,M=1,CH_v=DbGd_DB1_paths(:n_DbGd_DBs,:))
 endif
 !
 ! LOG/REP files
 !
 call INIT_report_and_log_files()
 !
 live_timing_is_on =master_cpu
 write_to_report   =master_cpu
 write_to_log      =master_cpu
 !
 ! Input file
 !
 if (infile_editing) then
   open(unit=12,file=trim(infile))
   call initinfio(defs,12)
   close(12)
   call PP_redux_wait
 endif
 !
 if (master_cpu) then
   call rm_file(trim(repfile))
   open(unit=11,file=trim(repfile))
   call write_the_logo(11,' ')
   close(11)
 endif
 !
contains
 !
 subroutine call_INIT_load(mode) 
   character(*)::mode
   !
   if (mode=='load')       initmode=0
   if (mode=='todef')      initmode=1
   if (mode=='Gclose')     initmode=2
   if (mode=='GameOver')   initmode=3
   if (mode=='parserload') initmode=4
   call ypp_INIT_load(defs)
   !
 end subroutine
 !
 subroutine USER_vector_handle(field)
   !
   use parser_m,       ONLY:PARSER_array
   !
   character(*)::field
   !
   integer     ::nk,nq_s
   !
   call PARSER_array(mode="REAL dimensions",VAR=field,N=nk,M=3)
   !
   if (field=="GWKpts") then
     if (nk> 0) n_u_k_pts=nk
     if (nk==0) n_u_k_pts=1
     YAMBO_ALLOC(U_k_pts,(n_u_k_pts,3))
     if (nk> 0) call PARSER_array(mode="read",VAR=field,N=nk,M=3,R_v=U_k_pts)
     if (nk==0) U_k_pts=0.
     call PARSER_array(mode="write",VAR=field,DESC='K points list',N=n_u_k_pts,M=3,R_v=U_k_pts)
   else if (field=="BANDS_kpts") then
     if (nk> 0) then 
       n_u_interp_pts=nk
       YAMBO_ALLOC(U_interp_pts,(n_u_interp_pts,3))
       call PARSER_array(mode="read",VAR=field,N=nk,M=3,R_v=U_interp_pts)
       call PARSER_array(mode="write",VAR=field,DESC='K points of the bands circuit',&
&                        N=n_u_interp_pts,M=3,R_v=U_interp_pts)
     else
       n_u_interp_pts=0
       call PARSER_array(mode="write",VAR=field,DESC='K points of the bands circuit',N=0,M=3)
     endif
   else if (field=="Qpts") then
     if (nk> 0) n_u_q_pts=nk
     if (nk==0) n_u_q_pts=1
     YAMBO_ALLOC(U_q_pts,(n_u_q_pts,3))
     if (nk> 0) call PARSER_array(mode="read",VAR=field,N=nk,M=3,R_v=U_q_pts)
     if (nk==0) U_q_pts=0.
     call PARSER_array(mode="write",VAR=field,DESC='Q points list',&
&                      N=n_u_q_pts,M=3,R_v=U_q_pts)
   else if (field=="KShift") then
     U_q_shifts=0.
     if (nk> 0) call PARSER_array(mode="read",VAR=field,N=min(nk,3),M=3,R_v=U_q_shifts)
     call PARSER_array(mode="write",VAR=field,DESC='K-grid shifts (max 3)',&
&                      N=3,M=3,R_v=U_q_shifts)
   endif
   !
 end subroutine
 !
end subroutine
