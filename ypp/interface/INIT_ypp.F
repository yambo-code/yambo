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
! MA 02repfile_unit1-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine INIT_ypp(E,instr,FINALIZE)
 !
 use units,          ONLY:FS2AUT
 use YPPm,           ONLY:l_plot,l_map_WFs,l_map_kpts,l_mag,l_k_grid,l_high_sym_pts,BSiq,&
&                         l_free_hole,l_fix_syms,l_excitons,l_exc_wf,l_fix_save,&
&                         l_amplitude,l_bands,l_bxsf,l_bz_grids,l_current,BANDS_bands,&
&                         l_density,l_dos,l_electrons,l_q_grid,l_QP_DBs_create_and_modify,&
&                         l_QP_DBs_manipulate,l_random_grid,l_shifted_grid,l_sort,l_sp_wf,l_spin,&
&                         l_wannier,l_wavefunction,n_DbGd_DBs,n_u_interp_pts,n_u_k_pts,n_u_q_pts,&
&                         U_k_pts,U_interp_pts,U_q_pts,U_q_shifts,DbGd_DB1_paths,l_angular_momentum,l_position
 use pars,           ONLY:pi,schlen
 use D_lattice,      ONLY:i_time_rev
 use it_tools,       ONLY:ofiles_append,infile_dump,runlevel_is_on,&
&                         PARSER_force_command_line_value
 use it_m,           ONLY:initdefs,initmode,infile,nrnlvls,rnlvls,infile_verbosity,&
                          V_general,V_qp,V_all,V_real_time,V_nl_optics
 use drivers,        ONLY:infile_editing
 use com,            ONLY:write_to_report,repfile,repfile_unit,inpfile_unit
 use stderr,         ONLY:STRING_split,write_to_log,log_as_a_file,logfile,logfile_unit,STRING_match,STRING_same
 use electrons,      ONLY:n_spin,n_spinor,levels
 use parallel_m,     ONLY:master_cpu,PP_redux_wait
 use LIVE_t,         ONLY:live_timing_is_on
 use parser_m,       ONLY:PARSER_array
 use IO_m,           ONLY:rm_file,OP_RD_CL,DUMP,io_control,NONE
#if defined _YPP_SC
 use YPPm,           ONLY:l_mean_potential
 use SC,             ONLY:SC_bands
#endif
#if defined _YPP_RT || defined _YPP_SC
 use QP_m,           ONLY:QP_nb
 use electrons,      ONLY:levels,E_reset
#endif
#if defined _YPP_RT 
 use YPP_real_time,  ONLY:l_RealTime,l_RT_dos,l_RT_abs,l_RT_bands,l_RT_DBs,l_RT_density,&
&                         l_RT_energy,l_RT_lifetimes,l_RT_occupations,l_RT_time,l_RT_X,l_RTpump_Fermi,&
&                         l_RTpump_energy,l_RTpump_kspace,Eh_pumped_pair_n_BZ_regions,Eh_pumped_pair_BZ_regions,&
&                         l_NL_X,l_NL_exc,RT_conf
 use real_time,      ONLY:RT_bands
#endif
#if defined _YPP_NL 
 use nl_optics,      ONLY:NL_damping
 use pars,           ONLY:SP
#endif
#if defined _YPP_ELPH
 use YPP_ELPH
#endif
#if defined _YAML_OUTPUT
 use pars,           ONLY:logfile_index,repfile_index
 use com,            ONLY:of_yaml_IDs
 use yaml_output,    ONLY:yaml_close_stream
#endif
 !
#include<memory.h>
 !
 type(levels)     ::E
 character(*)     ::instr
 logical          ::FINALIZE
 !
 ! Work Space
 !
 logical          ::l_init_QP,l_flag
 type(initdefs)   ::defs
 integer          ::i1,i2,ID,io_err,i_cycle
 character(schlen)::rstr_piece(60) 
 !
 integer, external::io_RIM,io_COL_CUT,io_ATMPROJ_pwscf
 logical, external::file_exists 
 !
#if defined _YPP_SC
 type(levels)     ::E_dummy
 integer, external::io_SC_components
 integer          ::io_SC_err
#endif
#if defined _YPP_RT
 integer, external::io_RT_components,io_KB_pwscf,io_KB_abinit
 integer          ::io_RT_G_err,io_RT_OBS_err,io_KB_pwscf_err,io_KB_abinit_err
#endif
#if defined _YPP_NL
 integer, external::io_NL
 integer          ::io_Nonlinear
#endif
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
#if defined _YPP_SC
 ! 
 call E_reset(E_dummy) 
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) ! Current response
 io_SC_err=io_SC_components('WF' ,E_dummy,ID)
 if (io_SC_err==0) QP_nb=SC_bands(2)
 !
#endif
 !
#if defined _YPP_RT
 !
 ! GPL_EXCLUDE_START
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) ! GF
 io_RT_G_err=io_RT_components('G' ,ID)
 if (io_RT_G_err==0) QP_nb=RT_bands(2)
 !
 ! GPL_EXCLUDE_END
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
 ! ATOMIC PROJETIONS pwscf
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
 call call_INIT_load('load_defaults')
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
   call initactivate(1,'ElecTemp BoseTemp')
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
       !
       ! GPL_EXCLUDE_START 
       !
       l_high_sym_pts=STRING_match(rstr_piece(i1+1),'h')
       !
       ! GPL_EXCLUDE_END
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
       l_fix_save=STRING_match(rstr_piece(i1+1),'c')
       l_map_WFs =STRING_match(rstr_piece(i1+1),'p')
       if (l_map_WFs) call initactivate(1,'WFs_map')
     endif
     !
     if ( trim(rstr_piece(i1)) == 'QPDBs') then
       call initactivate(1,'QPDBs')
       l_QP_DBs_create_and_modify=STRING_match(rstr_piece(i1+1),'g')
       l_QP_DBs_manipulate            =STRING_match(rstr_piece(i1+1),'m')
       if (l_QP_DBs_create_and_modify) call initactivate(1,'QPDB_edit')
       if (l_QP_DBs_manipulate)        call initactivate(1,'QPDB_merge')
     endif
     !
#if defined _YPP_ELPH
     if ( trim(rstr_piece(i1)) == 'phonons') then
       l_dos              =STRING_match(rstr_piece(i1+1),'d')
       l_eliashberg       =STRING_match(rstr_piece(i1+1),'e')
       l_atomic_amplitude =STRING_match(rstr_piece(i1+1),'a')
       if (l_atomic_amplitude) infile_editing=.false.
       if (l_eliashberg)   call initactivate(1,'eliashberg')
     endif
#endif
     if ( trim(rstr_piece(i1)) == 'BSiq') then
       !
       read(rstr_piece(i1+1),'(i10)') BSiq 
       if (l_sort) infile_editing=.false.
       !
       call PARSER_force_command_line_value(defs,"BSQindex",I1=BSiq)
       !
     endif
     if ( trim(rstr_piece(i1)) == 'excitons') then
       l_wavefunction   =STRING_match(rstr_piece(i1+1),'w')
       l_sort           =STRING_match(rstr_piece(i1+1),'s')
       l_amplitude      =STRING_match(rstr_piece(i1+1),'a')
#if defined _YPP_ELPH
       l_gkkp           =STRING_match(rstr_piece(i1+1),'g')
#endif
       !
       l_spin  =STRING_match(rstr_piece(i1+1),'sp').and.n_spin>1
       l_mag   =STRING_match(rstr_piece(i1+1),'m').and.n_spin>1
       !
       ! GPL_EXCLUDE_START
       !
#if defined _YPP_ELPH
       l_eliashberg      =STRING_match(rstr_piece(i1+1),'e')
#endif
       ! GPL_EXCLUDE_END
       !
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
#if defined _YPP_ELPH
       l_eliashberg       =STRING_match(rstr_piece(i1+1),'e')
#endif
       !
#if defined _YPP_MAGNETIC
       l_angular_momentum =STRING_match(rstr_piece(i1+1),'l')
       l_position         =STRING_match(rstr_piece(i1+1),'p')
#endif
       !
     endif
     !
#if defined _YPP_RT
     !
     ! GPL_EXCLUDE_START
     !
     if ( trim(rstr_piece(i1)) == 'RTDBs') then
       l_RTpump_energy=STRING_match(rstr_piece(i1+1),'e')
       l_RTpump_kspace=STRING_match(rstr_piece(i1+1),'k')
       l_RTpump_Fermi =STRING_match(rstr_piece(i1+1),'f')
     endif
     !
     ! GPL_EXCLUDE_END
     !
     if ( trim(rstr_piece(i1)) == 'RealTime' .or. trim(rstr_piece(i1)) == 'TDplots') then
       !
       l_RT_X           =STRING_match(rstr_piece(i1+1),'X')
       !
       ! GPL_EXCLUDE_START
       l_RT_occupations =STRING_match(rstr_piece(i1+1),'o')
       l_RT_lifetimes   =STRING_match(rstr_piece(i1+1),'l')
       l_RT_density     =STRING_match(rstr_piece(i1+1),'d')
       l_RT_abs         =STRING_match(rstr_piece(i1+1),'a')
       ! GPL_EXCLUDE_END
       !
     endif
     !
     ! GPL_EXCLUDE_START
     !
     if ( trim(rstr_piece(i1)) == 'RealTime' .or. trim(rstr_piece(i1)) == 'TDplotmode') then
       !
       l_RT_bands       =STRING_match(rstr_piece(i1+1),'b')
       l_RT_time        =STRING_match(rstr_piece(i1+1),'t')
       l_RT_energy      =STRING_match(rstr_piece(i1+1),'e')
       l_RT_dos         =STRING_match(rstr_piece(i1+1),'d')
       !
     endif
     !
     ! GPL_EXCLUDE_END
     !
#endif
     !
#if defined _YPP_NL
     l_NL_X   =  trim(rstr_piece(i1)) == 'nonlinear'
#endif   
     !    
   enddo
 enddo
 !
#if defined _YPP_RT
 !
 if (l_RT_X)           call initactivate(1,'RT_X')
 !
 ! GPL_EXCLUDE_START
 !
 if (l_RT_abs)         call initactivate(1,'RTabs')
 if (l_RT_occupations) call initactivate(1,'RToccupations')
 if (l_RT_lifetimes)   call initactivate(1,'RTlifetimes')
 if (l_RT_density)     call initactivate(1,'RTdeltaRho')
 if (l_RT_bands)       call initactivate(1,'RTfitbands')
 if (l_RT_time)        call initactivate(1,'RTtime')
 if (l_RT_energy)      call initactivate(1,'RTenergy')
 if (l_RT_dos)         call initactivate(1,'RTdos')
#endif
 !
 if (l_amplitude)      call initactivate(1,'amplitude')
 if (l_spin)           call initactivate(1,'spin')
 if (l_mag)            call initactivate(1,'magnetization')
 if (l_wavefunction)   call initactivate(1,'wavefunction')
 !
#if defined _YPP_ELPH
 !
 if (l_eliashberg)       call initactivate(1,'eliashberg')
 if (l_gkkp)             call initactivate(1,'gkkp')
 !
#endif
#if defined _YPP_MAGNETIC
 !
 if (l_angular_momentum) call initactivate(1,'angular_m')
 if (l_position)         call initactivate(1,'position')
 !
#endif
 !
 if (l_current)        call initactivate(1,'current')
 if (l_density)        call initactivate(1,'density')
 if (l_sort)           infile_editing=.false.
 if (l_dos)            call initactivate(1,'dos')
 if (l_bands)          call initactivate(1,'bnds')
 !
#if defined _YPP_RT
 !
 ! GPL_EXCLUDE_START
 !
 if (l_RTpump_energy) call initactivate(1,'Select_energy')
 if (l_RTpump_kspace) call initactivate(1,'Select_kspace')
 if (l_RTpump_Fermi ) call initactivate(1,'Select_Fermi')
 !
 ! GPL_EXCLUDE_END
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
 !
#if defined _YPP_NL
 if (l_NL_X)         call initactivate(1,'nonlinear')
#endif
 !
 l_flag=.not.(l_sort.or.l_fix_save)
#if defined _YPP_ELPH
 l_flag=.not.l_sort.and..not.l_atomic_amplitude
#endif
 if (l_flag) call call_INIT_load('parser_input_file')
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
#if defined _YPP_SC
 l_mean_potential=runlevel_is_on('MeanPot')
 if (l_mean_potential) then
   call initactivate(1,'plot')
   call initactivate(1,'electrons')
 endif
#endif
#if defined _YPP_ELPH
 l_eliashberg=runlevel_is_on('eliashberg')
 l_phonons   =runlevel_is_on('phonons')
 l_gkkp      =runlevel_is_on('gkkp')
#endif
#if defined _YPP_RT
 !
 l_RealTime         = runlevel_is_on('RealTime').or.runlevel_is_on('TDplots').or.runlevel_is_on('TDpol')
 l_RT_X             = runlevel_is_on('RT_X')
 !
 ! GPL_EXCLUDE_START
 !
 l_RT_DBs           = runlevel_is_on('RTDBs')
 !
 l_RTpump_energy    = runlevel_is_on('Select_energy')
 l_RTpump_kspace    = runlevel_is_on('Select_kspace')
 l_RTpump_Fermi     = runlevel_is_on('Select_Fermi')
 !
 l_RT_abs           = runlevel_is_on('RTabs')
 l_RT_occupations   = runlevel_is_on('RToccupations')
 l_RT_lifetimes     = runlevel_is_on('RTlifetimes')
 l_RT_density       = runlevel_is_on('RTdeltaRho')
 !
 l_RT_bands         = runlevel_is_on('RTfitbands')
 l_RT_time          = runlevel_is_on('RTtime')
 l_RT_energy        = runlevel_is_on('RTenergy')
 l_RT_dos           = runlevel_is_on('RTdos')
 !
 ! GPL_EXCLUDE_END
 !
#endif
#if defined _YPP_NL
 l_NL_X   =runlevel_is_on('nonlinear')
#endif
#if defined _YPP_MAGNETIC
 l_angular_momentum = runlevel_is_on('angular_m').and.runlevel_is_on('electrons')
 l_position         = runlevel_is_on('position').and.runlevel_is_on('electrons')
#endif
 l_excitons =runlevel_is_on('excitons')
 l_electrons=runlevel_is_on('electrons')
 l_plot=any((/runlevel_is_on('wavefunction'),runlevel_is_on('magnetization'),&
&             runlevel_is_on('density'),runlevel_is_on('current')/))
#if defined _YPP_SC
 l_plot=l_plot .or. runlevel_is_on('MeanPot')
#endif
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
 l_flag=l_bands
#if defined _YPP_RT
 l_flag=l_bands.or.l_RT_bands
#endif
 !
 if (l_flag) then
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
 ! GPL_EXCLUDE_START
 !
#if defined _YPP_SC
 if (l_mean_potential) call initactivate(1,"PotValue")
#endif
 !
 ! GPL_EXCLUDE_END
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
 ! GPL_EXCLUDE_START
 !
 ! Wannier 90 interface
 !
 if (l_wannier) call initactivate(1,"Seed WriteAMU QPdbPath")
 !
 ! GPL_EXCLUDE_END
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
     call initactivate(1,"Proj_DOS PDOS_atoms PDOS_kinds PDOS_wfcs PDOS_l PDOS_j PDOS_m")
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
 !
 if (l_RealTime) then
   !
   ! GPL_EXCLUDE_START
   !
   l_flag=l_RT_occupations.or.l_RT_abs
   if (l_flag)      call init_RT_ctl_switch('G')
   l_flag=l_RT_occupations.or.l_RT_density.or.l_RT_lifetimes.or.l_RT_abs
   if (l_flag)      call initactivate(1,"TimeStep TimeRange")
   !
   l_flag=l_RT_time.and.l_RT_occupations
   if (l_flag)           call initactivate(1,"OCCgroup")
   if (l_RT_occupations) call initactivate(1,"IncludeEQocc SkipFermiFIT")
   if (l_RT_density)     call initactivate(1,"LoadGhistory IncludeEQocc")
   if (l_RT_dos)  then
     call initactivate(1,"DOSERange DOSESteps DOS_broad SeparateEH IncludeEQocc NoOcc")
     call initactivate(1,"Proj_DOS PDOS_atoms PDOS_kinds PDOS_wfcs PDOS_l PDOS_j PDOS_m")
     call initactivate(1,"INTERP_Grid INTERP_Shell_Fac")
     call INIT_QP_ctl_switch('G')
   endif
   if ((l_RT_dos.or.l_RT_time).and.n_spinor==2) call initactivate(1,'WFReference')
   !
   ! GPL_EXCLUDE_END
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
#if defined _YPP_MAGNETIC
 if (l_angular_momentum.or.l_position) call initactivate(1,"FFTGvecs")
 if (l_current) call initactivate(1,'PhaseTrick')
#endif
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
 if (l_spin.and..not.l_excitons) call initactivate(1,"FFTGvecs")
 !
 if (l_excitons) then
   call initactivate(1,"States En_treshold Res_treshold BSQindex")
   if (l_amplitude) then
     call INIT_QP_ctl_switch('G')
     call initactivate(1,"DipWeight")
   endif
   if (l_exc_wf.or.l_amplitude) call initactivate(1,"Degen_Step Weight_treshold")
   if (l_exc_wf.and..not.l_free_hole) call initactivate(1,"Cells Hole Dimension") 
   if (l_exc_wf.and.l_free_hole) call initactivate(1,"WFMult") 
   if (l_spin) call initactivate(1,"Degen_Step")
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
   inpfile_unit=12
   open(unit=inpfile_unit,file=trim(infile))
   call initinfio(defs,inpfile_unit)
   close(inpfile_unit)
   call PP_redux_wait
 endif
 !
 if (l_QP_DBs_create_and_modify.or.l_QP_DBs_manipulate) call QP_DBs_initialize()
 !
 l_init_QP=l_plot.and.l_sp_wf
#if defined _YPP_ELPH
 if (.not.l_init_QP) l_init_QP=l_electrons.and.l_eliashberg
#endif
#if defined _YPP_SC
 if (.not.l_init_QP) l_init_QP=l_electrons.and.l_mean_potential
#endif
#if defined _YPP_MAGNETIC
 if (.not.l_init_QP) l_init_QP=l_angular_momentum.or.l_position
#endif
 !
 ! GPL_EXCLUDE_START
 !
#if defined _YPP_RT
 if (.not.l_init_QP) l_init_QP=(l_RT_occupations.or.l_RT_lifetimes).and..not.l_RT_bands.and..not.l_RT_dos
 if (     l_init_QP) call INIT_QP(" ")
#else
 ! GPL_EXCLUDE_END
 if (l_init_QP) call INIT_QP(" ")
 ! GPL_EXCLUDE_START
#endif
 !
 ! GPL_EXCLUDE_END
 !
 ! Check and fill all possible vectorial USER fields
 !
 l_flag=l_bands
 !
 ! GPL_EXCLUDE_START
 !
#if defined _YPP_RT
 l_flag=l_flag.or.l_RT_bands
#endif
 !
 ! GPL_EXCLUDE_END
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
   open(unit=inpfile_unit,file=trim(infile))
   call initinfio(defs,inpfile_unit)
   close(inpfile_unit)
   call PP_redux_wait
 endif
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
     use YPPm,           ONLY:DOS_bands,SOC_bands_to_map,wannier_bands
     use electrons,      ONLY:n_bands
     !
     ! After DB reading/stdin logicals I propose here values for the input file
     ! 
     SOC_bands_to_map=(/ 1, n_bands /)
     BANDS_bands=(/1,n_bands/)
     DOS_bands=(/1,n_bands/)
     wannier_bands=(/1,n_bands/)
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
&                          N=n_u_interp_pts,M=3,R_v=U_interp_pts)
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
&                        N=n_u_q_pts,M=3,R_v=U_q_pts)
     else if (field=="KShift") then
       U_q_shifts=0.
       if (nk> 0) call PARSER_array(mode="read",VAR=field,N=min(nk,3),M=3,R_v=U_q_shifts)
       call PARSER_array(mode="write",VAR=field,DESC='K-grid shifts (max 3)',&
&                        N=3,M=3,R_v=U_q_shifts)
     endif
     !
   end subroutine
   !
end subroutine
