!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine ypp_init(E,instr,lnstr,FINALIZE)
 !
 use YPP
 use pars,           ONLY:pi
 use interpolate,    ONLY:lpfac
 use units,          ONLY:HA2EV,FS2AUT
 use D_lattice,      ONLY:alat,i_time_rev
 use it_m,           ONLY:initdefs,initmode,ofiles_append,&
&                         initinfio,infile,infile_dump,initactivate,&
&                         nrnlvls,rnlvls,runlevel_is_on,&
&                         infile_verbosity,V_general,V_qp,V_all,V_real_time
 use drivers,        ONLY:infile_editing
 use com,            ONLY:file_exists
 use stderr,         ONLY:string_split
 use electrons,      ONLY:n_spin,n_bands,n_spinor,levels
 use parallel_m,     ONLY:master_cpu,PP_redux_wait
 use LIVE_t,         ONLY:what_is_running
#if defined _YPP_SC
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nb
#endif
#if defined _YPP_RT || defined _YPP_SC
 use electrons,      ONLY:levels,E_reset
 use IO_m,           ONLY:OP_RD_CL,DUMP,io_control,NONE
#endif
#if defined _YPP_RT 
 use rt_ctl,         ONLY:RT_IO_JPO_steps,OCC_T_range,OCC_deltaT
 use real_time,      ONLY:RT_step,NE_steps
 use fields,         ONLY:Efield,i_Probe
#endif
#if defined _YPP_ELPH
 use ELPH,           ONLY:gsqF_energy_steps
#endif
#if defined _YPP_SURF
 use R_lattice,      ONLY:nkibz
#endif
 implicit none
 type(levels)     ::E
 integer          ::lnstr
 character(lnstr) ::instr
 logical          ::FINALIZE
 !
 ! Work Space
 !
 logical          ::l_QP_init
 type(initdefs)   ::defs
 integer          ::i1,i2
 character(schlen)::rstr_piece(60) 
 !
#if defined _YPP_SC
 type(levels)     ::E_dummy
 integer, external::io_SC_components
 integer          ::io_SC_err,ID
#endif
#if defined _YPP_RT
 logical          ::l_flag
 integer, external::io_RT_components,io_KB_pwscf,io_kb_abinit
 integer          ::io_RT_G_err,io_RT_J_err,io_KB_pwscf_err,io_kb_abinit_err,ID
#endif
 integer, external::parse_character_arrays
 !
 ! What is running ?
 !
 what_is_running='YPP'
#if defined _YPP_RT
 what_is_running='YPP_RT'
#endif
#if defined _YPP_MAGNETIC
 what_is_running='YPP_MAGNETIC'
#endif
#if defined _YPP_ELPH
 what_is_running='YPP_PH'
#endif
#if defined _YPP_SURF
 what_is_running='YPP_SURF'
#endif
#if defined _YPP_RT
 what_is_running='YPP_RT'
#endif
 !
 if (FINALIZE) then
   call call_init_load('GameOver')
   if (master_cpu) call ofiles_append(defs=defs)
   return
 endif
 !
 ! DEFAULTS
 ! ======== 
 !
 ! DOS
 !
 l_dos=.false.
 dos_broadening=0.1/HA2EV
 dos_bands=(/1,n_bands/)
 dos_E_range=(/1.,-1./)/HA2EV
 dos_E_steps=500
 !
 !
 ! RT OCCUPATIONS and DOS
 !
#if defined _YPP_RT
 OCC_T_range=0.
 OCC_deltaT=0.
#endif
 !
 l_RealTime=.FALSE.
 l_RT_X=.FALSE.
 l_RT_abs=.FALSE.
 l_RT_occ_Plot=.FALSE.
 l_RT_occ_Fit=.FALSE.
 l_RT_dos=.FALSE.
 l_RT_occ_Bands=.FALSE.
 l_RT_life_Bands=.FALSE.
 !
 !
 ! BANDS
 !
 BANDS_steps=0
 l_bands=.false.
 lpfac=20._SP
 interp_grid = (/-1, -1, -1/)
#if defined _YPP_RT
 Nel_fac=1._SP
#endif
 !
 !
 ! PLOT
 !
 p_dir      ='1'
 mag_dir    ='X'
 p_format   ='g'
 l_exc_wf   =.false.
 l_density  =.false.
 l_current  =.false.
 V_value    ='RE'
 l_mag=.false.
 l_sp_wf=.false.
#if defined _YPP_SC
 l_mean_potential=.false.
#endif
 l_norm_to_one=.true.
 output_fname=' '
 plot_title=' '
 !
 ! EXC WF
 !
 l_spin=.false.
 l_sort=.false.
 l_amplitude=.false.
 ncell = (/1, 1, 1/) 
 r_hole= (/0.,0.,0./)
 lambda = 1          
 state_ctl = '1 - 1'
 deg_energy =0.01/HA2EV
 min_weight =0.01
 WF_multiplier=1.
 !
#if defined _YPP_ELPH
 !
 ! ELPH
 !
 l_phonons=.false.
 l_gkkp=.false.
 l_eliashberg=.false.
 l_atomic_amplitude=.false.
 elph_steps=200
 ph_broad=0.01/HA2EV
 elph_gamma_broad=0.0
 elph_Ef=0.
 elph_dbs_path='.'
 ph_freqs_file='none'
 ph_modes_file='none'
 gsqF_energy_steps=1
 !
#endif
 !
#if defined _YPP_SURF
 !
 ! RAS & REELS
 !
 call ras_presets
 xdata = 'o.eps_q001-rpa_00'
 ydata = 'o.eps_q001-rpa_00'
 zdata = 'o.eps_q001-rpa_00'
 sshift = 0.0           ! surface shift
 datatype = 'eps23'
 d_cellin = 1.0
 !
 ! Localization
 !
 normdir = 3
 lowerlim = 0
 upperlim = 1
 ngloc = 0
 loc_bands = (/ 1, n_bands /)
 loc_kpts  = (/ 1, nkibz /)
 ! 
 ! Transition analysis
 ! 
 Ecv_min = 0.0
 Ecv_max = -1.0/HA2EV
 idir = 1
 !
#endif
 !
 ! Wannier interface
 !
 l_wannier=.FALSE.
 seed_name=" "
 what_to_write=" "
 !
 ! BZ grids
 !
 coo_in="rlu"
 coo_out="rlu"
 Kgw_1=0.
 q_shift= 0.0_SP
 PtsPath= ' '
 N_path_pts=0
 alat_used_for_output=alat(1)
 !
 !BZ RIM
 !
 gamma_radius=0.
 !
 ! BXSF interpolation
 !
 w90_fname="./w90.bxsf"
 wannier_bands=(/1,n_bands/)
 ord_dgrid_ipol = 0
 !
 ! GPL_EXCLUDE_START
 !
#if defined _YPP_RT
 !
 t_jump=0.
 !
 !REMOVE SYMMETRIES
 !
 E_field=0._SP
 B_field=0._SP
 B_theta=0._SP
 B_psi=0._SP
 !
 !ANALIZE RESPONSE
 !
 X_rt%time_range(:)  =(/-FS2AUT,-FS2AUT/)
 X_rt%energy_steps   =200
 X_rt%energy_range(:)=(/0._SP,20./HA2EV/)
 X_rt%damp           ="NONE"
 X_rt%damp_factor    =0.
 Probe_Keyword ="none"
 Pump_Keyword  ="none"
 X_order       =1
 !
#endif
 !
 ! ANGULAR MOMENTUM & ELECTRON DISPLACEMENT
 !
#if defined _YPP_MAGNETIC
 !
 l_angular_momentum=.false.
 l_position=.false.
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
 ! DATABASES 
 !===========
 !
 !
#if defined _YPP_SC
 ! 
 call E_reset(E_dummy) 
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) ! Current response
 io_SC_err=io_SC_components('WF' ,E_dummy,ID)
 !
 if (io_SC_err==0) QP_nb=SC_bands(2)
 !
#endif
 !
#if defined _YPP_RT
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) ! GF
 io_RT_G_err=io_RT_components('G' ,ID)
 ! 
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) ! Current response
 io_RT_J_err=io_RT_components('J_and_P' ,ID)
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_KB_pwscf_err=io_KB_pwscf(ID)
 ! 
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_KB_abinit_err=io_KB_abinit(ID)
 !
#endif
 !
 ! Defaults->Defs + Initialization
 !
 call call_init_load('load')
 !
 ! Dump internally the input file
 !
 if (file_exists(trim(infile))) call infile_dump()
 !
 ! RunLevels on
 !
 call string_split(instr,rstr_piece)
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
     if( trim(rstr_piece(i1)) == 'infver' .and. trim(rstr_piece(i1+1))=='qp' )  infile_verbosity=V_qp
     if( trim(rstr_piece(i1)) == 'infver' .and. trim(rstr_piece(i1+1))=='all' )  infile_verbosity=V_all
   endif
   !
   call initactivate(1,'ElecTemp BoseTemp')
   !
   do i2=1,nrnlvls
     if ( trim(rnlvls(i2,1)) == trim(rstr_piece(i1)) ) then
       infile_editing=.true.
       call initactivate(1,trim(rnlvls(i2,1)))
     endif
     if ( trim(rstr_piece(i1)) == 'bzgrids') then
       l_k_grid=trim(rstr_piece(i1+1))=='k'
       l_q_grid=trim(rstr_piece(i1+1))=='q'
       l_shifted_grid=trim(rstr_piece(i1+1))=='s'
       !
       ! GPL_EXCLUDE_START 
       !
       l_high_sym_pts=trim(rstr_piece(i1+1))=='h'
       !
       ! GPL_EXCLUDE_END
       !
       if (l_k_grid) call initactivate(1,"K_grid")
       if (l_q_grid) call initactivate(1,"Q_grid")
       if (l_shifted_grid) call initactivate(1,"Shifted_Grid")
       if (l_high_sym_pts) call initactivate(1,"High_Symm")
     endif
     !
     if ( trim(rstr_piece(i1)) == 'QPDBs') then
       call initactivate(1,'QPDBs')
       l_QP_DBs_create_and_modify=trim(rstr_piece(i1+1))=='g'
       l_QP_DBs_merge            =trim(rstr_piece(i1+1))=='m'
       if (l_QP_DBs_create_and_modify) call initactivate(1,'QPDB_edit')
       if (l_QP_DBs_merge)             call initactivate(1,'QPDB_merge')
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
       ! GPL_EXCLUDE_START
       !
#if defined _YPP_ELPH
       l_eliashberg      =trim(rstr_piece(i1+1))=='e'
#endif
       ! GPL_EXCLUDE_END
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
#if defined _YPP_MAGNETIC
       l_angular_momentum =trim(rstr_piece(i1+1))=='l'
       l_position         =trim(rstr_piece(i1+1))=='p'
#endif
       !
     endif
     !
#if defined _YPP_RT
     if ( trim(rstr_piece(i1)) == 'RealTime') then
       !
       l_RT_X           =trim(rstr_piece(i1+1))=='X'
       l_RT_abs         =trim(rstr_piece(i1+1))=='a'
       l_RT_dos         =trim(rstr_piece(i1+1))=='d'
       l_RT_life_Bands  =trim(rstr_piece(i1+1))=='lb'
       l_RT_occ_Bands   =trim(rstr_piece(i1+1))=='ob'
       l_RT_occ_Plot    =trim(rstr_piece(i1+1))=='op'
       l_RT_occ_Fit     =trim(rstr_piece(i1+1))=='of'
       !
     endif
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
#if defined _YPP_MAGNETIC
       if (l_angular_momentum) call initactivate(1,'angular_m')
       if (l_position)         call initactivate(1,'position')
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
     if ( trim(rstr_piece(i1)) == 'RealTime') then
       if (l_RT_X)           call initactivate(1,'RT_X')
       if (l_RT_abs)         call initactivate(1,'RTabs')
       if (l_RT_dos)         call initactivate(1,'RTcarrDos')
       if (l_RT_life_Bands)  call initactivate(1,'RTlifeBnd')
       if (l_RT_occ_Bands)   call initactivate(1,'RToccBnd')
       if (l_RT_occ_Plot)    call initactivate(1,'RToccPlot')
       if (l_RT_occ_Fit)     call initactivate(1,'RToccFit')
     endif
#endif

   enddo
 enddo
 !
 if (.not.l_sort.and..not.l_atomic_amplitude) call call_init_load('parserload')
 !
 l_bz_grids  = runlevel_is_on('bzgrids')
 l_k_grid    = runlevel_is_on('K_grid')
 l_q_grid    = runlevel_is_on('Q_grid')
 l_shifted_grid = runlevel_is_on('Shifted_Grid')
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
 l_RealTime         = runlevel_is_on('RealTime')
 l_RT_X             = runlevel_is_on('RT_X')
 l_RT_abs           = runlevel_is_on('RTabs')
 l_RT_dos           = runlevel_is_on('RTcarrDos')
 l_RT_life_Bands    = runlevel_is_on('RTlifeBnd')
 l_RT_occ_Bands     = runlevel_is_on('RToccBnd')
 l_RT_occ_Plot      = runlevel_is_on('RToccPlot')
 l_RT_occ_Fit       = runlevel_is_on('RToccFit')
 !
#endif
#if defined _YPP_MAGNETIC
 l_angular_momentum = runlevel_is_on('angular_m').and.runlevel_is_on('electrons')
 l_position         = runlevel_is_on('position').and.runlevel_is_on('electrons')
#endif
 l_excitons =runlevel_is_on('excitons')
 l_electrons=runlevel_is_on('electrons')
 l_plot=any((/runlevel_is_on('wavefunction'),runlevel_is_on('magnetization'),&
#if defined _YPP_SC
&             runlevel_is_on('MeanPot'), &
#endif
&             runlevel_is_on('density'),runlevel_is_on('current')/))
 l_free_hole=runlevel_is_on('freehole')
 l_amplitude=runlevel_is_on('amplitude')
 l_exc_wf   =runlevel_is_on('wavefunction').and.runlevel_is_on('excitons')
 l_sp_wf    =runlevel_is_on('wavefunction').and.runlevel_is_on('electrons')
 l_density  =runlevel_is_on('density').and.runlevel_is_on('electrons')
 l_current  =runlevel_is_on('current').and.runlevel_is_on('electrons')
 l_mag      =runlevel_is_on('magnetization')
 l_wannier  =runlevel_is_on('wannier')
 l_spin     =runlevel_is_on('spin')
 !
 if (.not.l_exc_wf) l_free_hole=.false.
 !
 if (l_bz_grids) then
   call initactivate(1,"cooIn cooOut")
   if (l_k_grid) call initactivate(1,"GWKpts")
   if (l_q_grid) call initactivate(1,"Qpts")
   if (l_shifted_grid) call initactivate(1,"KShift1 KShift2 KShift3")
   if (l_high_sym_pts) then
     call initactivate(1,"PtsPath NPtsPath")
     call initactivate(-1,"NoWeights ForceUserPts ListPts ExpandPts cooIn")
   else
     call initactivate(1,"NoWeights ForceUserPts ListPts ExpandPts")
   endif
 endif
 !
 if (l_bands) call initactivate(1,"cooIn BKpts BANDS_steps INTERPGrid ShellFac NormN NelFac")
 !	   
 l_bzrim     =runlevel_is_on('bzrim')
 !
 l_map_kpts  = runlevel_is_on('kpts_map')
 !
 l_bxsf      =runlevel_is_on('bxsf')
 !
 if (l_bxsf) call initactivate(1,"W90_fname WannBnds DgridOrd")
 !
 l_QP_DBs_create_and_modify = runlevel_is_on('QPDB_edit')
 l_QP_DBs_merge = runlevel_is_on('QPDB_merge')
 !
 ! GPL_EXCLUDE_START
 !
#if defined _YPP_SC
 if (l_mean_potential) then
   call initactivate(1,"PotValue")
 endif
#endif
 !
 ! GPL_EXCLUDE_END
 !
 if (l_fix_syms.and.n_spinor==2.and.i_time_rev==0) call initactivate(1,"ExpandSymm")
 !
 ! GPL_INCLUDE_START
 !
 !if (l_fix_syms) call initactivate(1,"RmAllSymm RmTimeRev")
 !
 ! GPL_INCLUDE_END
 !
 !
 ! GPL_EXCLUDE_START
 !
 if (l_fix_syms) call initactivate(1,"Efield1 Efield2 BField Bpsi Btheta RmAllSymm RmTimeRev")
 !
#if defined _YPP_RT
 !
 if(l_QP_DBs_merge) call initactivate(1,"Z_input")
 !             
 if (l_RT_X) then 
   call initactivate(1,"Xorder ProbeKey PumpKey EnRngeRt ETStpsRt")
   call initactivate(1,"TmRngeRt TmJumps DampMode DampFactor SkipJP_IO FrMinDamp RmAsymLim UseFFT CheckPaths")
 endif
 !
#endif
 !
 ! Wannier 90 interface
 !
 if (l_wannier) call initactivate(1,"Seed WriteAMU QPdbPath")
 !
 ! GPL_EXCLUDE_END
 !
 if (l_bzrim)    call initactivate(1,"BZ_RIM_Nk cooOut OutputAlat GammaRadius")
 !
 if (l_map_kpts) call initactivate(1,"BZ_RIM_Nk SkipCheck noBZExpand NoWeights")
 !
 if (l_electrons) then
   if (l_dos.or.l_bands) call init_QP_ctl_switch('G')
   if (l_dos) call initactivate(1,"DOSERange DOSESteps DOS_broad DOS_bands")
 endif
#if defined _YPP_RT
 if (l_RealTime) then
   l_flag=l_RT_dos.or.l_RT_occ_Plot.or.l_RT_occ_Fit.or.l_RT_occ_Bands.or.l_RT_abs
   if (l_flag) call init_QP_ctl_switch('G')
   if (l_flag) call initactivate(1,"OCCdT OCCTime OCCTref")
   if (l_RT_occ_Bands.or.l_RT_life_Bands) call initactivate(1,"cooIn BKpts BANDS_steps NNInterp")
   if (l_RT_occ_Plot) call initactivate(1,"OCCgroup")
   if (l_RT_abs) call initactivate(1,"EnRngeRt ETStpsRt DampFactor Transmission Reflectivity IncludeIP")
   if (l_RT_dos) call initactivate(1,"DOSERange DOSESteps DOS_broad")
 endif
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
 if (l_excitons) then
   call initactivate(1,"States")
   if (l_amplitude) then
     call init_QP_ctl_switch('G')
     call initactivate(1,"DipWeight")
   endif
   if (l_exc_wf.or.l_amplitude) call initactivate(1,"Degen_Step MinWeight")
   if (l_exc_wf.and..not.l_free_hole) call initactivate(1,"Cells Hole Dimension") 
   if (l_exc_wf.and.l_free_hole) call initactivate(1,"WFMult") 
   if (l_spin) call initactivate(1,"Degen_Step")
#if defined _YPP_ELPH
   if (l_eliashberg)  call initactivate(1,"Degen_Step") 
#endif
 endif
 !
#if defined _YPP_ELPH
 if (l_gkkp.and..not.l_excitons)   call initactivate(1,"DBsPATH PHfreqF PHmodeF GkkpExpand")
 if (l_phonons.and.l_eliashberg)   call initactivate(1,"EE_Fermi EfGbroad")
 if ( ((l_excitons.or.l_electrons).and.l_eliashberg) .or. (l_phonons.and.l_dos) ) call initactivate(1,"PhBroad PhStps")
#endif
 !
#if defined _YPP_SURF
 !
 lsurf =runlevel_is_on('surf')
 lras  =runlevel_is_on('ras')
 lreels=runlevel_is_on('reels')
 lloc  =runlevel_is_on('loc')
 ltrans=runlevel_is_on('trans')
 if(lsurf) then
   if (lras) call initactivate(1,"XData YData "//&
&       " BulkFile BulkForm BlkShift BlkBroad "//&
&       " DataType SrfShift d_cell")
   if (lreels) call initactivate(1,"XData YData ZData"//&
&       " q0x q0y"//&
&       " BulkFile BulkForm BlkShift BlkBroad Layers"//&
&       " DataType SrfShift d_cell "//&
&       " E0 Theta0 Thetap Phi DetAngle "//&
&       " LossForm ImpactFt DetIntMd NumIntPt GausConv")
   if(lloc) call initactivate(1,"NormDir LowerLim UpperLim NGLoc LbndRnge KptRange WrtLocDB")
   if(ltrans) call initactivate(1,"EcvMin EcvMax qdir")
 endif
 !
#endif
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
 if (l_QP_DBs_create_and_modify.or.l_QP_DBs_merge) call QP_DBs_initialize()
 !
 l_QP_init=(l_plot.and.l_sp_wf).or.l_bands
#if defined _YPP_ELPH
 if (.not.l_QP_init) l_QP_init=l_electrons.and.l_eliashberg
#endif
#if defined _YPP_SC
 if (.not.l_QP_init) l_QP_init=l_electrons.and.l_mean_potential
#endif
#if defined _YPP_MAGNETIC
 if (.not.l_QP_init) l_QP_init=l_angular_momentum.or.l_position
#endif
 !
#if defined _YPP_RT
 if (.not.l_QP_init) l_QP_init=l_RT_occ_Plot.or.l_RT_occ_Fit.or.l_RT_dos.or.l_RT_occ_bands.or.l_RT_life_Bands
 if (l_QP_init) call QP_init(.FALSE.,.TRUE.)
#else
 if (l_QP_init) call QP_init(.FALSE.,.FALSE.)
#endif
 !
 if (l_map_kpts) then
   n_RIM_DBs=parse_character_arrays(RIM_DB1_paths,MAX_RIM_DBs,"RIM_DB1_paths",.TRUE.)
   if (infile_editing) i1=parse_character_arrays(RIM_DB1_paths,n_RIM_DBs,"RIM_DB1_paths",.FALSE.)
 endif
 !
contains
 !
 subroutine call_init_load(mode) 
   character(*)::mode
   !
   if (mode=='load') initmode=0
   if (mode=='todef') initmode=1
   if (mode=='Gclose') initmode=2
   if (mode=='GameOver') initmode=3
   if (mode=='parserload') initmode=4
   call ypp_init_load(defs)
   !
 end subroutine
 !
end subroutine
