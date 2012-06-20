!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine ypp_init(instr,lnstr,FINALIZE)
 !
 use YPP
 use pars,           ONLY:pi
 use units,          ONLY:HA2EV,FS2AUT
 use wave_func,      ONLY:wf_ng 
 use D_lattice,      ONLY:alat
 use R_lattice,      ONLY:ng_vec,nkibz
 use it_m,           ONLY:initdefs,initmode,ofiles_append,&
&                         initinfio,infile,infile_dump,initactivate,&
&                         nrnlvls,rnlvls,runlevel_is_on,&
&                         infile_verbosity,V_general,V_qp,V_all,V_real_time
 use drivers,        ONLY:infile_editing
 use com,            ONLY:file_exists
 use stderr,         ONLY:string_split
 use electrons,      ONLY:n_spin,n_bands
 use par_proc_m,     ONLY:master_cpu,pp_redux_wait
 use timing,         ONLY:what_is_running
#if defined _YPP_SC
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nb
#endif
#if defined _YPP_RT || defined _YPP_SC
 use electrons,      ONLY:levels,E_reset
 use IO_m,           ONLY:OP_RD_CL,DUMP,io_control,NONE
#endif
#if defined _YPP_RT 
 use real_time,      ONLY:RT_step,NE_steps
 use fields,         ONLY:Efield,i_Probe
#endif
#if defined _YPP_ELPH
 use ELPH,           ONLY:gsqF_energy_steps
#endif
 implicit none
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
#if defined _YPP_RT || defined _YPP_SC
 type(levels)     ::E_dummy
 integer          ::io_SC_err,ID
 integer, external::io_SC_components
#endif
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
#if defined _YPP_TEST_2
 ! BANDS
 !
 BANDS_steps=0
 BANDS_range=0
 !
#endif
 !
 ! PLOT
 !
 p_dir      ='1'
 mag_dir    ='X'
 p_format   ='g'
#if !defined _YPP_RT
 wf_ng=ng_vec
 ! Notice that ng_vec can be larger than maxval(wf_igk) 
 ! and this can cause some problem in ypp. TO BE CHECKED
#endif
 l_exc_wf   =.false.
 l_density  =.false.
 V_value    ='RE'
#if defined _YPP_TEST_2
 l_bands=.false.
#endif
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
 gsqF_energy_steps=1
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
 coo_in="iku"
 coo_out="rlu"
 Kgw_1=0.
 q_shift= 0.0_SP
 PtsPath= ' '
 N_path_pts=0
 alat_used_for_output=alat(1)
 !
 !BZ RIM
 !
 BZ_RIM_path="."
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
 Magnetization=0._SP
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
 ! CURRENT
 !
#if defined _YPP_BOLTZMANN
 l_current=.false.
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
 call E_reset(E_dummy) 
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) ! Current response
 io_SC_err=io_SC_components('G' ,E_dummy,ID)
 ! 
 if (io_SC_err==0) then
   !
   X_rt%energy_range=(/0._SP,pi/RT_step/)
   X_rt%time_range=(/Efield(i_Probe)%t_initial,NE_steps*RT_step/)
   X_rt%damp_factor=2.*pi/(RT_step*real(NE_steps)-Efield(i_Probe)%t_initial)
   !
 endif
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
       ! GPL_EXCLUDE_START SPIN_IGNORE
       !
       l_spin  =trim(rstr_piece(i1+1))=='sp'.and.n_spin>1
       l_mag   =trim(rstr_piece(i1+1))=='m'.and.n_spin>1
       !
       ! GPL_EXCLUDE_END SPIN_IGNORE
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
       l_density        =trim(rstr_piece(i1+1))=='d'
       l_dos            =trim(rstr_piece(i1+1))=='s'
       l_wavefunction   =trim(rstr_piece(i1+1))=='w'
#if defined _YPP_TEST_2
       l_bands          =trim(rstr_piece(i1+1))=='b'
#endif
       !
       ! GPL_EXCLUDE_START SPIN_IGNORE
       !
       l_mag   =trim(rstr_piece(i1+1))=='m'.and.n_spin>1
       !
       ! GPL_EXCLUDE_END SPIN_IGNORE
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
       if (l_density)        call initactivate(1,'density')
       if (l_sort)           infile_editing=.false.
     endif
     if ( trim(rstr_piece(i1)) == 'phonons'.or. trim(rstr_piece(i1)) == 'electrons') then
       if (l_dos)            call initactivate(1,'dos')
     endif
     if ( trim(rstr_piece(i1)) == 'electrons') then
#if defined _YPP_TEST_2	     
       if (l_bands)          call initactivate(1,'bands')
#endif	       
     endif
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
 l_fix_syms         = runlevel_is_on('fixsyms')
 l_RT_X             = runlevel_is_on('rtX')
#endif
#if defined _YPP_BOLTZMANN
 l_current          = runlevel_is_on('current')
 if (l_current) call initactivate(1,'electrons')
#endif
#if defined _YPP_MAGNETIC
 l_angular_momentum = runlevel_is_on('angular_m').and.runlevel_is_on('electrons')
 l_position         = runlevel_is_on('position').and.runlevel_is_on('electrons')
#endif
 l_excitons =runlevel_is_on('excitons')
 l_electrons=runlevel_is_on('electrons')
 l_plot=any((/runlevel_is_on('wavefunction'),runlevel_is_on('magnetization'),&
#if defined _YPP_BOLTZMANN
&             runlevel_is_on('current'), &
#endif
#if defined _YPP_SC
&             runlevel_is_on('MeanPot'), &
#endif
&             runlevel_is_on('density')/))
#if defined _YPP_TEST_2 
 l_bands     =runlevel_is_on('bands')
#endif 
 l_free_hole=runlevel_is_on('freehole')
 l_amplitude=runlevel_is_on('amplitude')
 l_exc_wf   =runlevel_is_on('wavefunction').and.runlevel_is_on('excitons')
 l_sp_wf    =runlevel_is_on('wavefunction').and.runlevel_is_on('electrons')
 l_density  =runlevel_is_on('density').and.runlevel_is_on('electrons')
 l_mag      =runlevel_is_on('magnetization')
 l_wannier  =runlevel_is_on('wannier')
 if (.not.l_exc_wf) l_free_hole=.false.
 !
 ! GPL_EXCLUDE_START SPIN_IGNORE
 !
 l_spin     =runlevel_is_on('exc_spin')
 !
 ! GPL_EXCLUDE_END SPIN_IGNORE 
 !
 if (l_bz_grids) then
   call initactivate(1,"cooIn cooOut")
   if (l_k_grid) call initactivate(1,"GWKpts")
   if (l_q_grid) call initactivate(1,"Qpts")
   if (l_shifted_grid) call initactivate(1,"KShift1 KShift2 KShift3")
   if (l_high_sym_pts) then
     call initactivate(1,"PtsPath NPtsPath")
     call initactivate(-1,"NoWeights ForceUserPts cooIn")
   else
     call initactivate(1,"NoWeights ForceUserPts")
   endif
 endif
 !
#if defined _YPP_TEST_2
 if (l_bands) call initactivate(1,"cooIn BKpts BANDS_steps BANDS_range")
#endif	 
 !	   
 l_bzrim     =runlevel_is_on('bzrim')
 !
 l_bxsf      =runlevel_is_on('bxsf')
 !
 if (l_bxsf) call initactivate(1,"W90_fname WannBnds DgridOrd")
 !
 l_qpdb = runlevel_is_on('qpdb')
 !
 ! GPL_EXCLUDE_START
 !
#if defined _YPP_SC
 if (l_mean_potential) then
   call initactivate(1,"PotValue")
 endif
#endif
 !
#if defined _YPP_RT
 !             
 if (l_fix_syms) call initactivate(1,"Efield1 Efield2 BField Bpsi Btheta Magnetization RmAllSymm RmTimeRev RmInvMBaxis")
 !
 if (l_RT_X) then 
   call initactivate(1,"Xorder ProbeKey PumpKey EnRngeRt ETStpsRt")
   call initactivate(1,"TmRngeRt TmJumps DampMode DampFactor SkipJP_IO FrMinDamp RmAsymLim UseFFT")
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
 if (l_bzrim) call initactivate(1,"BZ_RIM_path BZ_RIM_Nk cooOut OutputAlat NoWeights GammaRadius")
 !
 if (l_electrons) then
    if (l_dos.or.l_bands) call QP_ctl_switch('G')
    if (l_dos) then
      call initactivate(1,"OCCdT OCCTime")
      call initactivate(1,"DOS_bands DOSERange DOSESteps DOS_broad")
    endif
 endif
 !
#if defined _YPP_MAGNETIC
 if (l_angular_momentum.or.l_position) call initactivate(1,"FFTGvecs")
 if (l_current) call initactivate(1,'PhaseTrick')
#endif
#if defined _YPP_BOLTZMANN
 if (l_current) call initactivate(1,"FFTGvecs Nelectro ElecTemp Tau El_Field")
#endif
 !
 if (l_plot) then
   call initactivate(1,"Format Direction FFTGvecs") 
   if (l_sp_wf) call initactivate(1,"Degen_Step")  
   if (l_mag)   call initactivate(1,"MagDir") 
 endif
 !
 if (l_excitons) then
   if (l_amplitude) call QP_ctl_switch('G')
   call initactivate(1,"States")
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
 if (l_gkkp.and..not.l_excitons)   call initactivate(1,"DBsPATH PHfreqF")
 if (l_phonons.and.l_eliashberg)   call initactivate(1,"EEfermi EfGbroad")
 if ( ((l_excitons.or.l_electrons).and.l_eliashberg) .or. (l_phonons.and.l_dos) ) call initactivate(1,"PhBroad PhStps")
#endif
 !
 if (infile_editing) then
   open(unit=12,file=trim(infile))
   call initinfio(defs,12)
   close(12)
   call pp_redux_wait
 endif
 !
 if(l_qpdb) call qpdb_init()
 !
 l_QP_init=l_plot.and.l_sp_wf
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
 if (l_QP_init) call QP_init(.FALSE.,.FALSE.)
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
