!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
 use units,          ONLY:HARTREE,FS2AUT
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
#if defined _YPP_RT
 use electrons,      ONLY:levels,E_reset
 use IO_m,           ONLY:OP_RD_CL,DUMP,io_control,NONE
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
 type(initdefs)   ::defs
 integer          ::i1,i2
 character(schlen)::rstr_piece(60) 
 !
#if defined _YPP_RT
 type(levels)     ::E_rt
 integer          ::io_G,ID
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
#if defined _YPP_RAS
 what_is_running='YPP_RAS'
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
 dos_broadening=0.1/HARTREE
 dos_bands=n_bands
 !
 ! PLOT
 !
 p_dir      ='1'
 mag_dir    ='X'
 p_format   ='g'
 wf_ng      =ng_vec
 l_exc_wf=.false.
 l_density=.false.
 l_mag=.false.
 l_sp_wf=.false.
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
 deg_energy =0.01/HARTREE
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
 ph_broad=0.01/HARTREE
 elph_gamma_broad=0.0
 elph_Ef=0.
 elph_dbs_path='.'
 ph_freqs_file='none'
 gsqF_energy_steps=1
 !
#endif
 !
#if defined _YPP_RAS
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
 Ecv_max = -1.0/HARTREE
 idir = 1
 !
#endif
 !
 ! BZ grids
 !
 coo_in="iku"
 coo_out="rlu"
 Kgw_1=0.
 qlong= 0.0_SP
 PtsPath= ' '
 N_path_pts=0
 alat_used_for_output=alat(1)
 !
 ! GPL_EXCLUDE_START
 !
 !BZ RIM
 !
 BZ_RIM_path="."
 gamma_radius=0.
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
 X_rt%energy_range(:)=(/0._SP,20./HARTREE/)
 X_rt%damp           ="NONE"
 X_rt%damp_factor    =0.
 Probe_Keyword ="none"
 Pump_Keyword ="none"
 !
#endif
 !
 ! ANGULAR MOMENTUM & ELECTRON DISPLACEMENT
 !
#if defined _YPP_BOLTZMANN
 l_current=.false.
#endif
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
#if defined _YPP_RT
 ! 
 call E_reset(E_rt) 
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) ! Current response
 io_G=io_SC_components('G' ,E_rt,ID)
 ! 
 if (io_G==0) then
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
       l_long_grid=trim(rstr_piece(i1+1))=='l'
       !
       ! GPL_EXCLUDE_START 
       !
       l_high_sym_pts=trim(rstr_piece(i1+1))=='h'
       !
       ! GPL_EXCLUDE_END
       !
       if (l_k_grid) call initactivate(1,"K_grid")
       if (l_q_grid) call initactivate(1,"Q_grid")
       if (l_long_grid) call initactivate(1,"Long_Gauge")
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
#if defined _YPP_RT
     if ( trim(rstr_piece(i1)) == 'rtpp') then
       l_RT_X_one      =trim(rstr_piece(i1+1))=='o'
       l_RT_X_two      =trim(rstr_piece(i1+1))=='t'
       if (l_RT_X_one) call initactivate(1,'RToptics')
       if (l_RT_X_two) call initactivate(1,'RTnlo')
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
   enddo
 enddo
 !
 if (.not.l_sort.and..not.l_atomic_amplitude) call call_init_load('parserload')
 !
 l_bz_grids  = runlevel_is_on('bzgrids')
 l_k_grid    = runlevel_is_on('K_grid')
 l_q_grid    = runlevel_is_on('Q_grid')
 l_long_grid = runlevel_is_on('Long_Gauge')
 l_high_sym_pts = runlevel_is_on('High_Symm')
 l_dos       =runlevel_is_on('dos')
#if defined _YPP_ELPH
 l_eliashberg=runlevel_is_on('eliashberg')
 l_phonons   =runlevel_is_on('phonons')
 l_gkkp      =runlevel_is_on('gkkp')
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
&             runlevel_is_on('density')/))
 l_free_hole=runlevel_is_on('freehole')
 l_amplitude=runlevel_is_on('amplitude')
 l_exc_wf   =runlevel_is_on('wavefunction').and.runlevel_is_on('excitons')
 l_sp_wf    =runlevel_is_on('wavefunction').and.runlevel_is_on('electrons')
 l_density  =runlevel_is_on('density').and.runlevel_is_on('electrons')
 l_mag      =runlevel_is_on('magnetization')
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
   if (l_long_grid) call initactivate(1,"KShift1 KShift2 KShift3")
   if (l_high_sym_pts) then
     call initactivate(1,"PtsPath NPtsPath")
     call initactivate(-1,"NoWeights ForceUserPts cooIn")
   else
     call initactivate(1,"NoWeights ForceUserPts")
   endif
 endif
 !
 ! GPL_EXCLUDE_START
 !
 l_bzrim     =runlevel_is_on('bzrim')
 !
#if defined _YPP_RT
 l_fix_syms   =runlevel_is_on('fixsyms')
 !             
 l_RT_X_one  =runlevel_is_on('RToptics')
 !
 l_RT_X_two =runlevel_is_on('RTnlo')
 !
 if (l_fix_syms) call initactivate(1,"Efield1 Efield2 BField Bpsi Btheta RmAllSymm RmTimeRev")
 !
 if (l_RT_X_one.or.l_RT_X_two) then 
    call initactivate(1,"ProbeKey PumpKey")
    if (l_RT_X_one) call initactivate(1,"EnRngeRt ETStpsRt")
    call initactivate(1,"TmRngeRt TmJumps DampMode DampFactor FrMinDamp")
    if (l_RT_X_one) call initactivate(1,"UseFFT")
 endif
 !
 ! 
#endif
 if (l_bzrim) call initactivate(1,"BZ_RIM_path BZ_RIM_Nk cooOut OutputAlat GammaRadius")
 !
 ! GPL_EXCLUDE_END
 !
 if (l_electrons.and.l_dos) then
    if(l_dos) call QP_ctl_switch('G')
    call initactivate(1,"DOS_broad DOS_bands")
 endif
#if defined _YPP_MAGNETIC
 if (l_angular_momentum.or.l_position) call initactivate(1,"FFTGvecs")
#endif
#if defined _YPP_BOLTZMANN
 if (l_current) call initactivate(1,"FFTGvecs Nelectro ElecTemp Tau El_Field")
#endif
#if defined _YPP_MAGNETIC
 if (l_current) call initactivate(1,'PhaseTrick')
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
   if (l_exc_wf.or.l_amplitude) call initactivate(1,"Degen_Step")
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
#if defined _YPP_RAS
 !
 lsurf =runlevel_is_on('surf')
 lras  =runlevel_is_on('ras')
 lreels=runlevel_is_on('reels')
 lloc  =runlevel_is_on('loc')
 ltrans=runlevel_is_on('trans')
 if(lsurf) then
   if (lras) call initactivate(1,"XData YData "//&
&       " BulkFile BulkForm BlkShift BlkBroad "//&
&       " DataType SrfShift d_cell GausConv")
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
 if (infile_editing) then
   open(unit=12,file=trim(infile))
   call initinfio(defs,12)
   close(12)
   call pp_redux_wait
 endif
 !
#if defined _YPP_ELPH
 if ((l_plot.and.l_sp_wf).or.(l_electrons.and.l_eliashberg)) &
#else
 if (l_plot.and.l_sp_wf) &
#endif
&                        call QP_init(.FALSE.,.FALSE.)
#if defined _YPP_MAGNETIC
 if (l_angular_momentum.or.l_position) call QP_init(.FALSE.,.FALSE.)
#endif
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
