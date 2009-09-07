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
subroutine ypp_init(instr,lnstr,FINALIZE)
 !
 use units,          ONLY:HARTREE,FS2AUT
 use wave_func,      ONLY:wf_ng 
 use R_lattice,      ONLY:ng_vec
 use it_m,           ONLY:initdefs,initmode,ofiles_append,&
&                         initinfio,infile,infile_dump,initactivate,&
&                         nrnlvls,rnlvls,runlevel_is_on,&
&                         infile_verbosity
 use drivers,        ONLY:infile_editing
 use com,            ONLY:file_exists
 use stderr,         ONLY:string_split
 use YPP
 use electrons,      ONLY:n_spin,n_bands
 use par_proc_m,     ONLY:master_cpu,pp_redux_wait
 use timing,         ONLY:what_is_running
#if defined _YPP_ELPH
 use ELPH,           ONLY:gsqF_energy_steps
#endif
#if defined _YPP_RAS
 use bulkeps,        only : bshift, bbroad, bfile, btype 
 use convolute,      only : gbroad
 use eels_kinematics,      ONLY : bimp, E0, theta0_in, thetap_in, phi_in
 use model_loss_function,  ONLY : model
 use surface_geometry,     ONLY : nlayers
 use eels_detector,        ONLY : thetadet, det_int_type, n_det_pt

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
 character(schlen)::rstr_piece(50) 
 !
 !
 ! What is running ?
 !
 what_is_running='YPP'
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
 else
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
   !REMOVE SYMMETRIES
   !
   efield=0._SP
   !
   ! Probe external field to calcualted dielectric constant
   ! 
   ref_efield=1
   !
   !ANALIZE RESPONSE
   !
   X_rt%time_range(:)  =(/-FS2AUT,-FS2AUT/)
   X_rt%energy_steps   =100
   X_rt%energy_range(:)=(/0.,10./HARTREE/)
   X_rt%damp           ="LORENTZIAN"
   X_rt%damp_factor    =0.1/HARTREE
   X_rt%noise_ratio    =200
   !
#endif
   ! GPL_EXCLUDE_END
   !
 endif
 !
 ! Defaults->Defs + Initialization
 !
 call call_init_load('load')
 !
 ! Dump internally the input file
 !
 if (file_exists(trim(infile))) call infile_dump()
 !
 !RunLevels on
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
   if ( trim(rstr_piece(i1)) == 'infver' )  read(rstr_piece(i1+1),*) infile_verbosity
   if (infile_verbosity<0) infile_verbosity=0
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
       !
#if defined _YPP_ELPH
       l_eliashberg      =trim(rstr_piece(i1+1))=='e'
#endif
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
       if (l_density)        call initactivate(1,'density')
       if (l_sort)           infile_editing=.false.
     endif
     if ( trim(rstr_piece(i1)) == 'phonons'.or. trim(rstr_piece(i1)) == 'electrons') then
       if (l_dos)            call initactivate(1,'dos')
     endif
   enddo
 enddo
 !
 if (.not.l_sort) call call_init_load('parserload')
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
 l_excitons =runlevel_is_on('excitons')
 l_electrons=runlevel_is_on('electrons')
 l_plot=any((/runlevel_is_on('wavefunction'),runlevel_is_on('magnetization'),&
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
 l_rsymm     =runlevel_is_on('rsymm')
 !             
 l_rtoptics  =runlevel_is_on('rtoptics')
 !
 if (l_rsymm) call initactivate(1,"ExtField1 ExtField2 RmAllSymm")
 !
 if (l_rtoptics) then 
    call initactivate(1,"EnRngeRt ETStpsRt TmRngeRt DampMode DampFactor FourierDamp")
    call initactivate(1,"RefExtField NoiseRatio PrtCurrent PrtEinduced UseFFT")
 endif
 ! 
#endif
 if (l_bzrim) call initactivate(1,"BZ_RIM_path BZ_RIM_Nk cooOut GammaRadius")
 !
 ! GPL_EXCLUDE_END
 !
 if (l_electrons.and.l_dos) call initactivate(1,"DOS_broad DOS_bands")
 !
 if (l_plot) then
   call initactivate(1,"Format Direction FFTGvecs") 
   if (l_sp_wf) call initactivate(1,"Degen_Step")  
   if (l_mag)   call initactivate(1,"MagDir") 
 endif
 !
 if (l_excitons) then
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
 if (l_phonons.and.(l_dos.or.l_eliashberg))   call initactivate(1,"EEfermi EfGbroad PhBroad PhStps")
 if ( ((l_excitons.or.l_electrons).and.l_eliashberg) .or. (l_phonons.and.l_dos) ) &
&                                  call initactivate(-1,"EEfermi EfGbroad")
#endif

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
   if(lloc) call initactivate(1,"NormDir LowerLim UpperLim NGLoc")
   if(ltrans) call initactivate(1,"EcvMin EcvMax PolDir")
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
