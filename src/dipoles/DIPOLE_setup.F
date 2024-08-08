!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine DIPOLE_setup(Dip,use_dipole_transverse)
 !
 use drivers,        ONLY:l_real_time,l_sc_run,l_nl_optics
 use DIPOLES,        ONLY:DIPOLE_t,use_covariant_approach,use_real_space_approach,&
&                         compute_P2_dipoles,compute_Orb_dipoles,compute_Spin_dipoles,&
&                         use_g_space_approach,eval_OVERLAPS,use_shifted_grids_approach,compute_P_dipoles
 use IO_m,           ONLY:io_DIP
 use BS,             ONLY:BSE_prop,l_BS_magnons,l_BS_dichroism,l_BS_photolum
 use electrons,      ONLY:n_spin
 use parser_m,       ONLY:parser
#if defined _SLEPC && !defined _NL
 use BS_solvers,     ONLY:BSS_mode
#endif
 !
 implicit none
 !
 type(DIPOLE_t),intent(inout) :: Dip
 logical                      :: use_dipole_transverse
 !
 ! Work Space
 !
#if defined _NL
 logical           :: l_dip_appr_from_user
#endif
 !
 call section('+','Setup: observables and procedures')
 !=====================================================
 !
 ! io_DIP
 !-------
 ! DS 2017/08/22 : Case with io_DIP=.false. to be fixed
 if (.not.io_DIP) then
   call warning(' Cannot use DIP_P on the flight at the moment. Setting io_DIP=.true.')
   io_DIP=.true.
 endif
 !
 ! Observables (needed later)
 !-------------
 l_BS_magnons=   (index(BSE_prop,'magn')/=0)
 l_BS_dichroism= (index(BSE_prop,'dich')/=0)
 l_BS_photolum=  (index(BSE_prop,'photolum')/=0)
 if (l_BS_dichroism.or.l_BS_magnons .or. l_BS_photolum) Dip%bands_ordered=.false.
#if defined _SLEPC && !defined _NL
 if ( index(BSS_mode,'s')/=0 )  Dip%bands_ordered=.false.
#endif
 !
 ! logicals (approach)
 !--------------------
#if defined _NL
 if (l_nl_optics) then
   !      
   ! In NL I always force the use of covariant dipoles
   ! because they have a coerent phase, while normal dipole don't
   ! this can be see in a districtive interference in the polarization
   ! perpendicular to the field direction
   ! I expect that dipole covariant decently reproduce non-linear response
   !
   l_dip_appr_from_user=.false.
   call parser("DipApproach",l_dip_appr_from_user)
   if (.not.l_dip_appr_from_user) then
     Dip%approach="Covariant"
     Dip%ib(1)   =1
   endif
 endif
#endif
 !
 use_g_space_approach        = trim(Dip%approach)=='G-space v'
 use_real_space_approach     = trim(Dip%approach)=='R-space x'
 use_covariant_approach      = trim(Dip%approach)=='Covariant'
 use_shifted_grids_approach  = trim(Dip%approach)=='Shifted grids'
 if(.not.any((/use_g_space_approach,use_real_space_approach,use_covariant_approach,use_shifted_grids_approach/))) then
   call warning(' Dipoles approach defined in input not recognised. Reverting to G-space v')
   use_g_space_approach = .true.
   Dip%approach='G-space v'
 endif
 !
 call parser('PDirect'  ,Dip%force_v_g_space)
#if defined _SC || defined _RT || defined _NL
 Dip%force_v_g_space=Dip%force_v_g_space.or.l_sc_run.or.l_real_time.or.l_nl_optics
#endif
 Dip%force_v_g_space     =Dip%force_v_g_space.or.l_BS_photolum
 Dip%force_v_g_space     =Dip%force_v_g_space.and.(.not.use_g_space_approach)
 use_dipole_transverse   =use_g_space_approach
 !
 ! logicals (components)
 !-----------------------
 compute_P_dipoles           = use_g_space_approach !.or.use_dipole_transverse
 compute_P2_dipoles          = index(Dip%computed,"P2")  /=0
 compute_Spin_dipoles        = index(Dip%computed,"Spin")/=0
 compute_Orb_dipoles         = index(Dip%computed,"Orb") /=0
#if defined _SC
 compute_P2_dipoles          = compute_P2_dipoles .or. l_sc_run
#endif
#if defined _RT
 compute_P2_dipoles          = compute_P2_dipoles   .or. l_real_time
 compute_Spin_dipoles        = compute_Spin_dipoles .or. (l_real_time .and. n_spin>1)
 compute_Orb_dipoles         = compute_Orb_dipoles  .or. (l_real_time .and. n_spin>1)
#endif
 compute_Spin_dipoles        = compute_Spin_dipoles .or. (l_BS_magnons .and. n_spin>1)
 ! 
 ! Computed components
 !---------------------
 Dip%computed='R V'
 if (compute_P_dipoles)    Dip%computed=trim(Dip%computed)//' P'
 if (compute_P2_dipoles)   Dip%computed=trim(Dip%computed)//' P2'
 if (compute_Spin_dipoles) Dip%computed=trim(Dip%computed)//' Spin'
 if (compute_Orb_dipoles)  Dip%computed=trim(Dip%computed)//' Orb'
 !
 if (use_covariant_approach)  eval_OVERLAPS=.true.
 !
end subroutine
