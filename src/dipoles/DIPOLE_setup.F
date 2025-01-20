!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine DIPOLE_setup(Dip)
 !
 use drivers,        ONLY:l_real_time,l_sc_run,l_nl_optics
 use DIPOLES,        ONLY:DIPOLE_t,x_space_obs,g_space_obs,covariant_obs,shifted_grids_obs,&
&                         specific_obs,eval_OVERLAPS
 use IO_m,           ONLY:io_DIP
 use BS,             ONLY:BSE_prop,l_BS_magnons,l_BS_dichroism,l_BS_photolum
 use electrons,      ONLY:n_spin,n_spinor
 use parser_m,       ONLY:parser
 use stderr,         ONLY:STRING_same,STRING_match
 use com,            ONLY:exp_user
#if defined _SLEPC && !defined _NL
 use BS_solvers,     ONLY:BSS_mode
#endif
 !
 implicit none
 !
 type(DIPOLE_t),intent(inout) :: Dip
 !
 ! Work Space
 !
 logical           :: l_Hbare
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
 !----------------------------
 l_BS_magnons=   (index(BSE_prop,'magn')/=0)
 l_BS_dichroism= (index(BSE_prop,'dich')/=0)
 l_BS_photolum=  (index(BSE_prop,'photolum')/=0)
 !
 ! Bands ordering
 !----------------
 if (l_BS_dichroism.or.l_BS_magnons .or. l_BS_photolum) Dip%bands_ordered=.false.
#if defined _SLEPC && !defined _NL
 if ( index(BSS_mode,'s')/=0 )  Dip%bands_ordered=.false.
#endif
 !
 ! Force covariant in NL
 !-----------------------
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
     call warning(' Dipoles approach forced to Covariant for nloptics calculations')
     Dip%approach="Covariant"
     Dip%ib(1)   =1
   endif
 endif
#endif
 !
 call parser('BareHfromScratch', l_Hbare)
 !
 ! If we have additional nonlocal contributions such as Hubbard U,
 ! we need to switch to the covariant approach to capture them
 ! 2024/04/10 DS: check should be changed, checking for lda+U, and not for l_Hbare
 if (l_Hbare.and..not.STRING_same(Dip%approach,"Covariant")) then
   if(exp_user) then
     call warning(' Covariant dipoles should be used since the Hamiltonian contains U non local projectors')
   else
     call warning(' Covariant forced, since Hamiltonian contains U non local projectors')
     Dip%approach="Covariant"
   endif
 endif
 !
 ! Force <V> to be calculated using the G-space
 !---------------------------------------------
 call parser('PDirect'  ,Dip%force_v_g_space)
 if (.not.STRING_same(Dip%approach,"g-space v")) then
#if defined _SC || defined _RT || defined _NL
   Dip%force_v_g_space=Dip%force_v_g_space.or.l_sc_run.or.l_real_time.or.l_nl_optics
#endif
   ! For some reason (to understand) also in the PL case Dip%force_v_g_space=.TRUE.
   Dip%force_v_g_space     =Dip%force_v_g_space.or.l_BS_photolum
 endif
 !
 ! Components based on approach
 !------------------------------
 !
 ! <P> is ALWAYS calculated in G-space
 g_space_obs="P"
 !
 if (STRING_same(Dip%approach,"r-space x")) then
   x_space_obs="R V"
   if (Dip%force_v_g_space) x_space_obs="R"
 else if (STRING_same(Dip%approach,"covariant")) then
   covariant_obs="S R V"
   if (Dip%force_v_g_space) covariant_obs="S R"
   eval_OVERLAPS=.true.
 else if (STRING_same(Dip%approach,"shifted grids")) then
   shifted_grids_obs="R V"
   if (Dip%force_v_g_space) shifted_grids_obs="R"
 else if (STRING_same(Dip%approach,"g-space v")) then
   g_space_obs="R V P"
 else
   call warning(' User approach undefined. Switching to G-space v')
   g_space_obs="R V P"
 endif 
 !
 ! ... V 
 if (Dip%force_v_g_space.and..not.STRING_match(g_space_obs,"V")) g_space_obs=trim(g_space_obs)//" V"
 !
 ! ... P2
 !
#if defined _RT
 if (l_real_time) then
   g_space_obs=trim(g_space_obs)//" P2"
 endif
#else
 ! P2 is needed for kinetic part of bare Hamiltonian
 if (STRING_match(Dip%computed,"p2") .or. l_Hbare) g_space_obs=trim(g_space_obs)//" P2"
#endif
 !
 !
 ! ... P_spinor
 if (n_spinor==2) then
#if defined _RT
   if (l_real_time) g_space_obs=trim(g_space_obs)//" P_spinor"
#else
   if (STRING_match(Dip%computed,"spin").or.l_BS_magnons) g_space_obs=trim(g_space_obs)//" P_spinor"
#endif
 endif
 ! ...M_spin, M_orb, M_iti, M_orb_CD
 if (n_spin>1) then
#if defined _RT
   if (l_real_time) specific_obs=" M_spin M_orb M_it"
#else
   if (STRING_match(Dip%computed,"orb")) specific_obs=trim(specific_obs)//" M_orb M_it"
#endif
   if (STRING_match(Dip%computed,"spin").or.l_BS_magnons) specific_obs=" M_spin"
 endif
 if (l_BS_dichroism) specific_obs=trim(specific_obs)//" M_CD_orb"
 ! 
 ! Computed components
 !---------------------
 Dip%computed=" "
 if (len_trim(x_space_obs  )    >0) Dip%computed=trim(x_space_obs)//" [X-space]"
 if (len_trim(covariant_obs)    >0) Dip%computed=trim(covariant_obs)//" [covarian]"
 if (len_trim(shifted_grids_obs)>0) Dip%computed=trim(shifted_grids_obs)//" [shift-grids]"
 if (len_trim(Dip%computed)==0) then
   Dip%computed=trim(g_space_obs)//" [G-space]"
 else
   Dip%computed=trim(dip%computed)//" "//trim(g_space_obs)//" [G-space]"
 endif
 Dip%computed=trim(dip%computed)//" "//trim(specific_obs)
 !
end subroutine
