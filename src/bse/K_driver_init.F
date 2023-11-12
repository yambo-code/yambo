!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine K_driver_init(what,iq,Ken,Xk)
 !
 use R_lattice,    ONLY:bz_samp
 use electrons,    ONLY:levels,n_sp_pol,n_spinor
 use parser_m,     ONLY:parser
 use parallel_m,   ONLY:PARALLEL_default_mode
 use stderr,       ONLY:STRING_match,STRING_same
 use BS_solvers,   ONLY:BSS_eels_to_eps,BSS_mode,BSS_slepc_matrix,BSS_uses_DbGd,&
&                       l_ab_from_eels,BSS_slepc_double_grp
 use BS,           ONLY:BSE_L_kind,BSE_mode,BS_K_is_ALDA,BS_dip_size,l_BSE_minimize_memory,BS_perturbative_SOC,&
&                       BS_perturbative_SOC,l_BS_magnons,l_BS_photolum,&
&                       BS_cpl_K_exchange,BS_n_g_exch,BS_res_K_exchange,BS_K_coupling,BS_res_ares_n_mat,&
&                       BS_n_eh_spaces,l_BS_ares_from_res,BS_bands,BS_K_anti_resonant
 use TDDFT,        ONLY:FXC_is_retarded,FXC_n_g_corr
 use drivers,      ONLY:l_col_cut,l_rpa_IP,l_alda_fxc,l_bs_fxc
 use D_lattice,    ONLY:i_time_rev,i_space_inv,l_0D,l_1D,l_2D,l_3D
#if defined _RT
 use drivers,      ONLY:l_rt_carriers_in_use
 use RT_control,   ONLY:NEQ_Kernel,EQ_Transitions,EQ_NoOcc,NEQ_Residuals,RT_BSE_Occ_Mode
#endif
 !
 implicit none
 !
 character(*)  ::what
 integer       ::iq
 type(levels)  ::Ken
 type(bz_samp) ::Xk
 !
 ! Work Space
 !
 logical :: l_flag,l_slepc_special
 !
 ! Defaults
 !==========
 BS_n_eh_spaces=1
 BS_res_ares_n_mat=1
 !
 ! BSE_L_kind check
 !==============
 if (.not.STRING_match(BSE_L_kind,"bar").and..not.STRING_match(BSE_L_kind,"full")) call error('set Lkind = BAR or FULL')
 !
 if (STRING_match(BSE_L_kind,"full").and.(STRING_match(BSE_mode,'causal').or.STRING_match(BSE_mode,'retarded'))) then
   call warning(' Lkind=full with retarded leads to bad epsilon. Use either resonant or coupling. Setting resonant. ')
   BSE_mode="resonant"
 endif
 !
 ! RES/ANTI-RES (q-dependent)
 !==============
 !
 ! Do I need the anti-res part?
 if (BS_K_coupling) BS_K_anti_resonant=.TRUE.
 if (STRING_match(BSE_mode,'causal').or.STRING_match(BSE_mode,'retarded')) BS_K_anti_resonant=.TRUE.
#if defined _RT 
 if (l_rt_carriers_in_use) BS_K_anti_resonant=.TRUE.
#endif
 !
 ! Is ARES derivable from RES?
 !-----------------------------
 ! Developer options. Without the code works in default mode
 ! With the user can switch to experimental coding
 call parser('ImposeAsym',l_flag)
 if (l_flag)        l_BS_ares_from_res=.FALSE.
 !
 if (iq/=1.and.i_time_rev==0.and.i_space_inv==0 )  l_BS_ares_from_res=.FALSE.
 if (l_BS_photolum)                                l_BS_ares_from_res=.FALSE.
 if (l_BS_magnons.and.n_sp_pol==2)                 l_BS_ares_from_res=.FALSE.
 ! The next line is to switch of section of the codes due to the calculation
 ! of the ARES part without checking the BSE_mode (see io_BS_PAR_init for example)
 if (trim(BSE_mode)=="resonant")                   l_BS_ares_from_res=.TRUE.
 !
 ! I need ARES and I cannot get it from RES
 ! 
 if (BS_K_anti_resonant.and..not.l_BS_ares_from_res) then
   BS_n_eh_spaces=2
   if (.not.BS_K_coupling) BS_res_ares_n_mat=2
 endif
 !
 ! Need to convert eels to epsilon ? (part A, part B in K_driver)
 !================================================================
 !
 ! If the system is 3D eps/=eps^-1
 if (l_3D)          BSS_eels_to_eps=STRING_match(BSE_L_kind,"full")
 !
 ! If the Coulomb cutoff is     used ...
 if (l_col_cut) then
   !
   !...the system is 0D  eps= eps^-1=1 since vbar=0
   if (l_0D)  BSS_eels_to_eps=.false.
   !
   ! If the Coulomb cutoff is used and the system is 1D or 2D eps and eels are different at finite q
   if (l_1D.or.l_2D) then
     if (iq==1) BSS_eels_to_eps=.false.
     if (iq/=1) BSS_eels_to_eps=STRING_match(BSE_L_kind,"full")
   endif
   !
 endif
 !
 ! In general alpha depend on eels
 ! Without cutoff, this is used only in 3D, and in (2D or 1D) at q>0,
 ! otherwise eps=eel and eps converges faster to the +inf volume limit,
 ! thus it is convenient un relate alpha to eps
 !
 ! Also to remember that standard BSE is with Lbar, i.e. something proportional to eps
 ! For the case Lfull we could use the same logic of G-space since we have something proportional to eps^-1
 ! The following logical could be merged with the BSS_eels_to_eps logical
 !-------
 l_ab_from_eels=.false.
 !if ( l_3D .or. (  (l_1D.or.l_2D) .and. iq>1 ) .or. (l_col_cut .and. STRING_same(BSE_L_kind,"Lbar")) ) l_ab_from_eels = .true.
 if ( l_3D .or. (  (l_1D.or.l_2D) .and. iq>1 ) .or. l_col_cut ) l_ab_from_eels = .true.
 !
 if (what=="loop-init") return
 !
 BS_K_is_ALDA=l_alda_fxc
 BS_dip_size=1
 !
 ! Memory saving
 !===============
 l_BSE_minimize_memory=((Xk%nbz==1).or.index(PARALLEL_default_mode,"memory")>0)
 !
 if (l_BSE_minimize_memory) call warning(" BSE calculation with minimal memory utilization")
 !
 ! SOC
 !=====
 if (Ken%nb_SOC/=0) BS_perturbative_SOC=.TRUE.
 !
 ! Copatibility with old inputs
 !==============================
 if (STRING_match(BSE_mode,'causal')) BSE_mode='retarded'
 !
 ! BS-Fxc Setup
 !==============
 !
 ! Carriers and bs-fxc non compatible!
 !-------------------------------------
#if defined _RT 
 if(l_rt_carriers_in_use .and. STRING_match(BSS_mode,'t')) call error('RT occupations and BSE via f_xc not compatible')
#endif
 !
 ! If the em1s DB has been calculated in the same run
 ! of a BS-Fxc kernel construction that l_bs_fxc is set .FALSE.
 !      =============
 ! in TDDFT_do_X_W_typs. So here we need to check again if
 ! l_bs_fxc has been provided
 !
 if (.not.l_bs_fxc) call parser('bs_fxc',l_bs_fxc)
 !
 if (l_bs_fxc) then
   BS_n_g_exch=FXC_n_g_corr
   if (STRING_match(BSS_mode,'t')) then
     ! Remove the exchange if TDDFT Fxc is obtained from K
     BS_res_K_exchange=.FALSE.
     BS_cpl_K_exchange=.FALSE.
     BSE_mode='retarded'
     call parser('FxcRetarded',FXC_is_retarded)
   endif
 endif
 !
 ! Remove exchange if computing the irreducible response
 if (trim(BSE_L_kind)=="tilde") then
   BS_res_K_exchange=.FALSE.
   BS_cpl_K_exchange=.FALSE.
 endif
 !
 ! CORE Logicals
 !===============
 if (l_rpa_IP.and.STRING_match(BSE_mode,'coupling')) BSE_mode='retarded'
 !
 ! TR-ABS logicals
 !-----------------
#if defined _RT
 NEQ_Kernel   =STRING_match(RT_BSE_Occ_Mode,"k").and.allocated(Ken%fo)
 NEQ_Residuals=STRING_match(RT_BSE_Occ_Mode,"r").and.allocated(Ken%fo)
 call parser('ForceEqTrans',EQ_Transitions)
 call parser('ForceEqNoOcc',EQ_NoOcc)
#endif
 !
 ! SLEPC
 !=======
 ! Here we will have two options to initialize the slepc matrix:
 call parser('BSSSlepcMatrix',BSS_slepc_matrix)
 !
#if defined _SLEPC && !defined _NL
 !
 ! Special case, slepc with distributed matrix need to explicitly compute the anti-resonant block
 l_slepc_special = index(BSS_mode,'s')/=0 .and. BS_K_coupling .and. (.not.BSS_slepc_matrix)
 !
 ! The doubling is activated unless explicily imposed by input since slepc
 ! with shells requires the extended K_multiply_by_V for the non hermitian algorithm
 ! Slepc are not (yet) able to deal with a pseudo-hermitian algorithm 
 !
 call parser('ImposeSymm',l_flag)
 if (l_slepc_special.and..not.l_flag) l_BS_ares_from_res=.FALSE.
 BSS_slepc_double_grp=l_BS_ares_from_res.and.l_slepc_special
 !
#endif
 !
 ! Double Grid support
 !=====================
 BSS_uses_DbGd = (Ken%FG%nb>=BS_bands(2)).and.STRING_match(BSS_mode,'i')
 !
end subroutine
