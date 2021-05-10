!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine K_driver_init(what,iq,Ken,Xk)
 !
 use R_lattice,    ONLY:bz_samp
 use electrons,    ONLY:n_spin,levels,n_sp_pol
 use parser_m,     ONLY:parser
 use parallel_m,   ONLY:PARALLEL_default_mode
 use stderr,       ONLY:STRING_match
 use BS_solvers,   ONLY:BSS_eels_to_eps,BSS_mode,BSS_slepc_matrix,BSS_uses_DbGd,&
&                       BSS_slepc_double_grp
 use BS,           ONLY:L_kind,BSE_mode,BS_K_is_ALDA,BS_dip_size,l_BSE_minimize_memory,BS_perturbative_SOC,&
&                       l_BS_anomalous_Hall,BS_perturbative_SOC,l_BS_abs,l_BS_jdos,l_BS_trace,BSE_prop,&
&                       l_BS_esort,l_BS_kerr,l_BS_magnons,l_BS_dichroism,l_BS_anomalous_Hall,l_BS_photolum,l_BS_optics,&
&                       BS_cpl_K_exchange,BS_n_g_exch,BS_res_K_exchange,BS_K_coupling,BS_res_ares_n_mat,&
&                       BS_n_eh_spaces,l_BS_ares_from_res,BS_bands,BS_K_anti_resonant
 use TDDFT,        ONLY:FXC_is_retarded,FXC_n_g_corr
 use drivers,      ONLY:l_col_cut,l_rpa_IP,l_alda_fxc,l_bs_fxc
 use D_lattice,    ONLY:l_0D
#if defined _RT
 use drivers,      ONLY:l_rt_carriers_in_use
 use RT_control,   ONLY:NEQ_Kernel,EQ_Transitions,NEQ_Residuals,RT_BSE_Occ_Mode
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
 if (iq/=1 )        l_BS_ares_from_res=.FALSE.
 if (l_BS_photolum) l_BS_ares_from_res=.FALSE.
 if (l_BS_magnons.and.n_sp_pol==2) l_BS_ares_from_res=.FALSE.
 if (trim(BSE_mode)=="resonant")   l_BS_ares_from_res=.TRUE.
 !
 ! I need ARES and I cannot get it from RES
 ! 
 if (BS_K_anti_resonant.and..not.l_BS_ares_from_res) then
   BS_n_eh_spaces=2
   if (.not.BS_K_coupling) BS_res_ares_n_mat=2
 endif
 !
 if (what=="loop-init") return
 !
 ! Need to convert eels to epsilon ? (part A, part B in K_driver)
 !================================================================
 ! If the Coulomb cutoff is not used (ie the system is 3D) eps/=eps^-1
 if (.not.l_col_cut)          BSS_eels_to_eps=trim(L_kind)=="full"
 ! If the Coulomb cutoff is     used and the system is 0D  eps= eps^-1=1 since vbar=0
 if (     l_col_cut.and.l_0D) BSS_eels_to_eps=.false.
 ! If the Coulomb cutoff is     used and the system is 1D or 2D it depends on the q-point (see part B)
 !
 ! Memory saving
 !===============
 l_BSE_minimize_memory=((Xk%nbz==1).or.trim(PARALLEL_default_mode)=="memory")
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
 ! CORE Logicals
 !===============
 if (l_rpa_IP.and.STRING_match(BSE_mode,'coupling')) BSE_mode='retarded'
 !
 BS_K_is_ALDA=l_alda_fxc
 !
 BS_dip_size=1
 !
 ! What...
 !--------
 l_BS_abs       = STRING_match(BSE_prop,'abs')
 l_BS_jdos      = STRING_match(BSE_prop,'jdos').or.STRING_match(BSS_mode,'d').or.STRING_match(BSS_mode,'s').or.l_rpa_IP
 l_BS_dichroism = STRING_match(BSE_prop,'dich')
 l_BS_kerr      = STRING_match(BSE_prop,'kerr')
 l_BS_photolum  = STRING_match(BSE_prop,'photolum')
 l_BS_magnons   = STRING_match(BSE_prop,'magn') .and.n_spin>1
 !
 ! How ...
 !---------
 l_BS_esort     = STRING_match(BSE_prop,'esrt').and.(STRING_match(BSS_mode,'d').or.STRING_match(BSS_mode,'s').or.l_rpa_IP)
 !
 ! How to treat dipoles...
 !------------------------
 l_BS_trace     = STRING_match(BSE_prop,'trace')
 !
 if (l_BS_magnons.and.n_sp_pol==2) then
   l_BS_abs    = .false.
   l_BS_kerr   = .false.
   l_BS_trace  = .false.
   call warning(" n_sp_pol=2 and magnons requested. Spin flip BSE only is computed")
 endif
 if (l_BS_kerr) then
   call parser('AnHall',l_BS_anomalous_Hall)
   BS_dip_size=2
   l_BS_abs=.TRUE.
 endif
 if (l_BS_photolum) l_BS_trace  = .false.
 if (l_BS_trace   ) BS_dip_size = 3
 !
 l_BS_optics=l_BS_abs.or.l_BS_kerr.or.l_BS_dichroism.or.l_BS_photolum
 !
 ! TR-ABS logicals
 !-----------------
#if defined _RT
 NEQ_Kernel   =STRING_match(RT_BSE_Occ_Mode,"k").and.allocated(Ken%fo)
 NEQ_Residuals=STRING_match(RT_BSE_Occ_Mode,"r").and.allocated(Ken%fo)
 call parser('ForceEqTrans',EQ_Transitions)
#endif
 !
 ! SLEPC
 !=======
 ! Here we will have two options to initialize the slepc matrix:
 call parser('BSSSlepcMatrix',BSS_slepc_matrix)
 !
#if defined _SLEPC && !defined _SLEPC_OFF
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
