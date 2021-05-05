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
subroutine K_driver(Xen,Ken,Xk,q,X_static_pp,Xw,Dip)
 !
 use pars,           ONLY:IP
 use parser_m,       ONLY:parser
 use com,            ONLY:msg
 use stderr,         ONLY:intc,STRING_match
 use electrons,      ONLY:levels,n_sp_pol,n_spin
 use frequency,      ONLY:w_samp,W_reset
 use X_m,            ONLY:X_t,X_duplicate,X_reset,global_gauge
 use DIPOLES,        ONLY:DIPOLE_t
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,REP
 use BS_solvers,     ONLY:BSS_n_freqs,BSS_er,BSS_dr,BSS_mode,BSS_antires_from_resonant,BSS_damp_reference,&
&                         BSS_uses_DbGd,BSS_q0,BSS_eels_to_eps
 use BS,             ONLY:BS_bands,BS_q,BS_n_g_exch,BS_res_K_corr,BS_res_K_exchange,l_BS_anomalous_Hall,&
&                         BS_n_g_W,BS_K_is_ALDA,BS_cpl_K_corr,BS_cpl_K_exchange,&
&                         BSE_mode,BSE_prop,BS_Blocks_and_Transitions_free,l_BSE_minimize_memory,BSqpts,L_kind,&
&                         BS_perturbative_SOC,BS_dip_size,l_BS_abs,l_BS_jdos,l_BS_trace,&
&                         l_BS_esort,l_BS_kerr,l_BS_magnons,l_BS_dichroism,l_BS_anomalous_Hall,l_BS_photolum,l_BS_optics
 use TDDFT,          ONLY:FXC_per_memstps,FXC_n_mem_freqs,FXC_is_retarded,FXC_n_g_corr
 use drivers,        ONLY:l_rpa_IP,l_alda_fxc,l_bs_fxc,l_bss,l_col_cut
 use parallel_m,     ONLY:PAR_IND_WF_b_and_k,PARALLEL_default_mode,PAR_K_scheme
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use interfaces,     ONLY:eval_G_minus_G
 use R_lattice,      ONLY:bz_samp,qindx_B,qindx_X,qindx_B_max,qindx_free
 use D_lattice,      ONLY:l_0D,l_1D,l_2D
#if defined _RT
 use drivers,        ONLY:l_rt_carriers_in_use
 use RT_control,     ONLY:NEQ_Kernel,EQ_Transitions,NEQ_Residuals,RT_BSE_Occ_Mode
#endif
 !
#include<memory.h>
 !
 type(levels)  ::Xen,Ken
 type(bz_samp) ::Xk,q
 type(X_t)     ::X_static_pp
 type(DIPOLE_t)::Dip
 type(w_samp)  ::Xw
 !
 ! Work Space
 !
 type(X_t)         :: X_tddft,X_oscillators
 integer           :: iq,Max_G_m_G,iG_BSE,iGo_BSE
 type(w_samp)      :: W_bss 
 !
 ! I/O
 !
 integer, allocatable :: qindx_B_max_CPU(:)
 integer           :: io_err,ID
 integer, external :: io_QINDX
 !
#if defined _RT 
 if(l_rt_carriers_in_use .and. STRING_match(BSS_mode,'t')) call error('RT occupations and BSE via f_xc not compatible')
#endif
 !
 call section('*','Bethe Salpeter Equation driver')
 !
 ! Need to convert eels to epsilon ? (part A)
 !===========================================
 ! If the Coulomb cutoff is not used (ie the system is 3D) eps/=eps^-1
 if (.not.l_col_cut)          BSS_eels_to_eps=trim(L_kind)=="full"
 ! If the Coulomb cutoff is     used and the system is 0D  eps= eps^-1=1 since vbar=0
 if (     l_col_cut.and.l_0D) BSS_eels_to_eps=.false.
 ! If the Coulomb cutoff is     used and the system is 1D or 2D it depends on the q-point (see later)
 !
 ! Copatibility with old inputs
 !==============================
 if (STRING_match(BSE_mode,'causal')) BSE_mode='retarded'
 !
 l_BSE_minimize_memory=((Xk%nbz==1).or.trim(PARALLEL_default_mode)=="memory")
 if(l_BSE_minimize_memory) call warning(" BSE calculation with minimal memory utilization")
 !
 ! TDDFT setup and Resets
 !========================
 call X_reset(X_tddft)
 call W_reset(W_bss)
 !
 call TDDFT_do_X_W_typs(-1,X_tddft,Xw)
 !
 ! PL is implemented only in velocity gauge
 if (l_BS_photolum) global_gauge="velocity"
 !
 if(.not.any((/trim(global_gauge)=="velocity".or.trim(global_gauge)=="length"/)) ) then
   call warning(" Not a valid gauge in input. Switching to lenght gauge")
   global_gauge="length"
 endif
 !
 ! If the em1s DB has been calculated in the same run
 ! of a BS-Fxc kernel construction that l_bs_fxc is set .FALSE.
 !      =============
 ! in tddft_do_X_W_typs. So here we need to check again if
 ! l_bs_fxc has been provided
 !
 if (.not.l_bs_fxc) call parser('bs_fxc',l_bs_fxc)
 !
 if (Ken%nb_SOC/=0) BS_perturbative_SOC=.true.
 !
 if (l_bs_fxc) then
   !==========
   !
   BS_n_g_exch=FXC_n_g_corr
   !
   if (STRING_match(BSS_mode,'t')) then
     !
     ! Remove the exchange if TDDFT Fxc is obtained from K
     !
     BS_res_K_exchange=.FALSE.
     BS_cpl_K_exchange=.FALSE.
     !
     BSE_mode='retarded'
     call parser('FxcRetarded',FXC_is_retarded)
     !
   endif
   !
 endif
 !
 ! Logicals (Drivers)
 !====================
 if (l_rpa_IP.and.STRING_match(BSE_mode,'coupling')) BSE_mode='retarded'
 !
 BSS_antires_from_resonant=STRING_match(BSE_mode,'retarded')
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
 ! Energy points
 !===============
 W_bss%n_freqs=BSS_n_freqs
 W_bss%er=BSS_er
 W_bss%dr=BSS_dr
 W_bss%damp_reference=BSS_damp_reference
 !
 l_BS_trace     = STRING_match(BSE_prop,'trace')
 l_BS_magnons   = STRING_match(BSE_prop,'magn') .and.n_spin>1
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
 endif
 if (l_BS_photolum) l_BS_trace  = .false.
 if (l_BS_trace   ) BS_dip_size = 3
 !
 l_BS_optics=l_BS_abs.or.l_BS_kerr.or.l_BS_dichroism.or.l_BS_photolum
 !
 ! Energy points
 !===============
 W_bss%n_freqs=BSS_n_freqs
 W_bss%er=BSS_er
 W_bss%dr=BSS_dr
 W_bss%damp_reference=BSS_damp_reference
 !
 ! Double Grid support
 !=====================
 BSS_uses_DbGd = (Ken%FG%nb>=BS_bands(2)).and.STRING_match(BSS_mode,'i')
 !
 ! Frequencies 
 !=============
 W_bss%per_memstps=FXC_per_memstps
 call FREQUENCIES_setup(W_bss)
 FXC_n_mem_freqs=W_bss%n_freqs
 !
 ! PARALLEL SETUP
 !================
 call X_duplicate(X_static_pp,X_oscillators) 
 X_oscillators%whoami=X_static_pp%whoami
 X_oscillators%ib(1:2)=BS_bands(1:2)
 X_oscillators%q0=BSS_q0
 !
 if ((BS_res_K_corr.or.BS_cpl_K_corr).and.BS_n_g_W<0) then
   BS_n_g_W=X_static_pp%ng
   call msg('s','[BSE-W] Use all G-vectors of W ',BS_n_g_W)
 endif
 !
 call PARALLEL_global_indexes(Ken,Xk,q,"Response_T_space",X=X_oscillators)
 !
 ! Compute qindx_B_max
 ! =====================
 !
 if (any((/BS_res_K_corr,BS_cpl_K_corr/)).and..not.(l_bs_fxc.or.BS_K_is_ALDA)) then
   !
   ! Here I need the distribution over k
   call PARALLEL_SETUP_K_scheme("K")
   !
   ! here qindx_B is allocated
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/5/),ID=ID)
   io_err=io_QINDX(Xk,q,ID)
   !
   YAMBO_ALLOC(qindx_B_max_CPU,(PAR_K_scheme%COM_ibz_INDEX%n_CPU))
   qindx_B_max_CPU=0
   qindx_B_max_CPU(PAR_K_scheme%COM_ibz_INDEX%CPU_id+1)=maxval(qindx_B(:,:,2))
   call PP_redux_wait(qindx_B_max_CPU,COMM=PAR_K_scheme%COM_ibz_INDEX%COMM)
   qindx_B_max=maxval(qindx_B_max_CPU)
   YAMBO_FREE(qindx_B_max_CPU)
   !
 endif
 !
 ! Tranferred momentum
 !======================
 YAMBO_ALLOC(BSqpts,(3,BS_q(1):BS_q(2)))
 !
 do iq=BS_q(1),BS_q(2)
   !
   if (iq==1) BSqpts(:,iq) = BSS_q0
   if (iq/=1) BSqpts(:,iq) = q%pt(iq,:)
   !
   call section('*','Bethe Salpeter Equation @q'//trim(intc(iq)))
   !
   ! Need to convert eels to epsilon ? (part B)
   !===========================================
   ! If the Coulomb cutoff is used and the system is 1D or 2D eps and eels are different at finite q
   if (l_col_cut.and.(l_1D.or.l_2D)) then
     if(iq==1) BSS_eels_to_eps=.false.
     if(iq/=1) BSS_eels_to_eps=trim(L_kind)=="full"
   endif
   !
   ! BSS output arrays
   !===================
   call K_observables("ALLOC",W_bss)
   !
   if (any((/BS_res_K_corr,BS_cpl_K_corr/)).and..not.(l_bs_fxc.or.BS_K_is_ALDA)) then
     !
     ! W/X RL size setup looking at the max index of G-G' 
     ! =========
     !
     iGo_BSE=qindx_B_max
     iG_BSE =BS_n_g_W
     if (.not.iq==1) then
       iGo_BSE=maxval((/4*qindx_B_max,qindx_X(iq,:,2)/))
       iG_BSE =max(BS_n_g_exch,BS_n_g_W)
     endif
     !
     Max_G_m_G=eval_G_minus_G(iG_BSE,iGo_BSE)
     !
     if (.not.iq==1) then
       if (BS_n_g_exch> Max_G_m_G ) then
         call warning (' Exch size reduced from '//trim(intc(BS_n_g_exch))//' to '&
&                        //trim(intc(Max_G_m_G))//' due to symmetry constraints')
         BS_n_g_exch= Max_G_m_G
         call msg('s','[BSE-exch] Exch size forced by symmetry to be ',BS_n_g_exch)
       endif
     endif
     !
     if (BS_n_g_W> Max_G_m_G ) then
       call warning (' W size reduced from '//trim(intc(BS_n_g_W))//' to '&
&                    //trim(intc(Max_G_m_G))//' due to symmetry constraints')
       BS_n_g_W= Max_G_m_G
       call msg('s','[BSE-W] W size forced by symmetry to be ',BS_n_g_W)
     endif
     !
   endif
   !
   call DIPOLE_dimensions(Ken,Dip,BS_bands,BSS_q0)
   !
   ! ... EH space (no parallelization)
   call K_Transitions_k_resolved(iq,Ken,Xk,Dip)
   !
   !... Parallel distribution
   call PARALLEL_global_Response_T_transitions(Xk)
   !
   ! ... EH space (with parallelization) and Transition groups build-up
   call K_Transitions_setup(iq,Ken,Xk,Dip)
   !
   !... Blocks build-up
   if (.not.l_rpa_IP) call K_blocks(iq)
   !
   ! Wave Functions distribution 
   !=============================
   call PARALLEL_WF_distribute(B_and_K_index=PAR_IND_WF_b_and_k,CLEAN_UP=.FALSE.)
   call PARALLEL_WF_index( )
   !
   ! 1. The IP Part
   ! ===============
   call K_IP(iq,Ken,Xk,Dip,W_bss)
   !
   ! 2. The KERNEL
   ! ================
   if (.not.l_rpa_IP) call K(iq,Ken,Xk,q,X_static_pp,Xw,W_bss)
   !
   ! 3. The SOLVER
   ! =================
   if (l_bss.and..not.l_rpa_IP) call K_solvers(iq,Ken,Xk,q,X_static_pp,W_bss)
   !
   ! `TDDFT` SOLVER
   !
   if (STRING_match(BSS_mode,'t')) then
     !
     call X_duplicate(X_static_pp,X_tddft) ! initialize X_tddft to X_static_pp
     !
     X_tddft%ordering='Tr'
     if (FXC_is_retarded) X_tddft%ordering='R'
     !
     ! If the kernel contains QP corrections I have to equally correct Xen
     !
     if (allocated(Ken%Eo)) then
       YAMBO_ALLOC(Xen%Eo,(Xen%nb,Xen%nk,n_sp_pol))
       Xen%Eo=Xen%E
       Xen%E =Ken%E
     endif
     !
     call PP_redux_wait
     X_tddft%iq=iq
     X_tddft%ng=BS_n_g_exch
     call OPTICS_driver(Xen,Xk,q,W_bss,X_tddft,Dip) 
     !
   endif
   !
   ! CLEAN (OBSs)
   call K_observables("FREE",W_bss)
   !
   ! CLEAN (Blocks)
   !
   call BS_Blocks_and_Transitions_free( )
   !
 enddo
 !
 !... Parallel distribution reset
 call PARALLEL_global_indexes(Ken,Xk,q,"Response_T_space",X=X_oscillators,RESET=.TRUE.)
 !
 YAMBO_FREE(BSqpts)
 call qindx_free('B')
 !
end subroutine
