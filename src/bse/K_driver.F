!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM IMA
!
subroutine K_driver(Xen,Ken,Xk,q,X_static_pp,Xw,Dip)
 !
 use pars,           ONLY:IP
 use com,            ONLY:msg
 use stderr,         ONLY:intc,STRING_match
 use electrons,      ONLY:levels,n_sp_pol
 use frequency,      ONLY:w_samp,W_reset
 use X_m,            ONLY:X_t,X_duplicate,X_reset,X_OUTPUT_driver
 use DIPOLES,        ONLY:DIPOLE_t
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,REP
 use BS_solvers,     ONLY:BSS_n_freqs,BSS_er,BSS_dr,BSS_mode,BSS_damp_reference,BSS_q0,BSS_uses_DbGd
 use BS,             ONLY:BS_bands,BS_q,BS_n_g_exch,BS_res_K_corr,BS_n_g_W,BS_cpl_K_corr,&
&                         BS_Blocks_and_Transitions_free,BSqpts,l_BS_photolum
 use TDDFT,          ONLY:FXC_per_memstps,FXC_n_mem_freqs,FXC_is_retarded
 use drivers,        ONLY:l_rpa_IP,l_bs_fxc,l_bss
 use parallel_m,     ONLY:PAR_IND_WF_b_and_k,PAR_K_scheme,PARALLEL_default_mode
 use parallel_int,   ONLY:PP_wait,PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use interfaces,     ONLY:eval_G_minus_G
 use R_lattice,      ONLY:bz_samp,qindx_B,qindx_X,qindx_B_max,qindx_free
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
 integer              :: io_err,ID
 integer, external    :: io_QINDX
 !
 call section('*','Bethe Salpeter Equation driver')
 !
 ! TDDFT setup and Resets
 !========================
 call X_reset(X_tddft)
 call TDDFT_do_X_W_typs(-1,X_tddft,Xw)
 !
 ! Logicals initialization
 !=========================
 call K_driver_init("init",1,Ken,Xk)
 !
 ! Energy points
 !===============
 call W_reset(W_bss)
 W_bss%n_freqs=BSS_n_freqs
 W_bss%er=BSS_er
 W_bss%dr=BSS_dr
 W_bss%damp_reference=BSS_damp_reference
 W_bss%per_memstps=FXC_per_memstps
 call FREQUENCIES_setup(W_bss)
 FXC_n_mem_freqs=W_bss%n_freqs
 !
 ! Observables initialization
 !============================
 call K_observables(W_bss)
 !
 ! Double Grid support
 !=====================
 BSS_uses_DbGd = ((Ken%FG%nb>=BS_bands(2)) .and. (index(BSS_mode,'i')/=0)) &
&           .or. ((Ken%FG%nb>=BS_bands(2)) .and. (index(BSS_mode,'h')/=0))
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
 if (any((/BS_res_K_corr,BS_cpl_K_corr/)).and..not.l_bs_fxc) then
   !
   ! Here I need the distribution over k
   if(     trim(PARALLEL_default_mode)=="KQmemory") call PARALLEL_SETUP_K_scheme("K")
   if(.not.trim(PARALLEL_default_mode)=="KQmemory") call PARALLEL_SETUP_K_scheme("Kdef")
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
   ! Output files & observables initialization
   !===========================================
   call X_OUTPUT_driver("DEF INIT") 
   !
   if (iq==1) BSqpts(:,iq) = BSS_q0
   if (iq/=1) BSqpts(:,iq) = q%pt(iq,:)
   !
   call K_driver_init("loop-init",iq,Ken,Xk)
   !
   if (iq==BS_q(1)) call section('+','Bethe Salpeter Equation @q'//trim(intc(iq)))
   if (iq> BS_q(1)) then
     call section('-P',' ')
     call section('=','Bethe Salpeter Equation @q'//trim(intc(iq)))
   endif
   !
   ! BSS output arrays
   !===================
   call X_ALLOC_global("ALLOC",W_bss)
   !
   if (any((/BS_res_K_corr,BS_cpl_K_corr/)).and..not.l_bs_fxc) then
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
   call K_Transitions_setup("K",iq,Ken,Xk,Dip)
   !
   !... Parallel distribution
   call PARALLEL_global_Response_T_transitions(Xk)
   !
   ! ... EH space (with parallelization) and Transition groups build-up
   call K_Transitions_setup("T",iq,Ken,Xk,Dip)
   !
   !... Blocks build-up
   if (.not.l_rpa_IP) call K_blocks()
   !
   ! Wave Functions distribution 
   !=============================
   call PARALLEL_WF_distribute(B_and_K_index=PAR_IND_WF_b_and_k,CLEAN_UP=.FALSE.)
   call PARALLEL_WF_index( )
   !
   ! 0. The Dipoles 
   ! ===============
   call K_dipoles(iq,Ken,Xk,Dip,W_bss)
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
     call PP_wait()
     X_tddft%iq=iq
     X_tddft%ng=BS_n_g_exch
     call OPTICS_driver(Xen,Xk,q,W_bss,X_tddft,Dip) 
     !
   endif
   !
   ! CLEAN (OBSs)
   call X_ALLOC_global("FREE",W_bss)
   !
   ! CLEAN (Blocks)
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
