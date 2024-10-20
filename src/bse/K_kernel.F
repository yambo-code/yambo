!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS AF
!
subroutine K_kernel(iq,Ken,Xk,q,X,Xw,W_bss)
 !
 ! K = <2V-W> for n_spin=1; K=<V-W> for n_spin>1
 !
 use pars,          ONLY:SP,schlen,pi,cZERO,cI
 use LOGO,          ONLY:pickup_a_random
 use drivers,       ONLY:l_bs_fxc,l_tddft,l_rim_w
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin,n_spin,n_spinor,n_sp_pol
 use FFT_m,         ONLY:fft_size
 use interfaces,    ONLY:WF_load,WF_free,eval_G_minus_G,TDDFT_ALDA_eh_space_R_kernel
 use D_lattice,     ONLY:nsym,DL_vol,i_time_rev,i_space_inv,sop_inv,sop_tab
 use R_lattice,     ONLY:qindx_X,bz_samp,&
&                        qindx_B,qindx_B_max,qindx_B_load,RIM_W,RIM_W_d,&
&                        RIM_ng,RIM_W_is_diagonal,RIM_W_ng
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use LIVE_t,        ONLY:live_timing
 use X_m,           ONLY:X_t
 use interfaces,    ONLY:el_density_and_current
 use QP_m,          ONLY:QP_ng_Vxc
 use BS_solvers,    ONLY:BSS_kernel_last_stored_block
 use BS,            ONLY:BS_bands,BS_res_K_corr,BS_W,BS_W_is_diagonal,BS_ID,&
&                        BS_res_K_exchange,BS_Block_size,BS_n_g_W,&
&                        O_ng,BS_n_g_exch,BS_n_g_fxc,BS_identifier,BS_LiveTiming_steps,&
&                        BS_K_dim,BS_K_is_ALDA,BS_cpl_K_exchange,&
&                        BS_cpl_K_corr,K_EXCH_collision,K_CORR_collision,&
&                        WF_phase,n_BS_blks,BS_blk,BS_T_grp,BSK_IO_mode,BSK_IO_sum_value,&
&                        BS_nT_grps,l_BS_ares_from_res,BS_K_has_been_calculated_loaded,&
&                        l_BSE_minimize_memory,l_BSE_restart,l_BSE_kernel_complete,&
&                        BS_perturbative_SOC,BS_K_cutoff,BS_max_val,l_BS_magnons,&
&                        O1,O2,O_times_W
 use collision_el,  ONLY:elemental_collision_free
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:REP,OP_WR,RD,WR,WR_CL,OP_APP,io_BS_K,deliver_IO_error_message
 use TDDFT,         ONLY:FXC_K_diagonal,F_xc_gspace,FXC_n_g_corr,io_BS_Fxc, &
&                        FXC_mode,tddft_wf_t,l_Fxc_from_Vxc
 use xc_functionals,ONLY:V_xc,F_xc,F_xc_mat,magn,XC_potential_driver
 use global_XC,     ONLY:WF_xc_functional,WF_kind,WF_exx_fraction
 use wave_func,     ONLY:WF
 use openmp,        ONLY:OPENMP_update,master_thread
 use timing_m,      ONLY:timing
 use parallel_m,    ONLY:myid,ncpu,PAR_K_scheme,PARALLEL_default_mode,master_cpu
 use parallel_int,  ONLY:PP_redux_wait
 use MAGNONS,       ONLY:BSS_MAGN_free
 use PHOTOLUM,      ONLY:BSS_PL_free
 use parser_m,      ONLY:parser
 !
#include<dev_defs.h>
#include<y_memory.h>
 !
 type(levels)  ::Ken 
 type(bz_samp) ::Xk,q
 type(X_t)     ::X
 type(w_samp)  ::Xw,W_bss
 integer       ::iq
 !
 ! Work Space ...
 !
 type(tddft_wf_t), target  :: tddft_wf
 character(schlen) :: section_title
 character(1)      :: mode_now
 !
 ! ... Kernel loop
 !
 ! DEFINE BSK COMMON INDEXES
 !
 integer ::     i_k_s,i_k_s_m1,i_p_s,i_k_bz,i_p_bz,i_k,i_p,i_kp_s,& 
&               i_kmq_s,i_kmq_s_m1,i_pmq_s,i_kmq_bz,i_pmq_bz,i_kmq,i_pmq,i_kp_mq_s,& 
&               i_Tk,i_Tp,i_Tgrp_k,i_Tgrp_p,I_Tgrp_k_st,I_Tgrp_p_st,H_pos(2),& 
&               i_v_k,i_v_p,i_c_k,i_c_p,i_k_sp_pol_c,i_p_sp_pol_c,i_k_sp_pol_v,i_p_sp_pol_v,iq_W,iq_W_bz,iq_W_s,ig_W,& 
&               i_kmq_t,i_pmq_t,i_k_bz_mem,i_kmq_bz_mem,qindx_tmp(2),ig_W_final
 ! OMP LAST DEF
 integer ::     i_k_bz_last,i_p_bz_last,i_pmq_bz_last,i_kmq_bz_last, & 
&               iq_W_bz_last,ig_W_last,iq_W_bz_mq_last,ig_W_mq_last
 !
 integer    :: i_block,O_ng_shift,O_ng_exch,O_ng_tddft,iHxc,NG(2),NK(2),&
&              ig_kmq,ig_pmq,ig_W_mq,iq_W_bz_mq,iq_W_mq,iq_W_s_mq,&
&              bands_to_load(2),is_k(4),os_k(4),is_p(4),os_p(4)
 complex(SP):: Co,H_x,H_c,BS_mat_tmp
 complex(SP):: sqrt_f_itk,sqrt_f_itp
 real(SP), allocatable :: BS_max_val_CPU(:),rho(:)
 !
 complex(SP),   external ::K_correlation_kernel_std,K_correlation_kernel_dir
 complex(SP),   external ::TDDFT_ALDA_eh_space_G_kernel
 complex(SP),   external ::K_exchange_kernel_resonant,K_exchange_kernel_coupling
 !
 logical    :: l_bs_exch,l_bs_corr,l_bs_exch_wf_in_loop,l_bs_tddft_wf_in_loop,l_matrix_init,BS_blk_todo,&
 &             l_load_kernel,l_write_kernel,l_write_kernel_step,l_skip_phases,l_std_alg,l_dir_alg,l_tddft_gsum,l_tddft_rsum
 !
 ! I/O
 !
 integer           ::io_X_err,io_BS_Fxc_err,io_BS_err,qindx_ID,qindx_ID_frag,ID,ID_head,ID_compr,io_QINDX_err
 integer, external ::io_BS,io_BS_header
 integer, external ::qindx_B_init,qindx_B_close
 integer, external ::io_BS_PAR_init
 !
 ! Timing and restart
 !
 logical :: BS_blk_done(n_BS_blks),l_partial_kernel_loaded
 integer :: n_steps,m_steps,i_steps
 !
 ! Constants & Setup
 !==================
 !
 if(.not.BS_perturbative_SOC) Co=4._SP*real(spin_occ)*pi/DL_vol/real(q%nbz,SP)
 if(     BS_perturbative_SOC) Co=4._SP               *pi/DL_vol/real(q%nbz,SP)
 io_BS_err    =-1
 io_BS_Fxc_err=-1
 !
 call elemental_collision_free(K_CORR_collision)
 call elemental_collision_free(K_EXCH_collision)
 !
 ! K/S Table
 !
 call k_build_up_BZ_tables(Xk)
 !
 ! Titles
 !========
 !
 if (l_bs_fxc) then
   section_title="BSE Kernel -> xc-kernel"
 else
   if (BS_K_is_ALDA) then
     section_title="TDDFT Kernel"
   else
     section_title="BSE Kernel"
   endif
   section_title=trim(section_title)//" @q"//trim(intc(iq))
   !
   if (BS_res_K_corr.or.BS_res_K_exchange)     section_title=trim(section_title)//" (Resonant"
   if (BS_K_is_ALDA     )                      section_title=trim(section_title)//" ALDA"
   if (BS_res_K_corr    )                      section_title=trim(section_title)//" CORRRELATION"
   if (BS_res_K_exchange)                      section_title=trim(section_title)//" EXCHANGE"
   if (BS_res_K_corr.or.BS_res_K_exchange)     section_title=trim(section_title)//")"
   !
   if (BS_cpl_K_corr.or.BS_cpl_K_exchange)     section_title=trim(section_title)//" (Coupling"
   if (BS_K_is_ALDA.and.BS_cpl_K_exchange)     section_title=trim(section_title)//" ALDA"
   if (BS_cpl_K_corr    )                      section_title=trim(section_title)//" CORRRELATION"
   if (BS_cpl_K_exchange)                      section_title=trim(section_title)//" EXCHANGE"
   if (BS_cpl_K_corr.or.BS_cpl_K_exchange)     section_title=trim(section_title)//")"
   !
 endif
 !
 call section('=',trim(section_title))
 !
 l_bs_exch=(any((/BS_res_K_exchange,BS_cpl_K_exchange/)).or.l_bs_fxc).and..not.(l_BS_magnons.and.n_sp_pol==2)
 l_bs_corr=(any((/BS_res_K_corr,BS_cpl_K_corr/)) .and. .not.l_bs_fxc)
 !
 ! Oscillators Setup
 !===================
 !
 if (l_bs_exch) then
   O_ng_exch =maxval(qindx_X(iq,:,2))
   ! I load the WFs for exachange in a loop in two cases
   ! - q=0 and more than one k-point.
   !   In this case it is the default, unless I specify workload in the parallel mode
   ! - q/=0
   !   In this case it is activated only if I specify KQmemory in the parallel mode
   !   The code would work fine also in this case, however it would require continuous loading and unloading of WFs.
   !   To avoid this one would need to move the loop over i_star in K_Transitions_setup outside the
   !   loop over ic and iv (see related comment in K_Transiontion_setup.F) 
   l_bs_exch_wf_in_loop= ((iq==1) .and. (Xk%nbz>=1).and..not.trim(PARALLEL_default_mode)=="workload") .or. &
   &                     ((iq> 1) .and.                      trim(PARALLEL_default_mode)=="KQmemory")
   call msg('r','[BSE] Exchange components ',BS_n_g_exch)
 endif
 !
 if (BS_K_is_ALDA) then
   O_ng_tddft =maxval(qindx_X(iq,:,2))
   l_tddft_gsum=(index(FXC_mode,"G-")>0)
   l_tddft_rsum=(index(FXC_mode,"R-")>0)
   if( .not.(l_tddft_gsum.or.l_tddft_rsum) ) call error("Wrong FXC_mode:"//trim(FXC_mode))
   if(index(FXC_mode,"cut_GmGp")>0) then
     FXC_n_g_corr=BS_n_g_fxc
     if (BS_n_g_fxc/= eval_G_minus_G(BS_n_g_fxc,0) ) then
       BS_n_g_fxc   = eval_G_minus_G(BS_n_g_fxc,0)
       call msg('s','[TDDFT] Fxc cutoff is forced by symmetry to be ',BS_n_g_fxc)
     endif
   endif
   l_bs_tddft_wf_in_loop= ((iq==1) .and. (Xk%nbz>=1).and.(.not.trim(PARALLEL_default_mode)=="workload") ) .or. &
   &                      ((iq> 1) .and.                       trim(PARALLEL_default_mode)=="KQmemory" )
   call msg('r','[BSE] ALDA        components ',BS_n_g_fxc)
 endif
 !
 if (l_bs_corr) then
   O_ng_shift=qindx_B_max
   if (iq/=1 .and. n_spinor==2 .and. nsym>1) then
     ! Default is direct algorithm, unless imposed from input
     call parser('ImposeStdAlg',l_std_alg)
     l_dir_alg=.not.l_std_alg
   else
     ! Default is standard algorithm, unless imposed from input
     call parser('ImposeDirAlg',l_dir_alg)
     l_std_alg=.not.l_dir_alg
   endif
   call parser('SkipPhases',l_skip_phases)
   call msg('r','[BSE] Correlation components ',(/BS_n_g_W,O_ng/))
 endif
 !
 call K_restart(iq,X,BS_ID(1),BS_ID(2),BS_ID(3),l_partial_kernel_loaded)
 ID_head =BS_ID(1)
 ID      =BS_ID(2)
 ID_compr=BS_ID(3)
 !
 if( l_BSE_kernel_complete ) return
 !
 io_QINDX_err=0
 if (l_bs_corr) io_QINDX_err=qindx_B_init(qindx_ID,qindx_ID_frag)
 if(io_QINDX_err/=0) call error('Error reading qindx_B database ')
 !
 ! Screened interaction
 !======================
 !
 !Check if RIM_W is diagonal
 call parser('RIM_W_diag',RIM_W_is_diagonal)
 if(l_rim_w .and. RIM_W_ng == 0) RIM_W_ng = RIM_ng
 if (l_rim_w) call QP_interpolate_W(X,Xw,q,'BSE')
 !
 io_X_err=0
 if (l_bs_corr) call K_screened_interaction(X,Xw,q,io_X_err)
 !
 if (io_X_err<0) then
   call deliver_IO_error_message(io_X_err,'PP/Em1s')
   call warning('Bethe Salpter section skipped. Impossible to build the kernel.')
   call warning('Error reading the PP/Em1s database.')
   n_BS_blks=0
   call BS_blks_free(1)
   call BSS_MAGN_free( )
   call BSS_PL_free( )
   !
   YAMBO_FREE_GPU(BS_W)
   YAMBO_FREE(BS_W)
   !
   YAMBO_FREE_GPU(O1)
   YAMBO_FREE_GPU(O2)
   YAMBO_FREE_GPU(O_times_W)
   YAMBO_FREE(O1)
   YAMBO_FREE(O2)
   YAMBO_FREE(O_times_W)
   !
   YAMBO_FREE_GPU(DEV_VAR(RIM_W))
   YAMBO_FREE(RIM_W)
   !
   return
   !
 endif
 !
 call timing('X T_space Kernel',OPR='start')
 !
 ! TDDFT xc-kernel Setup
 !======================
 if (l_bs_fxc) then
   !  
   !if (iq==1) here put dipoles I/O
   if (iq==1) stop
   !
   ! Allocation
   !-----------
   YAMBO_ALLOC(FXC_K_diagonal,(BS_K_dim(1)))
   YAMBO_ALLOC(F_xc_gspace,(FXC_n_g_corr,FXC_n_g_corr,W_bss%n_freqs))
   FXC_K_diagonal=0._SP
   F_xc_gspace=cZERO
   !
 endif
 !
 ! DB identifier 
 !===============
 BS_identifier=pickup_a_random(10000._SP)
 !
 ! BS DB description section I/O
 !===============================
 !
 call section('=','Main kernel loop')
 !
 if(BS_K_cutoff>0._SP) then
   YAMBO_ALLOC(BS_max_val_CPU,(ncpu))
   BS_max_val_CPU=0._SP
 endif
 !
#if defined _PAR_IO
 !
 call io_control(ACTION=OP_APP,COM=REP,ID=ID_head)
 io_BS_err=io_BS_header(iq,X,ID_head,"full")
 !
 call io_control(ACTION=OP_APP,COM=REP,ID=ID,DO_IT=.TRUE.)
 io_BS_err=io_BS_PAR_init(iq,ID,"full")
 !
#else
 call io_control(ACTION=OP_WR,COM=REP,SEC=(/1/),ID=ID)
 io_BS_err=io_BS(iq,X,ID)
#endif
 !
 ! This loop is on the exchange and correlation of the BSE
 ! iHxc=1 --> K_H=v
 ! iHxc=2 --> K_TDDFT=fxc
 ! iHxc=3 --> K_c=W
 !
 l_matrix_init=.true.
 l_load_kernel=.true.
 l_write_kernel=.false.
 l_write_kernel_step= index(BSK_IO_mode,"write_all_steps")/=0
 !
 if (l_write_kernel_step) call warning("Experimental mode, IO at every step. Restart not possible")
 !
 BS_blk_done=.false.
 !
 do iHxc=1,3
   !
 if ((.not.l_bs_exch).and.iHxc==1) cycle
 !
 if ((.not.BS_K_is_ALDA).and.iHxc==2) cycle
 !
 if ((.not.l_bs_corr).and.iHxc==3) cycle
 !
 section_title='-BSK-'//trim(intc(iHxc))
 if (l_bs_fxc) section_title='-BSK->Fxc-'//trim(intc(iHxc))
 !
 if (iHxc==1) NG=(/BS_n_g_exch,O_ng_exch/)
 if (iHxc==2) NG=(/BS_n_g_fxc ,O_ng_tddft/)
 if (iHxc==3) NG=(/O_ng       ,O_ng_shift/)
 !
 ! ALDA
 !======
 if (iHxc==2) then
   !
   call WF_load(WF,NG(1),NG(2),(/1,Ken%nbm/),(/1,Xk%nibz/),space='R',title="-TDDFT",keep_states_to_load=.true.)
   !
   QP_ng_Vxc=BS_n_g_fxc
   !
   if (l_tddft_rsum) then
     YAMBO_ALLOC(tddft_wf%rhotwr1,(fft_size*n_spinor*n_spinor))
     YAMBO_ALLOC(tddft_wf%rhotwr2,(fft_size*n_spinor*n_spinor))
   else if (l_tddft_gsum) then
     YAMBO_ALLOC(tddft_wf%rhotwr_DP,(fft_size))
   endif
   !
   YAMBO_ALLOC(tddft_wf%WF_symm1,(fft_size,n_spinor))
   YAMBO_ALLOC(tddft_wf%WF_symm2,(fft_size,n_spinor))
   !
   YAMBO_ALLOC(F_xc,(fft_size,n_spin,n_spin))
   if(l_BS_magnons .or. n_spinor==2 .or. l_Fxc_from_Vxc) then
     YAMBO_ALLOC(V_xc,(fft_size,n_spin))
     V_xc=0._SP
   endif
   !
   if(n_spin>1) then
     YAMBO_ALLOC(magn,(fft_size,3))
   endif
   call XC_potential_driver(Ken,Xk,WF_kind,WF_xc_functional,2)
   YAMBO_ALLOC(F_xc_mat,(fft_size,n_spin,n_spin,n_spin,n_spin))
   call Build_F_xc_mat(V_xc,F_xc,F_xc_mat)
   YAMBO_FREE(F_xc)
   if(l_BS_magnons .or. n_spinor==2 .or. l_Fxc_from_Vxc) then
     YAMBO_FREE(V_xc)
   endif
   if(n_spin>1) then
     YAMBO_FREE(magn)
   endif
   !
   call TDDFT_ALDA_lower_cutoff(FXC_mode)
   !
   call WF_free(WF,keep_states_to_load=.true.)
   !
 endif
 !
 ! Timing
 !========
 !
 if (BS_LiveTiming_steps>= 0) n_steps=BS_LiveTiming_steps
 if (BS_LiveTiming_steps==-1) n_steps=n_BS_blks
 !
 m_steps=n_steps
 !
 ! First load previously computed matrix elements.
 !=================================================
 !
#ifdef _PAR_IO
 if (l_BSE_restart.and.l_load_kernel) then
   !
   if (m_steps>0) call live_timing("Loading partial kernel",m_steps)
   !
   do i_block=1,n_BS_blks
     !
     if (.not. allocated(BS_blk(i_block)%mat) ) call K_block_alloc(i_block,"all")
     !
     ! Read BS_blk to check if the transition was already computed
     !
     if (.not.l_partial_kernel_loaded) then
       call io_control(ACTION=RD,ID=ID)
       call io_BS_PAR_block(iq,i_block,ID,"full")
     endif
     !
     if (BS_LiveTiming_steps>= 0) i_steps=BS_Block_size(i_block)
     if (BS_LiveTiming_steps==-1) i_steps=1
     !
     i_Tgrp_k=BS_blk(i_block)%iT_k
     i_Tgrp_p=BS_blk(i_block)%iT_p
     !
     ! check if this block need to be done
     ! skip this block if already done
     !
     BS_blk_done(i_block)=all((/BS_blk(i_block)%done=="t"/))
     BS_blk_todo         =all((/BS_blk(i_block)%done=="f"/))
     !
     if (BS_blk_done(i_block)) m_steps=m_steps-i_steps
     !
     if ( (.not.l_partial_kernel_loaded) .and. io_BS_K .and. l_write_kernel_step) then
       !
       if (BS_blk_done(i_block) .or. BS_blk_todo) call K_block_free(i_block,"all")
       !
     endif
     !
     call live_timing(steps=i_steps)
     !
   enddo
   !
   if (m_steps>0) call live_timing()
   !
   call msg("s","Kernel loaded percentual ",real(n_steps-m_steps,SP)/real(n_steps,SP)*100._SP,'[%]')
   !
   l_load_kernel=.false.
   !
 endif
#endif
 !
 if ( (iHxc==1 .and. l_bs_exch_wf_in_loop) .or. (iHxc==2.and.l_bs_tddft_wf_in_loop.and.l_tddft_gsum)) then
   ! DS this is used only for iq==1
   call fft_setup(NG(1),NG(2),.true.)
 else
   !
   call WF_load(WF,NG(1),NG(2),BS_bands,(/1,Xk%nibz/),space='R',&
   &            title=trim(section_title),keep_states_to_load=.true.)
   !
   ! Wave Functions Phases
   !=======================
   if (iHxc==3.and.l_std_alg) then
     call K_WF_phases(Xk)
     if (l_skip_phases) then
       WF_phase=-99._SP
       call msg('r','[BSE] WF phases ignored as imposed in input')
     endif
   endif
   !
 endif
 !
 if (.not.l_bs_fxc) section_title='Kernel'
 if (     l_bs_fxc) section_title='Kernel->Fxc'
 !
 if (iHxc==1) section_title=trim(section_title)//' exch'
 if (iHxc==2) section_title=trim(section_title)//' tddft'
 if (iHxc==3) section_title=trim(section_title)//' corr'
 !
 if (m_steps>0) call live_timing(trim(section_title),m_steps)
 !
 !------------------
 ! LOOP over blocks
 !------------------
 !
 block_loop:&
 do i_block=1,n_BS_blks
   !
   mode_now=BS_blk(i_block)%mode
   if (.not.l_BS_ares_from_res) mode_now="F"
   !
   if (iHxc==1) l_write_kernel=.not.(BS_K_is_ALDA.or.l_bs_corr)
   if (iHxc==2) l_write_kernel=.not. l_bs_corr
   if (iHxc==3) l_write_kernel=.true.
   !
   if (BS_blk_done(i_block)) then
#if defined _PAR_IO
     if(l_partial_kernel_loaded.and.l_write_kernel) then
       call io_control(ACTION=WR,ID=ID)
       call io_BS_PAR_block(iq,i_block,ID,"full")
       if (io_BS_K.and.l_write_kernel_step) call K_block_free(i_block,"all")
     endif
#endif
     cycle
   endif
   !
   if (l_write_kernel_step) then
     if (iHxc==1) BSK_IO_sum_value=.false.
     if (iHxc==2) BSK_IO_sum_value=l_bs_exch
     if (iHxc==3) BSK_IO_sum_value=l_bs_exch.or.BS_K_is_ALDA
   endif
   !
   if (.not. allocated(BS_blk(i_block)%mat) ) call K_block_alloc(i_block,"all")
   !
   ! Read BS_blk to check if the transition was already computed
   !
   if (BS_LiveTiming_steps>= 0) i_steps=BS_Block_size(i_block)
   if (BS_LiveTiming_steps==-1) i_steps=1
   !
   ! Nothing to do, just write the kernel to disk
   if (trim(BS_blk(i_block)%mode)=="C" .and. iHxc==3 .and. .not.BS_cpl_K_corr) then
     BS_blk(i_block)%done(:,:)="t"
     if(BS_K_cutoff>0._SP) BS_max_val_CPU(myid+1)=maxval( (/BS_max_val_CPU(myid+1),abs(BS_blk(i_block)%mat(:,:))/) )
     goto 100
   endif
   !
   i_Tgrp_k   =BS_blk(i_block)%iT_k
   i_Tgrp_p   =BS_blk(i_block)%iT_p
   i_Tgrp_k_st=BS_blk(i_block)%iT_k_st
   i_Tgrp_p_st=BS_blk(i_block)%iT_p_st
   i_k        =BS_blk(i_block)%ik
   i_p        =BS_blk(i_block)%ip
   !
   ! Exchange oscillators
   !----------------------
   if(iHxc==1) then
     if (.not.allocated( BS_T_grp(i_Tgrp_k)%O_x) ) then
       call BS_exchange_oscillators_alloc(i_Tgrp_k)
       call K_exchange_collisions(iq,Xk,i_Tgrp_k,NG,l_bs_exch_wf_in_loop)
     endif
     if (.not.allocated( BS_T_grp(i_Tgrp_p)%O_x) ) then
       call BS_exchange_oscillators_alloc(i_Tgrp_p)
       call K_exchange_collisions(iq,Xk,i_Tgrp_p,NG,l_bs_exch_wf_in_loop)
     endif
   endif
   !
   ! TDDFT oscillators
   !----------------------
   if(iHxc==2 .and. l_tddft_gsum) then
     if (.not.allocated( BS_T_grp(i_Tgrp_k)%O_tddft_L) ) then
       call TDDFT_oscillators_alloc_L(i_Tgrp_k)
       call TDDFT_ALDA_eh_space_G_collisions_L(iq,Xk,i_Tgrp_k,NG,l_bs_tddft_wf_in_loop,tddft_wf,mode_now)
     endif
     if (.not.allocated( BS_T_grp(i_Tgrp_p)%O_tddft_R) ) then
       call TDDFT_oscillators_alloc_R(i_Tgrp_p)
       call TDDFT_ALDA_eh_space_G_collisions_R(iq,Xk,i_Tgrp_p,NG,l_bs_tddft_wf_in_loop,tddft_wf,mode_now)
     endif
   endif
   !
   ! Correlation oscillators
   !----------------------
   if(iHxc==3) then
     if (l_std_alg) call K_correlation_collisions_std(iq,i_block,qindx_ID_frag,Xk,q)
     if (l_dir_alg) call K_correlation_collisions_dir(iq,i_block,qindx_ID_frag,Xk,q)
   endif
   !
   call timing('X T_space Kernel sum',OPR='start')
   !
   ! Transition Loops
   !------------------
   !
#if defined _OPENMP
   !DEV_OMP parallel default(shared), &
   !DEV_OMP private(i_Tk,i_Tp, &
   !DEV_OMP &       sqrt_f_itk,i_k_bz,i_v_k,i_c_k,i_k_sp_pol_c,i_k_sp_pol_v,i_k_s,is_k,os_k,i_k_s_m1,H_pos, &
   !DEV_OMP &       sqrt_f_itp,i_p_bz,i_v_p,i_c_p,i_p_sp_pol_c,i_p_sp_pol_v,i_p_s,is_p,os_p,i_kp_s, &
   !DEV_OMP &       i_kmq_bz,ig_kmq,i_kmq,i_kmq_s,i_kmq_t,i_kmq_s_m1,i_kp_mq_s, &
   !DEV_OMP &       i_pmq_bz,ig_pmq,i_pmq,i_pmq_s,i_pmq_t,ig_W_final, &
   !DEV_OMP &       ig_W,iq_W,iq_W_s,iq_W_bz,iq_W_bz_mq,ig_W_mq,iq_W_mq,iq_W_s_mq,H_x,H_c, &
   !DEV_OMP &       i_p_bz_last,i_k_bz_last,i_pmq_bz_last,i_kmq_bz_last, &
   !DEV_OMP &       iq_W_bz_last,ig_W_last,iq_W_bz_mq_last,ig_W_mq_last, &
   !DEV_OMP &       i_k_bz_mem,i_kmq_bz_mem,qindx_tmp,BS_mat_tmp,tddft_wf )
#endif
   !
   call OPENMP_update(master_thread)   
   !
   i_p_bz_last=0   ; i_k_bz_last=0
   i_pmq_bz_last=0 ; i_kmq_bz_last=0
   !
#if defined _OPENMP
   !DEV_OMP do collapse(2), schedule(dynamic)
#endif
   do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
     do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
       !
       ! skip this block if already done
       !
       if (BS_blk(i_block)%done(i_Tk,i_Tp)=="t".and.l_BSE_restart) cycle
       !
       H_pos(1) = sum(BS_T_grp(I_Tgrp_k_st:I_Tgrp_k-1)%size)+i_Tk+& 
       &              (BS_T_grp(I_Tgrp_k)%i_res_ares-1)*BS_K_dim(1) 
       H_pos(2) = sum(BS_T_grp(I_Tgrp_p_st:I_Tgrp_p-1)%size)+i_Tp+& 
       &              (BS_T_grp(I_Tgrp_p)%i_res_ares-1)*BS_K_dim(1)
       !
       if (H_pos(1)>H_pos(2)) cycle
       !
       i_k_bz  = BS_T_grp(i_Tgrp_k)%table(i_Tk,1) 
       i_p_bz  = BS_T_grp(i_Tgrp_p)%table(i_Tp,1) 
       !
       i_k_s =Xk%sstar(i_k_bz,2) 
       i_p_s =Xk%sstar(i_p_bz,2) 
       !
       i_k_s_m1= sop_inv(i_k_s) 
       i_kp_s  = sop_tab(i_k_s_m1,i_p_s) 
       !
       i_kmq_bz=qindx_X(iq,i_k_bz,1) 
       i_pmq_bz=qindx_X(iq,i_p_bz,1) 
       !
       i_kmq   =Xk%sstar(i_kmq_bz,1) 
       i_pmq   =Xk%sstar(i_pmq_bz,1) 
       !
       i_kmq_s =Xk%sstar(i_kmq_bz,2) 
       i_pmq_s =Xk%sstar(i_pmq_bz,2) 
       !
       if (iHxc==3) then 
         i_kmq_t=BS_blk(i_block)%kp_table(1,i_kmq) 
         i_pmq_t=BS_blk(i_block)%kp_table(2,i_pmq) 
       endif 
       !
       i_kmq_s_m1 = sop_inv(i_kmq_s) 
       i_kp_mq_s  = sop_tab(i_kmq_s_m1,i_pmq_s) 
       !
       if (iHxc==3) then 
         i_k_bz_mem=PAR_K_scheme%bz_index(i_k_bz) 
         if (i_p_bz_last/=i_p_bz.or.i_k_bz_last/=i_k_bz) then 
           i_p_bz_last=i_p_bz 
           i_k_bz_last=i_k_bz 
           if (i_k_bz_mem==0) then 
             !DEV_OMP critical 
             qindx_tmp=qindx_B_load(i_p_bz,i_k_bz,qindx_ID_frag) 
             iq_W_bz=qindx_tmp(1) 
             ig_W   =qindx_tmp(2) 
             !DEV_OMP end critical 
           else 
             iq_W_bz=qindx_B(i_p_bz,i_k_bz_mem,1) 
             ig_W   =qindx_B(i_p_bz,i_k_bz_mem,2) 
           endif 
           iq_W_bz_last=iq_W_bz 
           ig_W_last   =ig_W    
         else 
           iq_W_bz=iq_W_bz_last 
           ig_W   =ig_W_last    
         endif 
         iq_W   =q%sstar( iq_W_bz ,1) 
         iq_W_s =q%sstar( iq_W_bz ,2) 
       endif 
       !
       i_v_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,2) 
       i_c_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,3) 
       !
       i_v_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,2) 
       i_c_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,3) 
       !
       i_k_sp_pol_c=BS_T_grp(i_Tgrp_k)%table(i_Tk,4) 
       i_p_sp_pol_c=BS_T_grp(i_Tgrp_p)%table(i_Tp,4) 
       i_k_sp_pol_v=BS_T_grp(i_Tgrp_k)%table(i_Tk,5) 
       i_p_sp_pol_v=BS_T_grp(i_Tgrp_p)%table(i_Tp,5) 
       !
       ig_kmq    = qindx_X(iq,i_k_bz,2) 
       ig_pmq    = qindx_X(iq,i_p_bz,2) 
       !        
       if (iHxc==3) then 
         i_kmq_bz_mem=PAR_K_scheme%bz_index(i_kmq_bz) 
         if (i_pmq_bz_last/=i_pmq_bz.or.i_kmq_bz_last/=i_kmq_bz) then 
           i_pmq_bz_last=i_pmq_bz 
           i_kmq_bz_last=i_kmq_bz 
           if (i_kmq_bz_mem==0) then 
             !DEV_OMP critical 
             qindx_tmp=qindx_B_load(i_pmq_bz,i_kmq_bz,qindx_ID_frag) 
             iq_W_bz_mq=qindx_tmp(1) 
             ig_W_mq   =qindx_tmp(2) 
             !DEV_OMP end critical 
           else 
             iq_W_bz_mq=qindx_B(i_pmq_bz,i_kmq_bz_mem,1) 
             ig_W_mq   =qindx_B(i_pmq_bz,i_kmq_bz_mem,2) 
           endif 
           iq_W_bz_mq_last=iq_W_bz_mq 
           ig_W_mq_last   =ig_W_mq    
         else 
           iq_W_bz_mq=iq_W_bz_mq_last 
           ig_W_mq   =ig_W_mq_last    
         endif
         !
         iq_W_mq   =q%sstar( iq_W_bz_mq,1) 
         iq_W_s_mq =q%sstar( iq_W_bz_mq,2) 
         !
         !if (iq==1) ig_W_final=      ig_W_mq
         !if (iq> 1) ig_W_final=G_m_G(ig_W_mq,g_rot(G_m_G(ig_pmq,ig_kmq),iq_W_s_mq))
         !
       endif 
       ! 
       if (iHxc==2.and.l_tddft_rsum) then 
         is_k = (/i_c_k,i_k,i_k_s,i_k_sp_pol_c/) 
         os_k = (/i_v_k,i_kmq,i_kmq_s,i_k_sp_pol_v/) 
         is_p = (/i_c_p,i_p,i_p_s,i_p_sp_pol_c/) 
         os_p = (/i_v_p,i_pmq,i_pmq_s,i_p_sp_pol_v/) 
       endif 
       !
       if (iHxc==3) then 
         !
         if (iq_W_bz/=iq_W_bz_mq.or.iq_W/=iq_W_mq.or.iq_W_s/=iq_W_s_mq) call error("Wrong transferred momentum")
         !
         !if ( G_m_G(ig_W,ig_W_mq) /= G_m_G(ig_kmq,ig_pmq) )             call error("Wrong gvector shifts")
         !
         if ( (.not.BS_W_is_diagonal) .and. iq_W_s>nsym/(i_time_rev+1) .and. i_space_inv == 0 ) iq_W=q%nibz+iq_W
         !
       endif
       !
       H_x=cZERO
       H_c=cZERO
       !
       select case (mode_now)
       !
       case ("R","A","F")
         !
         ! :::RESONANT/ANTI-RESONANT TERM:::
         !===================================
         !
         ! Exchange
         if (iHxc==1) H_x=K_exchange_kernel_resonant(iq, BS_n_g_exch,               &
&                                   BS_T_grp(i_Tgrp_p),i_Tp, BS_T_grp(i_Tgrp_k),i_Tk)
         !
         ! ALDA
         if (iHxc==2) then
           if(l_tddft_rsum)     H_x=H_x+TDDFT_ALDA_eh_space_R_kernel(is_k,os_k,is_p,os_p,tddft_wf,'RES')
           if(l_tddft_gsum)     H_x=H_x+TDDFT_ALDA_eh_space_G_kernel(BS_n_g_fxc,&
&                                   BS_T_grp(i_Tgrp_p),i_Tp, BS_T_grp(i_Tgrp_k),i_Tk)
         endif
         !
         ! Correlations
         if (iHxc==3) then
           if (l_std_alg)       H_c=K_correlation_kernel_std(i_block,i_p,i_pmq, &
&                                   i_k_s,i_kp_s,i_c_k,i_c_p,i_kmq_s,i_kp_mq_s,i_v_k,i_v_p,     &
&                                   i_kmq_t,i_pmq_t,i_k_sp_pol_c,i_p_sp_pol_c,i_k_sp_pol_v,i_p_sp_pol_v,  &
&                                   iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1)
           if (l_dir_alg)       H_c=K_correlation_kernel_dir(i_block,i_p,i_pmq, &
&                                   i_k_s,i_p_s, i_c_k,i_c_p,i_kmq_s,i_pmq_s  ,i_v_k,i_v_p,     &
&                                   i_kmq_t,i_pmq_t,i_k_sp_pol_c,i_p_sp_pol_c,i_k_sp_pol_v,i_p_sp_pol_v,  &
&                                   iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1)
         endif
         !         
       case ("C","Q")
         !
         ! :::COUPLING TERM:::
         !=============================
         !
         ! Exchange
         if (iHxc==1) H_x=K_exchange_kernel_coupling(iq, BS_n_g_exch,              &
&                                 BS_T_grp(i_Tgrp_p),i_Tp, BS_T_grp(i_Tgrp_k),i_Tk) 
         !
         ! ALDA
         if (iHxc==2) then
           if(l_tddft_rsum)     H_x=H_x+TDDFT_ALDA_eh_space_R_kernel(is_k,os_k,is_p,os_p,tddft_wf,'CPL')
           if(l_tddft_gsum)     H_x=H_x+TDDFT_ALDA_eh_space_G_kernel(BS_n_g_fxc,&
&                                 BS_T_grp(i_Tgrp_p),i_Tp, BS_T_grp(i_Tgrp_k),i_Tk)
         endif
         !
         ! Correlations
         if (iHxc==3) then
           if (l_std_alg)       H_c=K_correlation_kernel_std(i_block,i_p,i_pmq, &
&                                 i_k_s,i_kp_s,i_c_k,i_v_p,i_kmq_s,i_kp_mq_s,i_v_k,i_c_p,    &
&                                 i_kmq_t,i_pmq_t,i_k_sp_pol_c,i_p_sp_pol_v,i_k_sp_pol_v,i_p_sp_pol_c,  &
&                                 iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1)
           if (l_dir_alg)       H_c=K_correlation_kernel_dir(i_block,i_p,i_pmq, &
&                                 i_k_s,i_p_s ,i_c_k,i_v_p,i_kmq_s,i_pmq_s  ,i_v_k,i_c_p,    &
&                                 i_kmq_t,i_pmq_t,i_k_sp_pol_c,i_p_sp_pol_v,i_k_sp_pol_v,i_p_sp_pol_c,  &
&                                 iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1)
         endif
         !
       end select
       !
       if (l_tddft.and.WF_exx_fraction>0._SP.and.iHxc==3) H_c=H_c*WF_exx_fraction
       !
       sqrt_f_itk=sqrt(cmplx(BS_T_grp(i_Tgrp_k)%f(i_Tk),kind=SP))
       sqrt_f_itp=sqrt(cmplx(BS_T_grp(i_Tgrp_p)%f(i_Tp),kind=SP))
       !
       BS_mat_tmp=sqrt_f_itk*(H_x*Co-H_c)*sqrt_f_itp
       if (mode_now=="C") BS_mat_tmp= cI*BS_mat_tmp
       !
       if (     l_matrix_init) BS_blk(i_block)%mat(i_Tk,i_Tp) = BS_mat_tmp
       if (.not.l_matrix_init) BS_blk(i_block)%mat(i_Tk,i_Tp) = BS_blk(i_block)%mat(i_Tk,i_Tp)+BS_mat_tmp
       !
       if (l_write_kernel) BS_blk(i_block)%done(i_Tk,i_Tp)="t"
       !
       i_v_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,2) 
       i_c_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,3) 
       !
       i_v_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,2) 
       i_c_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,3) 
       !
       i_k_sp_pol_c=BS_T_grp(i_Tgrp_k)%table(i_Tk,4) 
       i_p_sp_pol_c=BS_T_grp(i_Tgrp_p)%table(i_Tp,4) 
       i_k_sp_pol_v=BS_T_grp(i_Tgrp_k)%table(i_Tk,5) 
       i_p_sp_pol_v=BS_T_grp(i_Tgrp_p)%table(i_Tp,5) 

       if (H_pos(1)==H_pos(2)) then
         BS_blk(i_block)%table(:,i_Tk)=BS_T_grp(i_Tgrp_k)%table(i_Tk,:)
         BS_blk(i_block)%E(i_Tk)      =BS_T_grp(i_Tgrp_k)%E(i_Tk,1)
         if (mode_now=="C") then
           ! Swap c and v
           BS_blk(i_block)%table(2,i_Tk)=BS_T_grp(i_Tgrp_k)%table(i_Tk,3)
           BS_blk(i_block)%table(3,i_Tk)=BS_T_grp(i_Tgrp_k)%table(i_Tk,2)
         endif
       endif
       !
     enddo   ! i_Tk
   enddo     ! i_Tp
#if defined _OPENMP
   !DEV_OMP end do
   !DEV_OMP end parallel
#endif
   !
   call timing('X T_space Kernel sum',OPR='stop')
   !
   if(BS_K_cutoff>0._SP.and.l_write_kernel) &
&     BS_max_val_CPU(myid+1)=maxval( (/BS_max_val_CPU(myid+1),abs(BS_blk(i_block)%mat(:,:))/) )
   !
   if(l_BSE_minimize_memory.and.iHxc==1) then
     call BS_exchange_oscillators_free(i_Tgrp_k)
     call BS_exchange_oscillators_free(i_Tgrp_p)
   endif
   !
   if(l_BSE_minimize_memory.and.iHxc==2.and.l_tddft_gsum) then
     call TDDFT_oscillators_free_L(i_Tgrp_k)
     call TDDFT_oscillators_free_R(i_Tgrp_p)
   endif
   !
   if(iHxc==3) call BS_correlation_oscillators_free(i_block,l_std_alg)
   !
100 if(l_write_kernel.or.l_write_kernel_step) then
#if defined _PAR_IO
     call io_control(ACTION=WR,ID=ID)
     call io_BS_PAR_block(iq,i_block,ID,"full")
#else
     call io_control(ACTION=WR,COM=REP,SEC=(/i_block+1/),ID=ID)
     io_BS_err=io_BS(iq,X,ID)
#endif
     if (io_BS_K.and.l_write_kernel_step) call K_block_free(i_block,"all")
   endif
   !
   call live_timing(steps=i_steps)
   !
 enddo block_loop
 !
 if (m_steps>0) call live_timing( )
 !
 ! GLOBAL CLEANING 
 !=================
 !
 if(iHxc==1) call BS_exchange_oscillators_free(BS_nT_grps)
 !
 if(iHxc==2) then
   !
   if (l_tddft_rsum) then
     YAMBO_FREE(tddft_wf%rhotwr1)
     YAMBO_FREE(tddft_wf%rhotwr2)
   else if (l_tddft_gsum) then
     call TDDFT_oscillators_free_L(BS_nT_grps)
     call TDDFT_oscillators_free_R(BS_nT_grps)
     YAMBO_FREE(tddft_wf%rhotwr_DP)
   endif
   YAMBO_FREE(tddft_wf%WF_symm1)
   YAMBO_FREE(tddft_wf%WF_symm2)
   !
   if (l_bs_fxc) then
     YAMBO_FREE(F_xc_gspace)
     YAMBO_FREE(FXC_K_diagonal)
   else if (BS_K_is_ALDA) then
     YAMBO_FREE(F_xc_mat)
   endif
   !
 endif
 !
 if(iHxc==3) then
   if (l_std_alg) then
     YAMBO_FREE(WF_phase)
   endif
   qindx_tmp=qindx_B_close(qindx_ID,qindx_ID_frag)
   YAMBO_FREE_GPU(BS_W)
   YAMBO_FREE(BS_W)
   !
   YAMBO_FREE_GPU(O1)
   YAMBO_FREE_GPU(O2)
   YAMBO_FREE_GPU(O_times_W)
   YAMBO_FREE(O1)
   YAMBO_FREE(O2)
   YAMBO_FREE(O_times_W)
   !
   YAMBO_FREE_GPU(DEV_VAR(RIM_W)) 
   YAMBO_FREE(RIM_W)
 endif
 !
 if ( (iHxc==1 .and. l_bs_exch_wf_in_loop) .or. (iHxc==2.and.l_bs_tddft_wf_in_loop.and.l_tddft_gsum)) then
   call fft_free()
 else
   call WF_free(WF,keep_states_to_load=l_write_kernel)
 endif
 !
 l_matrix_init=.false.
 !
 enddo ! iHxc loop
 !
#if defined _PAR_IO
 if(BS_K_cutoff>0._SP) then
   call PP_redux_wait(BS_max_val_CPU)
   BS_max_val=maxval(BS_max_val_CPU)
   call K_compress(iq,ID_compr)
   call K_compressed_IO(iq,io_BS_err,"write")
 endif
 !
 call io_control(ACTION=WR_CL,COM=REP,ID=ID_head)
 call io_control(ACTION=WR_CL,COM=REP,ID=ID)
 call io_BS_PAR_free(ID_head,ID,"full",.true.)
 ! In this mode the blocks are written to disk after each step and the memory is freed
 if (io_BS_K) then
   if(     l_write_kernel_step) BS_K_has_been_calculated_loaded=.false.
   if(.not.l_write_kernel_step) BSS_kernel_last_stored_block=n_BS_blks
 endif
#else
 call io_control(ACTION=WR_CL,COM=REP,SEC=(/n_BS_blks+2/),ID=ID)
 io_BS_err=io_BS(iq,X,ID)
#endif
 !
 call timing('X T_space Kernel',OPR='stop')
 !
end subroutine K_kernel
