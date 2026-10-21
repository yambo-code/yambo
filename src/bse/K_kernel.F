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
 use pars,          ONLY:SP,DP,schlen,pi,cZERO,cI
 use units,         ONLY:HA2KEL
 use LOGO,          ONLY:pickup_a_random
 use drivers,       ONLY:l_bs_fxc,l_tddft,l_rim_w
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin,n_spin,n_spinor,n_sp_pol
 use FFT_m,         ONLY:fft_size
 use interfaces,    ONLY:WF_load,WF_free,eval_G_minus_G,TDDFT_ALDA_eh_space_R_kernel
 use D_lattice,     ONLY:nsym,DL_vol,i_time_rev,i_space_inv,sop_inv,sop_tab,Tel
 use R_lattice,     ONLY:qindx_X,bz_samp,&
&                        qindx_B,qindx_B_max,qindx_B_load,RIM_W,RIM_W_d,&
&                        RIM_ng,RIM_W_is_diagonal,RIM_W_ng,bare_qpg
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use LIVE_t,        ONLY:live_timing
 use X_m,           ONLY:X_t
 use interfaces,    ONLY:el_density_and_current
 use QP_m,          ONLY:QP_ng_Vxc
 use BS,            ONLY:BS_bands,BS_res_K_corr,BS_W,BS_W_is_diagonal,&
&                        BS_res_K_exchange,BS_Block_size,BS_n_g_W,&
&                        O_ng,BS_n_g_exch,BS_n_g_fxc,BS_identifier,BS_LiveTiming_steps,&
&                        BS_K_dim,BS_K_is_ALDA,BS_K_is_Chi,BS_K_Chi_dynamic,BS_K_Chi_out,BS_K_coupling,BS_cpl_K_exchange,&
&                        BS_G_inplane,&
&                        BS_K_anti_resonant,&
&                        BS_cpl_K_corr,K_EXCH_collision,K_CORR_collision,&
&                        WF_phase,n_BS_blks,BS_blk,BS_T_grp,&
&                        BS_nT_grps,BS_blks_free,l_BS_ares_from_res,&
&                        l_BSE_minimize_memory,l_BSE_restart,l_BSE_kernel_complete,&
&                        BS_perturbative_SOC,BS_K_cutoff,BS_max_val,l_BS_magnons
 use collision_el,  ONLY:elemental_collision_free
 use ALLOC,         ONLY:DIPOLE_ALLOC_global
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:REP,OP_WR,RD,WR_CL,OP_APP,deliver_IO_error_message,WR,OP_RD_CL,DUMP
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
 use Chi_m,         ONLY:Chi_ng,Chi_n_freqs,Chi_freqs,Chi_fxc,Chi_free
 use Chi_m,         ONLY:Chi_optical_q0,Chi_optical_q0_known,Chi_G_mode,Chi_G_db
 use Chi_m,         ONLY:Chi_occupations,Chi_occ_bands,Chi_occ_nk,Chi_occ_nsp,Chi_occ_nbf,Chi_occ_nbm,&
&                        Chi_occ_Tel,Chi_occ_free
 use Chi_m,         ONLY:Chi_dyn_V,Chi_dyn_s,Chi_dyn_filled,Chi_dyn_c,Chi_dyn_free
 use BS_solvers,    ONLY:BSS_q0
 use TDDFT_Chi_Fourier_m, ONLY:TDDFT_Chi_pair_G
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
 type(w_samp)               :: Chi_w
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
&               i_kmq_t,i_pmq_t,i_k_bz_mem,i_kmq_bz_mem,qindx_tmp(2)
 ! OMP LAST DEF
 integer ::     i_k_bz_last,i_p_bz_last,i_pmq_bz_last,i_kmq_bz_last, & 
&               iq_W_bz_last,ig_W_last,iq_W_bz_mq_last,ig_W_mq_last,ig_W_final
 !
 integer ::     i_block,O_ng_shift,O_ng_exch,O_ng_tddft,iHxc,NG(2),NK(2),&
&               ig_kmq,ig_pmq,ig_W_mq,iq_W_bz_mq,iq_W_mq,iq_W_s_mq,&
&               bands_to_load(2),is_k(4),os_k(4),is_p(4),os_p(4)
 !
 complex(SP) :: Co,H_x,H_c,BS_mat_tmp
 complex(SP) :: sqrt_f_itk,sqrt_f_itp
 !
 real(SP),    allocatable :: BS_max_val_CPU(:),rho(:)
 complex(SP), allocatable DEV_ATTR :: O1(:),O2(:),O_times_W(:)
 !
 complex(SP),   external :: K_correlation_kernel_std,K_correlation_kernel_dir
 complex(SP),   external :: TDDFT_ALDA_eh_space_G_kernel
 complex(SP),   external :: K_exchange_kernel_resonant,K_exchange_kernel_coupling
 !
 logical    :: l_bs_exch,l_bs_corr,l_bs_exch_wf_in_loop,l_bs_tddft_wf_in_loop,l_matrix_init,&
 &             l_load_kernel,l_write_kernel,l_skip_phases,l_std_alg,l_dir_alg,l_tddft_gsum,l_tddft_rsum
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
 integer :: ID_Chi,io_Chi_err,chi_ierr,ig,jg,chi_grp_p,i_T_dyn,i_ra
 integer, external :: io_Chi
 logical :: l_KfnQPdb,l_chi_vtx,l_chi_vtx_only
 real(SP) :: chi_norm,chi_antiherm,chi_scale,chi_raw_norm,chi_raw_antiherm,chi_diff,chi_occ_diff
 complex(DP) :: chi_pair
 complex(DP), allocatable :: Chi_static(:,:)
 complex(SP), allocatable :: chi_R_res(:,:),chi_R_cpl(:,:)
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
   if (BS_K_is_ALDA.or.BS_K_is_Chi) then
     section_title="TDDFT Kernel"
   else
     section_title="BSE Kernel"
   endif
   section_title=trim(section_title)//" @q"//trim(intc(iq))
   !
   if (BS_res_K_corr.or.BS_res_K_exchange)     section_title=trim(section_title)//" (Resonant"
   if (BS_K_is_ALDA     )                      section_title=trim(section_title)//" ALDA"
   if (BS_K_is_Chi      )                      section_title=trim(section_title)//" Chi"
   if (BS_res_K_corr    )                      section_title=trim(section_title)//" CORRRELATION"
   if (BS_res_K_exchange)                      section_title=trim(section_title)//" EXCHANGE"
   if (BS_res_K_corr.or.BS_res_K_exchange)     section_title=trim(section_title)//")"
   !
   if (BS_cpl_K_corr.or.BS_cpl_K_exchange)     section_title=trim(section_title)//" (Coupling"
   if (BS_K_is_ALDA.and.BS_cpl_K_exchange)     section_title=trim(section_title)//" ALDA"
   if (BS_K_is_Chi .and.BS_cpl_K_exchange)     section_title=trim(section_title)//" Chi"
   if (BS_cpl_K_corr    )                      section_title=trim(section_title)//" CORRRELATION"
   if (BS_cpl_K_exchange)                      section_title=trim(section_title)//" EXCHANGE"
   if (BS_cpl_K_corr.or.BS_cpl_K_exchange)     section_title=trim(section_title)//")"
   !
 endif
 !
 call section('=',trim(section_title))
 !
 l_bs_exch=(any((/BS_res_K_exchange,BS_cpl_K_exchange/)).or.l_bs_fxc).and..not.(l_BS_magnons.and.n_sp_pol==2)
 !
 ! Chi transition vertices: the CHI kernel itself, or a kernel exported from this BSE
 ! (BSEChiOut, K_Chi_export), which only collects the vertices in the tddft pass.
 l_chi_vtx     =BS_K_is_Chi.or.trim(BS_K_Chi_out)/='none'
 l_chi_vtx_only=l_chi_vtx.and..not.BS_K_is_Chi
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
 if (BS_K_is_ALDA.or.l_chi_vtx) then
   O_ng_tddft =maxval(qindx_X(iq,:,2))
   l_tddft_gsum=l_chi_vtx.or.(index(FXC_mode,"G-")>0)
   l_tddft_rsum=(.not.l_chi_vtx).and.(index(FXC_mode,"R-")>0)
   if( .not.(l_tddft_gsum.or.l_tddft_rsum) ) call error("Wrong FXC_mode:"//trim(FXC_mode))
   if(BS_K_is_ALDA.and.index(FXC_mode,"cut_GmGp")>0) then
     FXC_n_g_corr=BS_n_g_fxc
     if (BS_n_g_fxc/= eval_G_minus_G(BS_n_g_fxc,0) ) then
       BS_n_g_fxc   = eval_G_minus_G(BS_n_g_fxc,0)
       call msg('s','[TDDFT] Fxc cutoff is forced by symmetry to be ',BS_n_g_fxc)
     endif
   endif
   l_bs_tddft_wf_in_loop= ((iq==1) .and. (Xk%nbz>=1).and.(.not.trim(PARALLEL_default_mode)=="workload") ) .or. &
   &                      ((iq> 1) .and.                       trim(PARALLEL_default_mode)=="KQmemory" )
   if (BS_K_is_ALDA) call msg('r','[BSE] ALDA components ',BS_n_g_fxc)
   if (BS_K_is_Chi ) call msg('r','[BSE] Chi  components ',BS_n_g_fxc)
   if (l_chi_vtx_only) call msg('r','[BSE] Chi export components ',BS_n_g_fxc)
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
 call K_restart(iq,X,ID_head,ID,ID_compr,l_partial_kernel_loaded)
 if (l_chi_vtx.and.l_partial_kernel_loaded)&
&  call error('[Chi/BSK] partial restart cannot verify ndb.Chi; use a new BSE job')
 !
 if( l_BSE_kernel_complete ) then
   if (l_chi_vtx) &
&    call error('[Chi/BSK] kernel restart cannot rebuild the Chi vertices; use a new BSE job')
   return
 endif
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
   YAMBO_FREE_GPU(DEV_VAR(RIM_W))
   YAMBO_FREE(RIM_W)
   !
   if (l_chi_vtx.and.iq==1) call DIPOLE_ALLOC_global()
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
 !
 BS_blk_done=.false.
 !
 do iHxc=1,3
   !
 if ((.not.l_bs_exch).and.iHxc==1) cycle
 !
 if ((.not.(BS_K_is_ALDA.or.l_chi_vtx)).and.iHxc==2) cycle
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
   if (BS_K_is_Chi) then
     if (ncpu/=1) call error('[Chi/BSK] transition projection is currently serial only')
     ! Explicit anti-resonant transitions (finite q, coupling): every block, mode "F", projects
     ! fxc on the vertices of its own transitions. Not at the optical q (ImposeAsym), nor
     ! without coupling (two BSE matrices).
     if ((BS_K_coupling.or.BS_K_anti_resonant).and..not.l_BS_ares_from_res.and.(iq==1.or..not.BS_K_coupling)) &
&      call error('[Chi/BSK] the anti-resonant blocks must be built from the resonant ones')
     chi_grp_p=0
     ! Scalar or spinor (SOC) states: the vertices are the transition densities summed over
     ! the spinor components, the occupations and Co carry spin_occ (2 or 1). Collinear spin
     ! polarization, magnons and perturbative SOC (energies not in the vertices) are not supported.
     if (n_sp_pol/=1.or.l_BS_magnons.or.BS_perturbative_SOC) &
&      call error('[Chi/BSK] scalar or spinor transitions only (no spin polarization, magnons or perturbative SOC)')
     ! OP_RD_CL: RD_CL only reads an already-open unit and never assigns an ID.
     call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1/),ID=ID_Chi)
     io_Chi_err=io_Chi(X,Chi_w,ID_Chi)
     if (io_Chi_err/=0) call error('[Chi/BSK] cannot read ndb.Chi header (IO code '//trim(intc(io_Chi_err))//&
&      '); put ndb.Chi in the job directory or list its job in -J')
     if (iq==1) then
       if (.not.Chi_optical_q0_known) call error('[Chi/BSK] ndb.Chi lacks the optical q0 direction')
       if (maxval(abs(Chi_optical_q0-BSS_q0))>&
&          max(1.E-9_SP,1.E-3_SP*maxval(abs(Chi_optical_q0))))&
&        call error('[Chi/BSK] optical q0 direction or normalization differs from ndb.Chi')
     endif
     if (Chi_ng/=BS_n_g_fxc) call error('[Chi/BSK] BSENGfxc must equal the ndb.Chi G-basis size')
     ! A kernel exported with BSEGinplane is exact only with the exchange on the same G
     if ((index(Chi_G_db,'BSEGinplane')>0).neqv.BS_G_inplane) &
&      call error('[Chi/BSK] BSEGinplane must match the ndb.Chi export (exchange and fxc on the same G)')
     ! Transition energies: KS for a kernel built on the KS bubble. A kernel exported with
     ! BSEChiOut= "exc" is built on the bubble of its BSE energies and needs the same QP ones.
     l_KfnQPdb=.FALSE.
     if (allocated(Ken%Eo)) l_KfnQPdb=maxval(abs(Ken%Eo-Ken%E))>1.E-6_SP
     if (trim(Chi_G_mode)=='BSE-EXC') then
       if (.not.l_KfnQPdb) call error('[Chi/BSK] this ndb.Chi (BSEChiOut= "exc") needs the QP corrections of its BSE (KfnQPdb)')
       call msg('r','[Chi/BSK] excitonic kernel on QP transitions (ndb.Chi from BSEChiOut= "exc")')
     else if (l_KfnQPdb) then
       call error('[Chi/BSK] use KS transitions; QP corrections would double count the QP shift')
     endif
     if (trim(Chi_G_mode)=='BSE-KS') call msg('r','[Chi/BSK] kernel from BSEChiOut= "full": it carries the QP shift, '//&
&      'so binding energies are measured from the QP gap of the exporting BSE, not from these KS transitions')
     ! The exported kernel is exact only with the occupations of its BSE: with partly filled
     ! bands the temperature of each job decides them (stop before the projection and solver)
     if (allocated(Chi_occupations)) then
       if (any(Chi_occ_bands/=BS_bands).or.Chi_occ_nk/=Ken%nk.or.Chi_occ_nsp/=n_sp_pol) &
&        call error('[Chi/BSK] the ndb.Chi occupations are on other bands, k points or spins')
       chi_occ_diff=maxval(abs(Chi_occupations-Ken%f(BS_bands(1):BS_bands(2),:,:)))/spin_occ
       call msg('r','[Chi/BSK] occupations vs the exporting BSE: max |df|, its and this temperature [K]',&
&               (/chi_occ_diff,Chi_occ_Tel*HA2KEL,Tel*HA2KEL/))
       if (chi_occ_diff>1.E-6_SP.or.Chi_occ_nbf/=minval(Ken%nbf(:n_sp_pol)).or.&
&          Chi_occ_nbm/=maxval(Ken%nbm(:n_sp_pol))) &
&        call error('[Chi/BSK] occupations differ from those of the exporting BSE. Use its ElecTemp: a job '//&
&                   'reading the databases of another job takes their temperature; a negative ElecTemp in both '//&
&                   'runs keeps it fixed')
       call Chi_occ_free(.TRUE.)
     else if (index(Chi_G_mode,'BSE-')==1) then
       call warning('[Chi/BSK] ndb.Chi from an export without stored occupations: they are not checked')
     endif
     call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/2*iq,2*iq+1/),ID=ID_Chi)
     io_Chi_err=io_Chi(X,Chi_w,ID_Chi)
     if (io_Chi_err/=0) call error('[Chi/BSK] cannot read ndb.Chi q fragment (IO code '//trim(intc(io_Chi_err))//')')
     if (Chi_n_freqs<1) call error('[Chi/BSK] ndb.Chi contains no frequency samples')
     if (BS_K_Chi_dynamic) then
       ! fxc(w) stays out of the BSE matrix. The transition vertices are kept below and
       ! K_Chi_dynamic projects fxc at every BSE frequency (checked there against ndb.Chi).
       call Chi_dyn_free()
       if (.not.l_BS_ares_from_res.and.BS_K_dim(2)/=BS_K_dim(1)) &
&        call error('[Chi/BSK] BSEChiDyn: as many anti-resonant as resonant transitions are needed')
       allocate(Chi_dyn_V(Chi_ng,BS_K_dim(1),2),Chi_dyn_s(BS_K_dim(1)),Chi_dyn_filled(BS_K_dim(1),2))
       Chi_dyn_V=(0._DP,0._DP)
       Chi_dyn_s=0._DP
       Chi_dyn_filled=.FALSE.
       Chi_dyn_c=real(Co,DP)/(4._DP*real(pi,DP))
       call msg('r','[Chi/BSK] frequency-dependent kernel from ndb.Chi @q',iq)
       call msg('r','[Chi/BSK] ndb.Chi frequencies',Chi_n_freqs)
     else
       if (abs(Chi_freqs(1))>1.E-7_SP) &
&      call error('[Chi/BSK] first ndb.Chi frequency must be exactly zero for static Casida')
       ! Hermiticity is judged twice: on the stored fxc, as before, and on
       ! |q+G| fxc |q+G'| = 4 pi (A^-1 - B^-1), which is O(1) in every block.
       ! At q=0 the stored head is O(1/q0**2) and hides the body from the first.
       chi_raw_norm=1._SP
       chi_raw_antiherm=0._SP
       chi_norm=0._SP
       chi_antiherm=0._SP
       do jg=1,Chi_ng
         do ig=1,Chi_ng
           chi_diff=abs(Chi_fxc(ig,jg,1)-conjg(Chi_fxc(jg,ig,1)))
           chi_scale=abs(bare_qpg(iq,ig))*abs(bare_qpg(iq,jg))
           chi_raw_norm=max(chi_raw_norm,abs(Chi_fxc(ig,jg,1)))
           chi_raw_antiherm=max(chi_raw_antiherm,chi_diff)
           chi_norm=max(chi_norm,abs(Chi_fxc(ig,jg,1))*chi_scale)
           chi_antiherm=max(chi_antiherm,chi_diff*chi_scale)
         enddo
       enddo
       if (chi_raw_antiherm>1.E-4_SP*chi_raw_norm.or.&
&        (chi_antiherm>1.E-4_SP*chi_norm.and.chi_antiherm>tiny(1._SP))) &
&      call error('[Chi/BSK] zero-frequency fxc is not Hermitian; static Casida is invalid')
       YAMBO_ALLOC(Chi_static,(Chi_ng,Chi_ng))
       Chi_static=cmplx(Chi_fxc(:,:,1),kind=DP)
       call msg('r','[Chi/BSK] static inverse-Fourier kernel from ndb.Chi @q',iq)
     endif
   endif
   !
   if (l_chi_vtx_only) then
     ! BSEChiOut: collect the Chi vertices of every transition for K_Chi_export
     if (ncpu/=1) call error('[Chi/BSK] BSEChiOut: the vertex pass is serial (MPI); use OpenMP threads')
     if (n_sp_pol/=1.or.l_BS_magnons.or.BS_perturbative_SOC) &
&      call error('[Chi/BSK] BSEChiOut: scalar or spinor transitions only (no spin polarization, magnons or perturbative SOC)')
     if (.not.BS_res_K_exchange.or.BS_n_g_exch/=BS_n_g_fxc) &
&      call error('[Chi/BSK] BSEChiOut: exchange on, with BSENGexx equal to BSENGfxc (one G basis for v and P)')
     call Chi_dyn_free()
     if (.not.l_BS_ares_from_res.and.BS_K_dim(2)/=BS_K_dim(1)) &
&      call error('[Chi/BSK] BSEChiOut: as many anti-resonant as resonant transitions are needed')
     allocate(Chi_dyn_V(BS_n_g_fxc,BS_K_dim(1),2),Chi_dyn_s(BS_K_dim(1)),Chi_dyn_filled(BS_K_dim(1),2))
     Chi_dyn_V=(0._DP,0._DP)
     Chi_dyn_s=0._DP
     Chi_dyn_filled=.FALSE.
     Chi_dyn_c=real(Co,DP)/(4._DP*real(pi,DP))
     chi_grp_p=0
   endif
   !
   call WF_load(WF,NG(1),NG(2),(/1,Ken%nbm/),(/1,Xk%nibz/),space='R',title="-TDDFT",keep_states_to_load=.true.)
   !
   if (BS_K_is_ALDA) QP_ng_Vxc=BS_n_g_fxc
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
   if (BS_K_is_ALDA) then
     YAMBO_ALLOC(F_xc,(fft_size,n_spin,n_spin))
     if(l_BS_magnons .or. n_spinor==2 .or. l_Fxc_from_Vxc) then
       YAMBO_ALLOC(V_xc,(fft_size,n_spin))
       V_xc=0._SP
     endif
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
     call TDDFT_ALDA_lower_cutoff(FXC_mode)
   endif
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
     BS_blk_done(i_block)=.true.
     skip_check: do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
       do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
         if (BS_blk(i_block)%done(i_Tk,i_Tp)=="t") cycle
         BS_blk_done(i_block)=.false.
         exit skip_check
       enddo
     enddo skip_check
     !
     if (BS_blk_done(i_block)) m_steps=m_steps-i_steps
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
   if (iHxc==1) l_write_kernel=.not.(BS_K_is_ALDA.or.BS_K_is_Chi.or.l_bs_corr)
   if (iHxc==2) l_write_kernel=.not. l_bs_corr
   if (iHxc==3) l_write_kernel=.true.
   !
   if (BS_blk_done(i_block)) then
#if defined _PAR_IO
     if(l_partial_kernel_loaded.and.l_write_kernel) then
       call io_control(ACTION=WR,ID=ID)
       call io_BS_PAR_block(iq,i_block,ID,"full")
     endif
#endif
     cycle
   endif
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
   if(iHxc==2 .and. l_tddft_gsum .and. .not.l_chi_vtx_only) then
     if (.not.allocated( BS_T_grp(i_Tgrp_k)%O_tddft_L) ) then
       call TDDFT_oscillators_alloc_L(i_Tgrp_k)
       call TDDFT_ALDA_eh_space_G_collisions_L(iq,Xk,i_Tgrp_k,NG,l_bs_tddft_wf_in_loop,tddft_wf,mode_now)
     endif
     if (.not.allocated( BS_T_grp(i_Tgrp_p)%O_tddft_R) ) then
       call TDDFT_oscillators_alloc_R(i_Tgrp_p)
       call TDDFT_ALDA_eh_space_G_collisions_R(iq,Xk,i_Tgrp_p,NG,l_bs_tddft_wf_in_loop,tddft_wf,mode_now,BS_K_is_Chi)
     endif
   endif
   !
   ! Chi right vertices. The R and C blocks of a column share one O_tddft_R cache,
   ! filled by whichever block comes first, but the coupling vertex is the
   ! de-excitation density (v<->c, flipped optical head). Keep both for the column.
   if(iHxc==2 .and. l_tddft_gsum .and. l_chi_vtx .and. chi_grp_p/=i_Tgrp_p) then
     if (allocated(chi_R_res)) deallocate(chi_R_res)
     if (allocated(chi_R_cpl)) deallocate(chi_R_cpl)
     if (.not.allocated(BS_T_grp(i_Tgrp_p)%O_tddft_R)) call TDDFT_oscillators_alloc_R(i_Tgrp_p)
     allocate(chi_R_res(size(BS_T_grp(i_Tgrp_p)%O_tddft_R,1),size(BS_T_grp(i_Tgrp_p)%O_tddft_R,2)))
     call TDDFT_ALDA_eh_space_G_collisions_R(iq,Xk,i_Tgrp_p,NG,l_bs_tddft_wf_in_loop,tddft_wf,"R",.TRUE.)
     chi_R_res=BS_T_grp(i_Tgrp_p)%O_tddft_R(:,:,1,1)
     ! The dynamic solver needs the de-excitation vertex also in TDA (anti-resonant block).
     ! Explicit anti-resonant transitions (finite q) are groups of their own: their "R"
     ! vertex, with the bands of their table, is the one of BS_mat row N+t.
     if ((BS_K_coupling.or.BS_K_Chi_dynamic).and.l_BS_ares_from_res) then
       allocate(chi_R_cpl(size(chi_R_res,1),size(chi_R_res,2)))
       call TDDFT_ALDA_eh_space_G_collisions_R(iq,Xk,i_Tgrp_p,NG,l_bs_tddft_wf_in_loop,tddft_wf,"C",.TRUE.)
       chi_R_cpl=BS_T_grp(i_Tgrp_p)%O_tddft_R(:,:,1,1)
     endif
     if (BS_K_Chi_dynamic.or.l_chi_vtx_only) then
       ! Column coordinate of this group in its (resonant or anti-resonant) space
       i_ra=BS_T_grp(i_Tgrp_p)%i_res_ares
       do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
         i_T_dyn=BS_blk(i_block)%coordinate(2)+i_Tp-1
         if (i_T_dyn>BS_K_dim(1)) call error('[Chi/BSK] BSEChiDyn: use BSEmod= "coupling" or "resonant"')
         if (l_BS_ares_from_res) then
           Chi_dyn_V(:,i_T_dyn,1)=cmplx(chi_R_res(:,i_Tp),kind=DP)
           Chi_dyn_V(:,i_T_dyn,2)=cmplx(chi_R_cpl(:,i_Tp),kind=DP)
           Chi_dyn_s(i_T_dyn)=sqrt(real(BS_T_grp(i_Tgrp_p)%f(i_Tp),DP))
           Chi_dyn_filled(i_T_dyn,:)=.TRUE.
         else
           Chi_dyn_V(:,i_T_dyn,i_ra)=cmplx(chi_R_res(:,i_Tp),kind=DP)
           if (i_ra==1) Chi_dyn_s(i_T_dyn)=sqrt(real(BS_T_grp(i_Tgrp_p)%f(i_Tp),DP))
           Chi_dyn_filled(i_T_dyn,i_ra)=.TRUE.
         endif
       enddo
     endif
     chi_grp_p=i_Tgrp_p
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
   !DEV_OMP parallel default(shared), &
   !DEV_OMP private(i_Tk,i_Tp, O1,O2,O_times_W, &
   !DEV_OMP &       sqrt_f_itk,i_k_bz,i_v_k,i_c_k,i_k_sp_pol_c,i_k_sp_pol_v,i_k_s,is_k,os_k,i_k_s_m1,H_pos, &
   !DEV_OMP &       sqrt_f_itp,i_p_bz,i_v_p,i_c_p,i_p_sp_pol_c,i_p_sp_pol_v,i_p_s,is_p,os_p,i_kp_s, &
   !DEV_OMP &       i_kmq_bz,ig_kmq,i_kmq,i_kmq_s,i_kmq_t,i_kmq_s_m1,i_kp_mq_s, &
   !DEV_OMP &       i_pmq_bz,ig_pmq,i_pmq,i_pmq_s,i_pmq_t,ig_W_final, &
   !DEV_OMP &       ig_W,iq_W,iq_W_s,iq_W_bz,iq_W_bz_mq,ig_W_mq,iq_W_mq,iq_W_s_mq,H_x,H_c, &
   !DEV_OMP &       i_p_bz_last,i_k_bz_last,i_pmq_bz_last,i_kmq_bz_last, &
   !DEV_OMP &       iq_W_bz_last,ig_W_last,iq_W_bz_mq_last,ig_W_mq_last, &
   !DEV_OMP &       i_k_bz_mem,i_kmq_bz_mem,qindx_tmp,BS_mat_tmp,tddft_wf,chi_pair,chi_ierr )
   !
   call OPENMP_update(master_thread)   
   !
   i_p_bz_last=0   ; i_k_bz_last=0
   i_pmq_bz_last=0 ; i_kmq_bz_last=0
   !
   ! workspace (for thread-safety, these vars are kept as
   !            local in K_correlation_kernel*, and only allocated
   !            here for performance in the presence of GPUs)
   !
   if (iHxc==3) then
     YAMBO_ALLOC_GPU(O1,(BS_n_g_W))
     YAMBO_ALLOC_GPU(O2,(BS_n_g_W))
     YAMBO_ALLOC_GPU(O_times_W,(BS_n_g_W))
   endif
   !
   !DEV_OMP do collapse(2), schedule(dynamic)
   !
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
           if(l_tddft_gsum.and.BS_K_is_ALDA) H_x=H_x+TDDFT_ALDA_eh_space_G_kernel(BS_n_g_fxc,&
&                                   BS_T_grp(i_Tgrp_p),i_Tp, BS_T_grp(i_Tgrp_k),i_Tk)
           if(BS_K_is_Chi.and..not.BS_K_Chi_dynamic) then
             call TDDFT_Chi_pair_G(Chi_static,&
&              cmplx(BS_T_grp(i_Tgrp_k)%O_tddft_L(:,i_Tk,1,1),kind=DP),&
&              cmplx(chi_R_res(:,i_Tp),kind=DP),chi_pair,chi_ierr)
             if (chi_ierr/=0) call error('[Chi/BSK] transition projection failed: code '//trim(intc(chi_ierr))//&
&              ' ng_fxc='//trim(intc(BS_n_g_fxc))//' fxc='//trim(intc(size(Chi_static,1)))//'x'//&
&              trim(intc(size(Chi_static,2)))//' L='//trim(intc(size(BS_T_grp(i_Tgrp_k)%O_tddft_L,1)))//&
&              ' R='//trim(intc(size(chi_R_res,1))))
             H_x=H_x+cmplx(chi_pair,kind=SP)/(4._SP*pi)
           endif
         endif
         !
         ! Correlations
         if (iHxc==3) then
           if (l_std_alg)       H_c=K_correlation_kernel_std(i_block,i_p,i_pmq, &
&                                   i_k_s,i_kp_s,i_c_k,i_c_p,i_kmq_s,i_kp_mq_s,i_v_k,i_v_p,     &
&                                   i_kmq_t,i_pmq_t,i_k_sp_pol_c,i_p_sp_pol_c,i_k_sp_pol_v,i_p_sp_pol_v,  &
&                                   iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1,&
&                                   BS_n_g_W,O1,O2,O_times_W)
           if (l_dir_alg)       H_c=K_correlation_kernel_dir(i_block,i_p,i_pmq, &
&                                   i_k_s,i_p_s, i_c_k,i_c_p,i_kmq_s,i_pmq_s  ,i_v_k,i_v_p,     &
&                                   i_kmq_t,i_pmq_t,i_k_sp_pol_c,i_p_sp_pol_c,i_k_sp_pol_v,i_p_sp_pol_v,  &
&                                   iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1,&
&                                   BS_n_g_W,O1,O2,O_times_W)
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
           if(l_tddft_gsum.and.BS_K_is_ALDA) H_x=H_x+TDDFT_ALDA_eh_space_G_kernel(BS_n_g_fxc,&
&                                 BS_T_grp(i_Tgrp_p),i_Tp, BS_T_grp(i_Tgrp_k),i_Tk)
           if(BS_K_is_Chi.and..not.BS_K_Chi_dynamic) then
             call TDDFT_Chi_pair_G(Chi_static,&
&              cmplx(BS_T_grp(i_Tgrp_k)%O_tddft_L(:,i_Tk,1,1),kind=DP),&
&              cmplx(chi_R_cpl(:,i_Tp),kind=DP),chi_pair,chi_ierr)
             if (chi_ierr/=0) call error('[Chi/BSK] transition projection failed: code '//trim(intc(chi_ierr))//&
&              ' ng_fxc='//trim(intc(BS_n_g_fxc))//' fxc='//trim(intc(size(Chi_static,1)))//'x'//&
&              trim(intc(size(Chi_static,2)))//' L='//trim(intc(size(BS_T_grp(i_Tgrp_k)%O_tddft_L,1)))//&
&              ' R='//trim(intc(size(chi_R_cpl,1))))
             H_x=H_x+cmplx(chi_pair,kind=SP)/(4._SP*pi)
           endif
         endif
         !
         ! Correlations
         if (iHxc==3) then
           if (l_std_alg)       H_c=K_correlation_kernel_std(i_block,i_p,i_pmq, &
&                                 i_k_s,i_kp_s,i_c_k,i_v_p,i_kmq_s,i_kp_mq_s,i_v_k,i_c_p,    &
&                                 i_kmq_t,i_pmq_t,i_k_sp_pol_c,i_p_sp_pol_v,i_k_sp_pol_v,i_p_sp_pol_c,  &
&                                 iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1,&
&                                 BS_n_g_W,O1,O2,O_times_W)
           if (l_dir_alg)       H_c=K_correlation_kernel_dir(i_block,i_p,i_pmq, &
&                                 i_k_s,i_p_s ,i_c_k,i_v_p,i_kmq_s,i_pmq_s  ,i_v_k,i_c_p,    &
&                                 i_kmq_t,i_pmq_t,i_k_sp_pol_c,i_p_sp_pol_v,i_k_sp_pol_v,i_p_sp_pol_c,  &
&                                 iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1,&
&                                 BS_n_g_W,O1,O2,O_times_W)
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
   !
   !DEV_OMP end do
   !
   if (iHxc==3) then
     YAMBO_FREE_GPU(O1)
     YAMBO_FREE_GPU(O2)
     YAMBO_FREE_GPU(O_times_W)
     YAMBO_FREE(O1)
     YAMBO_FREE(O2)
     YAMBO_FREE(O_times_W)
   endif
   !
   !DEV_OMP end parallel
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
100 if(l_write_kernel) then
#if defined _PAR_IO
     call io_control(ACTION=WR,ID=ID)
     call io_BS_PAR_block(iq,i_block,ID,"full")
#else
     call io_control(ACTION=WR,COM=REP,SEC=(/i_block+1/),ID=ID)
     io_BS_err=io_BS(iq,X,ID)
#endif
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
   else if (l_chi_vtx) then
     if (allocated(Chi_static)) then
       YAMBO_FREE(Chi_static)
     endif
     if (allocated(chi_R_res)) deallocate(chi_R_res)
     if (allocated(chi_R_cpl)) deallocate(chi_R_cpl)
     ! K_Chi_dynamic still needs fxc(w) and its frequencies; the export has no ndb.Chi yet
     if (BS_K_is_Chi.and..not.BS_K_Chi_dynamic) call Chi_free()
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
#else
 call io_control(ACTION=WR_CL,COM=REP,SEC=(/n_BS_blks+2/),ID=ID)
 io_BS_err=io_BS(iq,X,ID)
#endif
 !
 call timing('X T_space Kernel',OPR='stop')
 if (l_chi_vtx.and.iq==1) call DIPOLE_ALLOC_global()
 !
end subroutine K_kernel
