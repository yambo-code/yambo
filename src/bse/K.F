!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine K(iq,Ken,Xk,q,X,Xw,W_bss)
 !
 ! K = <2V-W>
 !
 use pars,         ONLY:IP,SP,schlen,pi,cZERO,cI
 use LOGO,         ONLY:pickup_a_random
 use drivers,      ONLY:l_bs_fxc,l_td_hf,l_bss
 use frequency,    ONLY:w_samp
 use electrons,    ONLY:levels,spin_occ,spin,n_spin
 use FFT_m,        ONLY:fft_size
 use interfaces,   ONLY:WF_load,WF_free
 use functions,    ONLY:K_scatter
 use D_lattice,    ONLY:nsym,DL_vol,i_time_rev,sop_tab,sop_inv,i_space_inv
 use R_lattice,    ONLY:G_m_G,g_rot,qindx_B,bz_samp,RIM_qpg,&
&                       RIM_anisotropy,minus_G,bare_qpg,qindx_X
 use com,          ONLY:warning,error,msg
 use stderr,       ONLY:intc
 use LIVE_t,       ONLY:live_timing
 use X_m,          ONLY:X_alloc,X_t,X_mat
 use BS_solvers,   ONLY:BSS_q0
 use BS,           ONLY:BS_bands,BS_res_K_corr,BS_W_is_diagonal,&
&                       BS_res_K_exchange,BS_n_g_W,BS_Block_size,&
&                       O_ng,BS_n_g_exch,BS_identifier,BS_LiveTiming_steps,&
&                       BS_K_dim,BS_K_is_ALDA,BS_cpl_K_exchange,&
&                       BS_cpl_K_corr,K_EXCH_collision,K_CORR_collision,&
&                       BS_oscillators_free,WF_phase,n_BS_blks,BS_blk,BS_T_grp,&
&                       BS_K_has_been_calculated_loaded,BS_nT_grps,BS_blks_free,&
&                       l_BSE_minimize_memory, l_restart_bse
 use collision_el, ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use IO_m,         ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,OP_RD,&
&                       RD,RD_CL,RD_CL_IF_END,OP_WR_CL,OP_WR,OP_APP_WR_CL,DUMP,&
&                       deliver_IO_error_message
 use TDDFT,         ONLY:FXC_K_diagonal,FXC,FXC_n_g_corr,io_BS_Fxc
 use xc_functionals,ONLY:F_xc,XC_potential_driver
 use wrapper_omp,   ONLY:V_dot_V_omp,Vstar_dot_V_omp,M_by_V_omp
 use global_XC,     ONLY:WF_kind,WF_xc_functional
 use wave_func,     ONLY:WF
 use timing_m,      ONLY:timing
 !
#if defined _KERR
 use KERR,           ONLY:BSS_KERR_free
#endif
#if defined _PL
 use PHOTOLUM,       ONLY:BSS_PL_free
#endif
 !
#include<memory.h>
 type(levels)  ::Ken 
 type(bz_samp) ::Xk,q
 type(X_t)     ::X
 type(w_samp)  ::Xw,W_bss
 integer       ::iq
 !
 ! Work Space ...
 !
 character(schlen) :: section_title
 !
 ! ... Kernel loop
 !
 integer    :: i_block,i_block_vv,i_block_cc,i_block_vc,i_block_cv,i_c_k,i_v_k,i_c_p,i_v_p,&
&              i_k_bz,i_p_bz,i_k_s,i_p_s,i_kp_s,i_k,i_p,i_k_s_m1,&
&              i_kmq_bz,i_pmq_bz,i_kmq_s,i_pmq_s,i_kp_mq_s,i_kmq,i_pmq,i_kmq_s_m1,ig_kmq,ig_pmq,i_kmq_t,i_pmq_t,&
&              i_Tgrp_k,i_Tgrp_p,i_Tp,i_Tk,i_k_spin,i_p_spin,H_pos(2),&
&              i_g1,i_g2,i_g3,O_ng_shift,&
&              ig_W,iq_bz_W,iq_W,iq_W_s,&
&              ig_W_mq,iq_bz_W_mq,iq_W_mq,iq_W_s_mq,&
&              bands_to_load(2),iOvv,iOcc,iOvc,iOcv,is(4),os(4),isp(4),osp(4)
 complex(SP):: Co,H_x,H_c,PHASE_1,PHASE_2
 real(SP)   :: sqrt_f_itk,sqrt_f_itp
 !
 complex(SP),allocatable ::BS_W(:,:,:)
 complex(SP),   external ::TDDFT_ALDA_R_space
 logical                 ::W_is_em1s,W_is_pp
 !
 ! Oscillators
 !
 logical    :: load_O_X
 complex(SP),allocatable  ::Ovv(:),Occ(:),O_times_W(:)
 complex(SP),allocatable  ::Ovc(:),Ocv(:)
 type(elemental_collision)::isc
 !
 ! I/O
 !
 integer           ::io_X_err,io_BS_Fxc_err,io_BS_err,XID,ID
 integer, external ::io_X,io_BS
 !
 ! Constants & Setup
 !==================
 !
 Co=4._SP*real(spin_occ)*pi/DL_vol/real(q%nbz,SP)
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
 call timing('X T_space Kernel',OPR='start')
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
   if (BS_res_K_corr.or.BS_K_is_ALDA)          section_title=trim(section_title)//" CORRRELATION"
   if (BS_res_K_exchange)                      section_title=trim(section_title)//" EXCHANGE"
   if (BS_res_K_corr.or.BS_res_K_exchange)     section_title=trim(section_title)//")"
   !
   if (BS_cpl_K_corr.or.BS_cpl_K_exchange)     section_title=trim(section_title)//" (Coupling"
   if (BS_cpl_K_corr.or.(BS_K_is_ALDA.and.BS_cpl_K_exchange))&
&                                              section_title=trim(section_title)//" CORRRELATION"
   if (BS_cpl_K_exchange)                      section_title=trim(section_title)//" EXCHANGE"
   if (BS_cpl_K_corr.or.BS_cpl_K_exchange)     section_title=trim(section_title)//")"
   !
 endif
 !
 call section('=',trim(section_title))
 !
 ! Exchange
 !==========
 !
 load_O_X=BS_res_K_exchange.or.l_bs_fxc
 !
 if (load_O_X) call msg('r','[BSE] Exchange components :',BS_n_g_exch)
 !
 ! PP/Epsm1S DBs
 !
 W_is_em1s=X%whoami==2
 W_is_pp  =X%whoami==4
 !
 ! Oscillators Setup
 !===================
 O_ng=maxval(G_m_G)
 O_ng_shift=maxval(qindx_X(iq,:,2))
 !
 if (any((/BS_res_K_corr,BS_cpl_K_corr/)).and..not.l_bs_fxc) then
   call fft_check_size(1,O_ng,BS_n_g_exch)
   if(iq/=1) O_ng_shift=max(maxval(qindx_B(:,:,2)),O_ng_shift)
 endif
 !
 ! GPL_EXCLUDE_START
 !
 if (l_bs_fxc)      call section('=','Bethe-Salpeter Kernel -> xc-kernel')
 !
 ! BS/FXC DB I/O
 !===============
 !
 if (l_bs_fxc) then
   !
   !call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
   !
   !io_BS_Fxc_err=io_BS_Fxc(iq,W_bss,ID,X=X)
   !
 else
   !
   ! GPL_EXCLUDE_END
   !
#if defined _PAR_IO
#else
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
   io_BS_err=io_BS(iq,X,ID)
   !
   if (io_BS_err==0.and.l_bss) then
     call io_control(ACTION=OP_RD,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
     io_BS_err=io_BS(iq,X,ID)
     do i_block=1,n_BS_blks
       call io_control(ACTION=RD_CL_IF_END,COM=NONE,MODE=DUMP,SEC=(/i_block+1/),ID=ID)
       io_BS_err=io_BS(iq,X,ID)
     enddo
   endif
#endif
   !
   if (io_BS_err==0) goto 1
   !
   ! GPL_EXCLUDE_START
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! 
 if (BS_res_K_corr.and..not.l_td_hf) then
   !
   call section('+','Screened interaction header I/O')
   !===================================================
   !
   ! X%ng are the G's in the X DB while BS_n_g_W the G's I want to read.
   ! Now to read only BS_n_g_W components I need to redefine X%ng
   !
   X%ng=BS_n_g_W
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=XID)
   io_X_err=io_X(X,Xw,XID) 
   !
   if (io_X_err<0) then
     call deliver_IO_error_message(io_X_err,'PP/Em1s')
     call warning('Bethe Salpter section skipped. Impossible to build the kernel.')
     n_BS_blks=0
     call BS_blks_free()
     !
#if defined _PL
     call BSS_PL_free( )
#endif
#if defined _KERR
     call BSS_KERR_free( )
#endif
     !
   endif
   !
   ! X%ng is set to BS_n_g_W to VERIFY if there are enough bands.
   ! From now on it is used to keep track of the actual sixe of W in the DB
   !
   X%ng=X%ng_db
   !
   if (io_X_err<0) goto 1
   !
 endif
 !
 ! TDDFT xc-kernel Setup
 !======================
 if (l_bs_fxc) then
   !
   if (iq==1) call DIPOLE_driver(Ken,Xk,X,BSS_q0,iq)
   !
   ! Allocation
   !-----------
   YAMBO_ALLOC(FXC_K_diagonal,(BS_K_dim))
   YAMBO_ALLOC(FXC,(FXC_n_g_corr,FXC_n_g_corr,W_bss%n_freqs))
   FXC_K_diagonal=0._SP
   FXC=cZERO
   !
 endif
 !
 ! Wave Functions
 !================
 if (l_bs_fxc) then
   !
   ! GPL_EXCLUDE_START
   !
   call WF_load(WF,max(O_ng,BS_n_g_exch),O_ng_shift,BS_bands,(/1,Xk%nibz/),space='R',title='-BSK->Fxc')
   !
   ! GPL_EXCLUDE_END
   !
 else
   !
   bands_to_load=BS_bands
   if (BS_K_is_ALDA) bands_to_load=(/1,BS_bands(2)/)
   !
   call WF_load(WF,max(O_ng,BS_n_g_exch),O_ng_shift,bands_to_load,(/1,Xk%nibz/),space='R',title='-BSK')
   !
 endif
 !
 ! Elemental collisions allocation
 !=================================
 !
 call elemental_collision_alloc(K_CORR_collision,NG=O_ng,       TITLE="CORR_COLL")
 call elemental_collision_alloc(K_EXCH_collision,NG=BS_n_g_exch,TITLE="EXCH_COLL")
 !
 ! Wave Functions Phases
 !=======================
 call K_WF_phases(Xk)
 ! 
 ! Spatial Inversion Test
 !========================
 call WF_spatial_inversion(Ken,Xk)
 !
 ! ALDA
 !======
 if (BS_K_is_ALDA) then
   YAMBO_ALLOC(F_xc,(fft_size,n_spin,n_spin))
   call XC_potential_driver(Ken,Xk,WF_KIND,WF_xc_functional,2)
 endif
 !
 ! Screened interaction
 !======================
 !
 if (BS_res_K_corr) then
   !
   call elemental_collision_free(isc)
   !
   X%ng=BS_n_g_W
   if (.not.l_td_hf) then
     if (W_is_em1s) call X_alloc('X',(/BS_n_g_W,BS_n_g_W,1/))
     if (W_is_pp)   call X_alloc('X',(/BS_n_g_W,BS_n_g_W,2/))
   endif
   !
   i_g1=BS_n_g_W
   if (BS_W_is_diagonal) i_g1=1
   !
   ! When TR is present but not the SI X_mat indexes need to be exchanged 
   ! when the TR is applied
   !
   if (i_space_inv==0.and.i_time_rev==1.and..not.BS_W_is_diagonal) then
     YAMBO_ALLOC(BS_W,(BS_n_g_W,i_g1,2*q%nibz))
   else
     YAMBO_ALLOC(BS_W,(BS_n_g_W,i_g1,q%nibz))
   endif
   !
   YAMBO_ALLOC(isc%gamp,(i_g1,BS_n_g_W))
   !
   if (.not.l_td_hf) then
     call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=XID)
     io_X_err=io_X(X,Xw,XID)
   endif
   !
   do iq_W=1,q%nibz
     !
     isc%qs(2)=iq_W
     call scatter_Gamp(isc,'c')
     !
     if (.not.l_td_hf) then
       !
       call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iq_W,2*iq_W+1/),ID=XID)
       io_X_err=io_X(X,Xw,XID)
       !
       forall(i_g2=1:BS_n_g_W) X_mat(i_g2,i_g2,1)=X_mat(i_g2,i_g2,1)+1._SP
       if (BS_W_is_diagonal) then
         forall(i_g2=1:BS_n_g_W) BS_W(i_g2,1,iq_W)=real(X_mat(i_g2,i_g2,1))*isc%gamp(1,i_g2)
       else if (.not.BS_W_is_diagonal) then
         do i_g3=1,BS_n_g_W
           do i_g2=1,BS_n_g_W
             BS_W(i_g2,i_g3,iq_W)=X_mat(i_g2,i_g3,1)*isc%gamp(i_g2,i_g3)
             if (i_space_inv==0.and.i_time_rev==1) BS_W(i_g2,i_g3,q%nibz+iq_W)=X_mat(i_g3,i_g2,1)*isc%gamp(i_g2,i_g3)
           enddo
         enddo
      endif
      !
     else
       !
       forall(i_g2=1:BS_n_g_W) BS_W(i_g2,1,iq_W)=isc%gamp(1,i_g2)
       !
     endif
     ! 
   enddo
   !
   ! Anisotropy correction. Note that isc%gamp(1,1)=RIM_anisotropy while
   ! the \delta(G,G') term must be multiplied by the standard RIM_qpg(1,1)
   !
   if (RIM_anisotropy/=0.) BS_W(1,1,1)=BS_W(1,1,1)+RIM_qpg(1,1,1)/2.-RIM_anisotropy/2.
   !
   YAMBO_FREE(isc%gamp)
   !
   if (.not.l_td_hf) call X_alloc('X')
   !
   X%ng=X%ng_db
   call elemental_collision_free(isc)
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
#if defined _PAR_IO
 call io_control(ACTION=OP_WR,COM=REP,ID=ID)
 call io_BS_PAR_init(iq,X,ID)
#else
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
 io_BS_err=io_BS(iq,X,ID)
#endif
 !
 !call section('=','Kernel loop')
 !==============================
 !
 ! Allocations
 !-------------
 if (BS_res_K_corr.or.BS_cpl_K_corr) then
   YAMBO_ALLOC(Ovv,(BS_n_g_W))
   YAMBO_ALLOC(Occ,(BS_n_g_W))
   YAMBO_ALLOC(O_times_W,(BS_n_g_W))
   if (BS_cpl_K_corr) then
     YAMBO_ALLOC(Ovc,(BS_n_g_W))
     YAMBO_ALLOC(Ocv,(BS_n_g_W))
   endif
 endif
 !
 ! Timing
 !========
 !
 if (BS_LiveTiming_steps>0) then
   if (.not.l_bs_fxc) call live_timing('Kernel',     BS_LiveTiming_steps)
   if (     l_bs_fxc) call live_timing('Kernel->Fxc',BS_LiveTiming_steps)
 endif
 !
 do i_block=1,n_BS_blks
   !
   ! Read BS_blk to check if the transition was already computed
   !
#ifdef _PAR_IO
   if (l_restart_bse) then
     call io_BS_PAR_read_block(iq,i_block,ID)
     if (all(BS_blk(i_block)%done=="t")) cycle
   endif
#endif
   !
   i_Tgrp_k=BS_blk(i_block)%iT_k
   i_Tgrp_p=BS_blk(i_block)%iT_p
   !
   ! Exchange oscillators
   !----------------------
   ! 
   call K_exchange_collisions(iq,Xk,i_Tgrp_k)
   !
   if (i_Tgrp_k/=i_Tgrp_p) call K_exchange_collisions(iq,Xk,i_Tgrp_p)
   !
   ! Exchange oscillators
   !----------------------
   ! 
   call K_correlation_collisions(iq,i_block,Xk,q)
   !
   ! Transitions Loop
   !------------------
   !
   do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
     !
     sqrt_f_itk=sqrt(BS_T_grp(i_Tgrp_k)%f(i_Tk))
     !
     i_k_bz  = BS_T_grp(i_Tgrp_k)%table(i_Tk,1)  !k-point in full BZ
     i_v_k   = BS_T_grp(i_Tgrp_k)%table(i_Tk,2)  !v-index
     i_c_k   = BS_T_grp(i_Tgrp_k)%table(i_Tk,3)  !c-index
     i_k_spin= BS_T_grp(i_Tgrp_k)%table(i_Tk,4)
     !
     i_kmq_bz  = qindx_X(iq,i_k_bz,1)            !k-q-point in full BZ
     ig_kmq    = qindx_X(iq,i_k_bz,2)            !Go shift to reach the first BZ
     !
     i_k  = Xk%sstar(i_k_bz,1)                   !k-point in IBZ
     i_k_s= Xk%sstar(i_k_bz,2)
     !
     i_kmq  = Xk%sstar(i_kmq_bz,1)               !k-q-point in IBZ
     i_kmq_s= Xk%sstar(i_kmq_bz,2)
     !
     if (BS_res_K_corr) i_kmq_t= BS_blk(i_block)%kp_table(1,i_kmq)
     !
     if (BS_K_is_ALDA) then
       is=(/i_c_k,i_k,i_k_s,i_k_spin/)
       os=(/i_v_k,i_kmq,i_kmq_s,i_k_spin/)
     endif
     !
     i_k_s_m1   = sop_inv(i_k_s)
     i_kmq_s_m1 = sop_inv(i_kmq_s)
     !
     H_pos(1) = sum(BS_T_grp(:I_Tgrp_k-1)%size)+i_Tk
     !
     do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
       !
       ! skip this block if already done
       if (BS_blk(i_block)%done(i_Tk,i_Tp)=="t") cycle
       !
       sqrt_f_itp=sqrt(BS_T_grp(i_Tgrp_p)%f(i_Tp))
       !
       i_p_bz  = BS_T_grp(i_Tgrp_p)%table(i_Tp,1)
       i_v_p   = BS_T_grp(i_Tgrp_p)%table(i_Tp,2)
       i_c_p   = BS_T_grp(i_Tgrp_p)%table(i_Tp,3)
       i_p_spin= BS_T_grp(i_Tgrp_p)%table(i_Tp,4)
       !
       i_pmq_bz  = qindx_X(iq,i_p_bz,1)
       ig_pmq    = qindx_X(iq,i_p_bz,2)
       !
       i_p  =Xk%sstar(i_p_bz,1)
       i_p_s=Xk%sstar(i_p_bz,2)
       !
       i_pmq  =Xk%sstar(i_pmq_bz,1)
       i_pmq_s=Xk%sstar(i_pmq_bz,2)
       !
       if (BS_res_K_corr) i_pmq_t= BS_blk(i_block)%kp_table(2,i_pmq)
       !
       if (BS_K_is_ALDA) then
         isp=(/i_c_p,i_p,i_p_s,i_p_spin/)
         osp=(/i_v_p,i_pmq,i_pmq_s,i_p_spin/)
       endif
       !
       i_kp_s    = sop_tab(i_k_s_m1,i_p_s)
       i_kp_mq_s = sop_tab(i_kmq_s_m1,i_pmq_s)
       !
       !ig_kp_mq=G_m_G(ig_kmq,ig_pmq)
       !
       H_pos(2) = sum(BS_T_grp(:I_Tgrp_p-1)%size)+i_Tp
       !
       if (H_pos(1)>H_pos(2)) cycle
       !
       iq_bz_W=qindx_B(i_k_bz,i_p_bz,1) !q_W_bz+G_W=k_bz-p_bz
       ig_W   =qindx_B(i_k_bz,i_p_bz,2)
       !
       iq_W   =q%sstar( iq_bz_W,1)
       iq_W_s =q%sstar( iq_bz_W,2)
       !
       iq_bz_W_mq=qindx_B(i_kmq_bz,i_pmq_bz,1) !q_W_mq_bz+G_W_mq=kmq_bz-pmq_bz
       ig_W_mq   =qindx_B(i_kmq_bz,i_pmq_bz,2)
       !
       iq_W_mq   =q%sstar( iq_bz_W_mq,1)
       iq_W_s_mq =q%sstar( iq_bz_W_mq,2)
       !
       if (iq_bz_W/=iq_bz_W_mq.or.iq_W/=iq_W_mq.or.iq_W_s/=iq_W_s_mq) call error("Wrong transferred momentum")
       !
       !ig_kp_W_mq=ig_W_mq
       !ig_kp_W_mq=G_m_G(ig_W,ig_kp_mq)
       !
       H_x=cZERO
       H_c=cZERO
       !
       if (BS_blk(i_block)%mode=="R") then
         if (BS_res_K_exchange) then
           !
           ! :::EXCHANGE    (resonant):::
           !=============================
           H_x=Vstar_dot_V_omp(BS_n_g_exch, BS_T_grp(i_Tgrp_p)%O_x(:,i_Tp),&
&                                           BS_T_grp(i_Tgrp_k)%O_x(:,i_Tk)/bare_qpg(iq,:BS_n_g_exch)**2)
         endif
       else 
         if (BS_cpl_K_exchange) then
           !
           ! :::EXCHANGE    (coupling):::
           !=============================
           H_x=V_dot_V_omp(BS_n_g_exch, BS_T_grp(i_Tgrp_p)%O_x( minus_G(1:BS_n_g_exch),i_Tp),&
&                                       BS_T_grp(i_Tgrp_k)%O_x(:,i_Tk)/bare_qpg(iq,:BS_n_g_exch)**2)
         endif
       endif
       !
       if (BS_K_is_ALDA) then
         !
         if (BS_blk(i_block)%mode=="R") then
           !
           ! :::ALDA        (resonant):::
           !=============================
           H_x=H_x+TDDFT_ALDA_R_space(is,os,isp,osp,'RES')
           !
         else
           !
           ! :::ALDA        (coupling):::
           !=============================
           H_x=H_x+TDDFT_ALDA_R_space(is,os,isp,osp,'CPL')
           !
         endif
         !
       endif
       !
       if (BS_blk(i_block)%mode=="R") then
         !
         if (BS_res_K_corr.and.i_k_spin==i_p_spin) then
           !
           ! :::SEX         (resonant):::
           !=============================
           !
           iOcc        = BS_blk(i_block)%O_table(1,i_kp_s,1,1,i_c_k,i_c_p,i_k_spin)
           i_block_cc  = BS_blk(i_block)%O_table(2,i_kp_s,1,1,i_c_k,i_c_p,i_k_spin)
           PHASE_1=WF_phase(i_p,i_kp_s,i_c_p,i_p_spin)
           if (PHASE_1==cZERO  ) PHASE_1=1._SP
           if (PHASE_1==-99._SP) PHASE_1=1._SP
           !
           iOvv       = BS_blk(i_block)%O_table(1,i_kp_mq_s,i_kmq_t,i_pmq_t,i_v_k,i_v_p,i_k_spin)
           i_block_vv = BS_blk(i_block)%O_table(2,i_kp_mq_s,i_kmq_t,i_pmq_t,i_v_k,i_v_p,i_k_spin)
           PHASE_2=WF_phase(i_pmq,i_kp_mq_s,i_v_p,i_p_spin)
           if (PHASE_2==cZERO  ) PHASE_2=1._SP
           if (PHASE_2==-99._SP) PHASE_2=1._SP
           !
           !$omp parallel default(shared), private(i_g1,i_g2,i_g3)
           !$omp do
           do i_g1=1,BS_n_g_W
             i_g2=g_rot(G_m_G(g_rot(i_g1,iq_W_s)   ,ig_W   ),i_k_s_m1  )
             i_g3=g_rot(G_m_G(g_rot(i_g1,iq_W_s_mq),ig_W_mq),i_kmq_s_m1)
             Occ(i_g1)=BS_blk(i_block_cc)%O_c(i_g2,iOcc)*PHASE_1
             Ovv(i_g1)=BS_blk(i_block_vv)%O_c(i_g3,iOvv)*PHASE_2
           enddo
           !$omp end do
           !$omp end parallel
           !
           if (i_k_s  >nsym/(i_time_rev+1)) Occ=conjg(Occ)
           if (i_kmq_s>nsym/(i_time_rev+1)) Ovv=conjg(Ovv)
           !
           if (BS_W_is_diagonal) then
             !
             !$omp parallel do default(shared), private(i_g1)
             do i_g1=1,BS_n_g_W
               O_times_W(i_g1)=Occ(i_g1)*BS_W(i_g1,1,iq_W)
             enddo
             !$omp end parallel do
             !
           else
             !
             if (iq_W_s>nsym/(i_time_rev+1) .and. i_space_inv == 0 ) iq_W=q%nibz+iq_W
             !
             call M_by_V_omp('T',BS_n_g_W,BS_W(:,:,iq_W),Occ(:),O_times_W)
             !
           endif
           !
           H_c=Vstar_dot_V_omp(BS_n_g_W,Ovv,O_times_W)*4._SP*pi
           !
         endif
         !
       else if (BS_blk(i_block)%mode=="C") then
         !
         if (BS_cpl_K_corr.and.i_k_spin==i_p_spin) then
           !
           ! :::SEX         (coupling):::
           !=============================
           !
           iOcv        = BS_blk(i_block)%O_table(1,i_kp_s,1,1,i_c_k,i_v_p,i_k_spin)
           i_block_cv  = BS_blk(i_block)%O_table(2,i_kp_s,1,1,i_c_k,i_v_p,i_k_spin)
           PHASE_1=WF_phase(i_p,i_kp_s,i_v_p,i_p_spin)
           if (PHASE_1==cZERO  ) PHASE_1=1._SP
           if (PHASE_1==-99._SP) PHASE_1=1._SP
           !
           iOvc       = BS_blk(i_block)%O_table(1,i_kp_mq_s,i_kmq_t,i_pmq_t,i_v_k,i_c_p,i_k_spin)
           i_block_vc = BS_blk(i_block)%O_table(2,i_kp_mq_s,i_kmq_t,i_pmq_t,i_v_k,i_c_p,i_k_spin)
           PHASE_2=WF_phase(i_pmq,i_kp_mq_s,i_c_p,i_p_spin)
           if (PHASE_2==cZERO  ) PHASE_2=1._SP
           if (PHASE_2==-99._SP) PHASE_2=1._SP
           !
           !$omp parallel default(shared), private(i_g1,i_g2,i_g3)
           !$omp do
           do i_g1=1,BS_n_g_W
             i_g2=g_rot(G_m_G(g_rot(i_g1,iq_W_s)   ,ig_W   ),i_k_s_m1  )
             i_g3=g_rot(G_m_G(g_rot(i_g1,iq_W_s_mq),ig_W_mq),i_kmq_s_m1)
             Ocv(i_g1)=BS_blk(i_block_cv)%O_c(i_g2,iOcv)*PHASE_1
             Ovc(i_g1)=BS_blk(i_block_vc)%O_c(i_g3,iOvc)*PHASE_2
           enddo
           !$omp end do
           !$omp end parallel
           !
           if (i_k_s  >nsym/(i_time_rev+1)) Ocv=conjg(Ocv)
           if (i_kmq_s>nsym/(i_time_rev+1)) Ovc=conjg(Ovc)
           !
           if (BS_W_is_diagonal) then
             !
             !$omp parallel do default(shared), private(i_g1)
             do i_g1=1,BS_n_g_W
               O_times_W(i_g1)=Ocv(i_g1)*BS_W(i_g1,1,iq_W)
             enddo
             !$omp end parallel do
             !
           else
             !
             if (iq_W_s>nsym/(i_time_rev+1) .and. i_space_inv == 0 ) iq_W=q%nibz+iq_W
             !
             call M_by_V_omp('T',BS_n_g_W,BS_W(:,:,iq_W),Ocv(:),O_times_W)
             !
           endif
           !
           H_c=Vstar_dot_V_omp(BS_n_g_W,Ovc,O_times_W)*4._SP*pi
           !
         endif
         !
       endif
       !
       ! We have to add a new mode in addition to resonant and coupling for finiteq, where the kernel is recomputed in the antiresonant case
       if (BS_blk(i_block)%mode=="R") BS_blk(i_block)%mat(i_Tk,i_Tp)=   sqrt_f_itk*(H_x*Co-H_c)*sqrt_f_itp
       if (BS_blk(i_block)%mode=="C") BS_blk(i_block)%mat(i_Tk,i_Tp)=cI*sqrt_f_itk*(H_x*Co-H_c)*sqrt_f_itp
       !
       BS_blk(i_block)%done(i_Tk,i_Tp)="t"
       !
     enddo 
     !
   enddo
   !
   call BS_oscillators_free(0,i_block)
   if(l_BSE_minimize_memory) then
     call BS_oscillators_free(i_Tgrp_p,0)
     call BS_oscillators_free(i_Tgrp_k,0)
   endif
   !
#if defined _PAR_IO
   call io_BS_PAR_write_block(iq,i_block,ID)
#else
   call io_control(ACTION=OP_APP_WR_CL,COM=REP,SEC=(/i_block+1/),ID=ID)
   io_BS_err=io_BS(iq,X,ID)
#endif
   !
   call live_timing(steps=BS_Block_size(i_block))
   !
 enddo
 !
#if defined _PAR_IO
 call  io_BS_PAR_free(ID)
#endif
 !
 if (BS_LiveTiming_steps>0) call live_timing( )
 !
 BS_K_has_been_calculated_loaded=.TRUE.
 !
 ! GLOBAL CLEANING 
 !=================
 !
 call BS_oscillators_free(BS_nT_grps,0)
 !
 if (BS_res_K_corr.or.BS_cpl_K_corr) then
   YAMBO_FREE(WF_phase)
   YAMBO_FREE(Ovv)
   YAMBO_FREE(O_times_W)
   YAMBO_FREE(Occ)
   if (BS_cpl_K_corr) then
     YAMBO_FREE(Ovc)
     YAMBO_FREE(Ocv)
   endif
 endif
 !
 if (l_bs_fxc) then
   !
   ! GPL_EXCLUDE_START
   !
   YAMBO_FREE(FXC)
   YAMBO_FREE(FXC_K_diagonal)
   !
   ! GPL_EXCLUDE_END
   !
 else if (BS_K_is_ALDA) then
   YAMBO_FREE(F_xc)
 endif
 !
1 continue
 !
 YAMBO_FREE(BS_W)
 call WF_free(WF)
 call elemental_collision_free(K_CORR_collision)
 call elemental_collision_free(K_EXCH_collision)
 !
 call timing('X T_space Kernel',OPR='stop')
 !
end subroutine
