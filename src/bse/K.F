!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use stderr,       ONLY:intc
 use LOGO,         ONLY:pickup_a_random
 use drivers,      ONLY:l_bs_fxc,l_td_hf,l_bss
 use frequency,    ONLY:w_samp
 use electrons,    ONLY:levels,spin_occ,spin,n_spin
 use FFT_m,        ONLY:fft_size
 use interfaces,   ONLY:WF_load,WF_free
 use functions,    ONLY:K_scatter
 use D_lattice,    ONLY:nsym,DL_vol,i_time_rev,sop_tab,sop_inv,i_space_inv
 use R_lattice,    ONLY:G_m_G,g_rot,qindx_B,bz_samp,RIM_anisotropy,RIM_qpg
#ifdef _CUDA
 use R_lattice,    ONLY:G_m_G_d,g_rot_d
#endif
 use com,          ONLY:warning,error,msg
 use LIVE_t,       ONLY:live_timing
 use X_m,          ONLY:X_alloc,X_t,X_mat
#ifdef _CUDA
 use X_m,          ONLY:X_mat_d
#endif
 use BS_solvers,   ONLY:BSS_q0
 use BS,           ONLY:BS_bands,BS_res_K_corr,BS_W_is_diagonal,&
&                       BS_res_K_exchange,BS_n_g_W,BS_Block_size,&
&                       O_ng,BS_n_g_exch,BS_identifier,BS_LiveTiming_steps,&
&                       BS_K_dim,BS_K_is_ALDA,BS_cpl_K_exchange,&
&                       BS_cpl_K_corr,K_EXCH_collision,K_CORR_collision,&
&                       BS_oscillators_free,WF_phase,n_BS_blks,BS_blk,BS_T_grp,&
&                       BS_K_has_been_calculated_loaded,BS_nT_grps,BS_blks_free,&
&                       l_BSE_minimize_memory, l_restart_bse
 use collision_el, ONLY:elemental_collision,elemental_collision_free
 use IO_m,         ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,OP_RD,&
&                       RD,WR,RD_CL,RD_CL_IF_END,OP_WR_CL,WR_CL,OP_WR,OP_APP_CL,DUMP,&
&                       deliver_IO_error_message
 use TDDFT,         ONLY:FXC_K_diagonal,FXC,FXC_n_g_corr,io_BS_Fxc
 use xc_functionals,ONLY:F_xc,XC_potential_driver
 use wrapper,       ONLY:V_dot_V,Vstar_dot_V,V_by_V_pwise,M_by_V
 use dev_util_m,    ONLY:dev_conjg,dev_memcpy
 use global_XC,     ONLY:WF_kind,WF_xc_functional
 use wave_func,     ONLY:WF
 use openmp,        ONLY:OPENMP_update,master_thread,n_threads_K
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
 integer    :: i_block,i_block_vv,i_block_cc,i_block_vc,i_block_cv,i_k_bz,i_p_bz,i_c_k,i_v_k,i_c_p,i_v_p,i_k_s,i_p_s,&
&              i_Tgrp_k,i_Tgrp_p,i_Tp,i_Tk,i_k_spin,i_p_spin,i_k,i_p,H_pos(2),i_k_s_m1,&
&              i_k_p_s,i_g1,ig_W,iq_W,iq_W_s,i_g2,i_g3,nibz,&
&              bands_to_load(2),iOvv,iOcc,iOvc,iOcv,is(4),os(4),isp(4),osp(4)
 complex(SP):: Co,H_x,H_c,PHASE_1,PHASE_2
 real(SP)   :: sqrt_f_itk,sqrt_f_itp,rtmp
 !
 complex(SP),allocatable ::BS_W(:,:,:)
#ifdef _CUDA
 attributes(device) :: BS_W
#endif
 !
 complex(SP),   external ::TDDFT_ALDA_R_space
 logical                 ::W_is_em1s,W_is_pp
 !
 ! Oscillators
 !
 logical :: load_O_X
 type(elemental_collision), target ::isc
 complex(SP),allocatable  ::Ovv(:),Occ(:),O_times_W(:)
 complex(SP),allocatable  ::Ovc(:),Ocv(:)
#ifdef _CUDA
 attributes(device)   :: Ovv,Occ,O_times_W
 attributes(device)   :: Ovc,Ocv
 complex(SP), pointer, device :: O_c_d_iocc(:),O_c_d_iocv(:)
 complex(SP), pointer, device :: O_c_d_iovc(:),O_c_d_iovv(:)
 complex(SP), pointer, device :: gamp_d(:,:)
#endif
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
   section_title="Bethe-Salpeter Kernel -> xc-kernel"
 else
   if (BS_K_is_ALDA) then
     section_title="TDDFT Kernel"
   else
     section_title="Bethe-Salpeter"
   endif
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
 !
 if (any((/BS_res_K_corr,BS_cpl_K_corr/)).and..not.l_bs_fxc) call fft_check_size(1,O_ng,BS_n_g_exch,"Kx")
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
   !
   ! TO DO
   !
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
   if (io_BS_err==0) return
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
2  if (io_X_err<0) then
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
   !if (iq==1) here put dipoles I/O
   if (iq==1) stop
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
   call WF_load(WF,max(O_ng,BS_n_g_exch),1,BS_bands,(/1,Xk%nibz/),space='R',title='-BSK->Fxc')
   !
   ! GPL_EXCLUDE_END
   !
 else
   !
   bands_to_load=BS_bands
   if (BS_K_is_ALDA) bands_to_load=(/1,BS_bands(2)/)
   !
   call WF_load(WF,max(O_ng,BS_n_g_exch),1,bands_to_load,(/1,Xk%nibz/),space='R',title='-BSK')
   !
 endif
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
#ifdef _CUDA
   allocate(isc%gamp_d, mold=isc%gamp)
#endif
   !
   if (.not.l_td_hf) then
     call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=XID)
     io_X_err=io_X(X,Xw,XID)
     if (io_X_err<0) then
       call warning('Reading of io_X heared failed depite previous check.')
       goto 2
     endif
   endif
   !
   do iq_W=1,q%nibz
     !
     isc%qs(2)=iq_W
#ifdef _CUDA
     call scatter_Gamp_gpu(isc,'c')
#else
     call scatter_Gamp(isc,'c')
#endif
     !
     if (.not.l_td_hf) then
       !
       call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iq_W,2*iq_W+1/),ID=XID)
       io_X_err=io_X(X,Xw,XID)
       if (io_X_err<0) then
         call warning('Reading of io_X for iq='//trim(intc(iq_W))//' failed depite previous check.')
         goto 2
       endif
       !
       forall(i_g2=1:BS_n_g_W) X_mat(i_g2,i_g2,1)=X_mat(i_g2,i_g2,1)+1._SP
       !
#ifdef _CUDA
       ! host->device transfer
       X_mat_d=X_mat
       !
       ! CUDA Fortran workarounds
       gamp_d => isc%gamp_d
       nibz=q%nibz
       !
       if (BS_W_is_diagonal) then
         !$cuf kernel do(1) <<<*,*>>>
         do i_g2=1, BS_n_g_W
            BS_W(i_g2,1,iq_W)=real(X_mat_d(i_g2,i_g2,1))*gamp_d(i_g2,1)
         enddo
       else if (.not.BS_W_is_diagonal) then
         !$cuf kernel do(2) <<<*,*>>>
         do i_g3=1,BS_n_g_W
           do i_g2=1,BS_n_g_W
             BS_W(i_g2,i_g3,iq_W)=X_mat_d(i_g2,i_g3,1)*gamp_d(i_g2,i_g3)
             if (i_space_inv==0.and.i_time_rev==1) BS_W(i_g2,i_g3,nibz+iq_W)=X_mat_d(i_g3,i_g2,1)*gamp_d(i_g2,i_g3)
           enddo
         enddo
       endif
#else
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
#endif
       !
     else
       !
#ifdef _CUDA
       call dev_memcpy(BS_W(:,1,iq_W),isc%gamp_d(:,1),(/1,BS_n_g_W/))
#else
       BS_W(1:BS_n_g_W,1,iq_W)=isc%gamp(1:BS_n_g_W,1)
#endif
       !
     endif
     ! 
   enddo
   !
   ! Anisotropy correction. Note that isc%gamp(1,1)=RIM_anisotropy while
   ! the \delta(G,G') term must be multiplied by the standard RIM_qpg(1,1)
   !
#ifdef _CUDA
   if (RIM_anisotropy/=0.) then
      rtmp=BS_W(1,1,1)
      rtmp=rtmp+RIM_qpg(1,1,1)/2._SP-RIM_anisotropy/2._SP
      BS_W(1,1,1)=rtmp
   endif
#else
   if (RIM_anisotropy/=0.) BS_W(1,1,1)=BS_W(1,1,1)+RIM_qpg(1,1,1)/2._SP-RIM_anisotropy/2._SP
#endif
   !
   call elemental_collision_free(isc)
   !
   if (.not.l_td_hf) call X_alloc('X')
   X%ng=X%ng_db
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
 call io_control(ACTION=OP_RD_CL,ID=ID)
 call io_BS_PAR_check(iq,ID)
 !
 call io_control(ACTION=OP_APP_CL,COM=REP,ID=ID)
 call io_BS_PAR_init(iq,X,ID)
#else
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
 io_BS_err=io_BS(iq,X,ID)
#endif
 !
 call section('=','Kernel loop')
 !==============================
 !
 ! Timing
 !========
 !
 if (BS_LiveTiming_steps>0) then
   if (.not.l_bs_fxc) call live_timing('Kernel',     BS_LiveTiming_steps)
   if (     l_bs_fxc) call live_timing('Kernel->Fxc',BS_LiveTiming_steps)
 endif
 !
 !------------------
 ! LOOP over blocks
 !------------------
 !
 block_loop:&
 do i_block=1,n_BS_blks
   !
   ! Read BS_blk to check if the transition was already computed
   !
#ifdef _PAR_IO
   if (l_restart_bse) then
     call io_control(ACTION=RD_CL,ID=ID)
     call io_BS_PAR_block(iq,i_block,ID)
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
   call K_correlation_collisions(i_block,Xk,q)
   !
   ! Transition Loops
   !------------------
   !
#if !defined _CUDA
   !$omp parallel default(shared), &
   !$omp private(i_Tk,i_Tp, &
   !$omp &       sqrt_f_itk,i_k_bz,i_v_k,i_c_k,i_k_spin,i_k,i_k_s,is,os,i_k_s_m1,H_pos, &
   !$omp &       sqrt_f_itp,i_p_bz,i_v_p,i_c_p,i_p_spin,i_p,i_p_s,isp,osp,i_k_p_s, &
   !$omp &       ig_W,iq_W,iq_W_s,H_x,H_c, &
   !$omp &       iOcc,i_block_cc,PHASE_1, iOcv, i_block_cv, iOvc, i_block_vc, &
   !$omp &       iOvv,i_block_vv,PHASE_2, i_g1,i_g2, Ovv,Occ,O_times_W,Ovc,Ocv)
#endif
   !
   call OPENMP_update(master_thread)   
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
#if !defined _CUDA
   !$omp do collapse(2), schedule(dynamic)
#endif
   do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
     do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
       !
       ! the following definitions are placed here because of
       ! omp parallelism (should not trigger any performance issue)
       !
       sqrt_f_itk=sqrt(BS_T_grp(i_Tgrp_k)%f(i_Tk))
       !
       i_k_bz  = BS_T_grp(i_Tgrp_k)%table(i_Tk,1)
       i_v_k   = BS_T_grp(i_Tgrp_k)%table(i_Tk,2)
       i_c_k   = BS_T_grp(i_Tgrp_k)%table(i_Tk,3)
       i_k_spin= BS_T_grp(i_Tgrp_k)%table(i_Tk,4)
       !
       i_k  = Xk%sstar(i_k_bz,1)
       i_k_s= Xk%sstar(i_k_bz,2)
       !
       if (BS_K_is_ALDA) then
         is=(/i_c_k,i_k,i_k_s,i_k_spin/)
         os=(/i_v_k,i_k,i_k_s,i_k_spin/)
       endif
       !
       i_k_s_m1 = sop_inv(i_k_s)
       !
       H_pos(1) = sum(BS_T_grp(:I_Tgrp_k-1)%size)+i_Tk
       !
       !-----
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
       i_p  =Xk%sstar(i_p_bz,1)
       i_p_s=Xk%sstar(i_p_bz,2)
       !
       if (BS_K_is_ALDA) then
         isp=(/i_c_p,i_p,i_p_s,i_p_spin/)
         osp=(/i_v_p,i_p,i_p_s,i_p_spin/)
       endif
       !
       i_k_p_s  = sop_tab(i_k_s_m1,i_p_s)
       !
       H_pos(2) = sum(BS_T_grp(:I_Tgrp_p-1)%size)+i_Tp
       !
       !-----
       !
       if (H_pos(1)>H_pos(2)) cycle
       !
       ig_W   =qindx_B(i_k_bz,i_p_bz,2)
       iq_W   =q%sstar( qindx_B(i_k_bz,i_p_bz,1) ,1)
       iq_W_s =q%sstar( qindx_B(i_k_bz,i_p_bz,1) ,2)
       !
       H_x=cZERO
       H_c=cZERO
       !
       if (BS_blk(i_block)%mode=="R") then
         if (BS_res_K_exchange) then
           !
           ! :::EXCHANGE    (resonant):::
           !=============================
           !H_x=Vstar_dot_V_omp(BS_n_g_exch, BS_T_grp(i_Tgrp_p)%O_x(:,i_Tp),&
           !                                 BS_T_grp(i_Tgrp_k)%O_x(:,i_Tk)/bare_qpg(iq,:BS_n_g_exch)**2)
           !
           H_x=eval_bseK_exchange_resonant(iq, BS_n_g_exch, BS_T_grp(i_Tgrp_p),i_Tp, BS_T_grp(i_Tgrp_k),i_Tk)
           !
         endif
       else 
         if (BS_cpl_K_exchange) then
           !
           ! :::EXCHANGE    (coupling):::
           !=============================
           !H_x=V_dot_V_omp(BS_n_g_exch, BS_T_grp(i_Tgrp_p)%O_x( minus_G(1:BS_n_g_exch),i_Tp),&
           !                             BS_T_grp(i_Tgrp_k)%O_x(:,i_Tk)/bare_qpg(iq,:BS_n_g_exch)**2)
           !
           H_x=eval_bseK_exchange_coupling(iq, BS_n_g_exch, BS_T_grp(i_Tgrp_p),i_Tp, BS_T_grp(i_Tgrp_k),i_Tk) 
           !
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
           iOcc        = BS_blk(i_block)%O_table(1,i_k_p_s,i_c_k,i_c_p,i_k_spin)
           i_block_cc  = BS_blk(i_block)%O_table(2,i_k_p_s,i_c_k,i_c_p,i_k_spin)
           PHASE_1=WF_phase(i_p,i_k_p_s,i_c_p,i_p_spin)
           if (PHASE_1==cZERO  ) PHASE_1=1._SP
           if (PHASE_1==-99._SP) PHASE_1=1._SP
           !
           iOvv       = BS_blk(i_block)%O_table(1,i_k_p_s,i_v_k,i_v_p,i_k_spin)
           i_block_vv = BS_blk(i_block)%O_table(2,i_k_p_s,i_v_k,i_v_p,i_k_spin)
           PHASE_2=WF_phase(i_p,i_k_p_s,i_v_p,i_p_spin)
           if (PHASE_2==cZERO  ) PHASE_2=1._SP
           if (PHASE_2==-99._SP) PHASE_2=1._SP
           !
#ifdef _CUDA
           O_c_d_iocc => BS_blk(i_block_cc)%O_c_d(:,iOcc)
           O_c_d_iovv => BS_blk(i_block_vv)%O_c_d(:,iOvv)
           !$cuf kernel do(1) <<<*,*>>>
           do i_g1=1,BS_n_g_W
             i_g2=g_rot_d(G_m_G_d(g_rot_d(i_g1,iq_W_s),ig_W),i_k_s_m1)
             Occ(i_g1)=O_c_d_iocc(i_g2)*PHASE_1
             Ovv(i_g1)=O_c_d_iovv(i_g2)*PHASE_2
           enddo
#else
           !$omp parallel do default(shared), private(i_g1,i_g2)
           do i_g1=1,BS_n_g_W
             i_g2=g_rot(G_m_G(g_rot(i_g1,iq_W_s),ig_W),i_k_s_m1)
             Occ(i_g1)=BS_blk(i_block_cc)%O_c(i_g2,iOcc)*PHASE_1
             Ovv(i_g1)=BS_blk(i_block_vv)%O_c(i_g2,iOvv)*PHASE_2
           enddo
           !$omp end parallel do
#endif
           !
           if (i_k_s>nsym/(i_time_rev+1)) call dev_conjg(Ovv,(/1,BS_n_g_W/))
           if (i_k_s>nsym/(i_time_rev+1)) call dev_conjg(Occ,(/1,BS_n_g_W/))
           !
           if (BS_W_is_diagonal) then
             !
             !do i_g1=1,BS_n_g_W
             !  O_times_W(i_g1)=Occ(i_g1)*BS_W(i_g1,1,iq_W)
             !enddo
             !
             call V_by_V_pwise(BS_n_g_W,O_times_W,Occ,BS_W(:,1,iq_W))
             !
           else
             !
             if (iq_W_s>nsym/(i_time_rev+1) .and. i_space_inv == 0 ) iq_W=q%nibz+iq_W
             !
             call M_by_V('T',BS_n_g_W,BS_W(:,:,iq_W),Occ(:),O_times_W)
             !
           endif
           !
           H_c=Vstar_dot_V(BS_n_g_W,Ovv,O_times_W)*4._SP*pi
           !
         endif
         !
       else
         !
         if (BS_cpl_K_corr.and.i_k_spin==i_p_spin) then
           !
           ! :::SEX         (coupling):::
           !=============================
           !
           iOcv        = BS_blk(i_block)%O_table(1,i_k_p_s,i_c_k,i_v_p,i_k_spin)
           i_block_cv  = BS_blk(i_block)%O_table(2,i_k_p_s,i_c_k,i_v_p,i_k_spin)
           PHASE_1=WF_phase(i_p,i_k_p_s,i_v_p,i_p_spin)
           if (PHASE_1==cZERO  ) PHASE_1=1._SP
           if (PHASE_1==-99._SP) PHASE_1=1._SP
           !
           iOvc       = BS_blk(i_block)%O_table(1,i_k_p_s,i_v_k,i_c_p,i_k_spin)
           i_block_vc = BS_blk(i_block)%O_table(2,i_k_p_s,i_v_k,i_c_p,i_k_spin)
           PHASE_2=WF_phase(i_p,i_k_p_s,i_c_p,i_p_spin)
           if (PHASE_2==cZERO  ) PHASE_2=1._SP
           if (PHASE_2==-99._SP) PHASE_2=1._SP
           !
#ifdef _CUDA
           O_c_d_iocv => BS_blk(i_block_cv)%O_c_d(:,iOcv)
           O_c_d_iovc => BS_blk(i_block_vc)%O_c_d(:,iOvc)
           !$cuf kernel do (1) <<<*,*>>>
           do i_g1=1,BS_n_g_W
             i_g2=g_rot_d(G_m_G_d(g_rot_d(i_g1,iq_W_s),ig_W),i_k_s_m1)
             Ocv(i_g1)=O_c_d_iocv(i_g2)*PHASE_1
             Ovc(i_g1)=O_c_d_iovc(i_g2)*PHASE_2
           enddo
#else
           !$omp parallel do default(shared), private(i_g1,i_g2)
           do i_g1=1,BS_n_g_W
             i_g2=g_rot(G_m_G(g_rot(i_g1,iq_W_s),ig_W),i_k_s_m1)
             Ocv(i_g1)=BS_blk(i_block_cv)%O_c(i_g2,iOcv)*PHASE_1
             Ovc(i_g1)=BS_blk(i_block_vc)%O_c(i_g2,iOvc)*PHASE_2
           enddo
           !$omp end parallel do
#endif
           !
           if (i_k_s>nsym/(i_time_rev+1)) call dev_conjg(Ovc,(/1,BS_n_g_W/))
           if (i_k_s>nsym/(i_time_rev+1)) call dev_conjg(Ocv,(/1,BS_n_g_W/))
           !
           if (BS_W_is_diagonal) then
             !
             !do i_g1=1,BS_n_g_W
             !  O_times_W(i_g1)=Ocv(i_g1)*BS_W(i_g1,1,iq_W)
             !enddo
             !
             call V_by_V_pwise(BS_n_g_W,O_times_W,Ocv,BS_W(:,1,iq_W))
             !
           else
             !
             if (iq_W_s>nsym/(i_time_rev+1) .and. i_space_inv == 0 ) iq_W=q%nibz+iq_W
             !
             call M_by_V('T',BS_n_g_W,BS_W(:,:,iq_W),Ocv(:),O_times_W)
             !
           endif
           !
           H_c=Vstar_dot_V(BS_n_g_W,Ovc,O_times_W)*4._SP*pi
           !
         endif
         !
       endif
       !
       if (BS_blk(i_block)%mode=="R") BS_blk(i_block)%mat(i_Tk,i_Tp)=   sqrt_f_itk*(H_x*Co-H_c)*sqrt_f_itp
       if (BS_blk(i_block)%mode=="A") BS_blk(i_block)%mat(i_Tk,i_Tp)=cI*sqrt_f_itk*(H_x*Co-H_c)*sqrt_f_itp
       BS_blk(i_block)%done(i_Tk,i_Tp)="t"
       !
     enddo   ! i_Tk
   enddo     ! i_Tp
#if ! defined _CUDA
   !$omp end do
#endif
   !
   if (BS_res_K_corr.or.BS_cpl_K_corr) then
     YAMBO_FREE(Ovv)
     YAMBO_FREE(Occ)
     YAMBO_FREE(O_times_W)
     if (BS_cpl_K_corr) then
       YAMBO_FREE(Ovc)
       YAMBO_FREE(Ocv)
     endif
   endif
   !
#if ! defined _CUDA
   !$omp end parallel
#endif
   !
   call BS_oscillators_free(0,i_block)
   if(l_BSE_minimize_memory) then
     call BS_oscillators_free(i_Tgrp_p,0)
     call BS_oscillators_free(i_Tgrp_k,0)
   endif
   !
#if defined _PAR_IO
   call io_control(ACTION=WR_CL,ID=ID)
   call io_BS_PAR_block(iq,i_block,ID)
#else
   call io_control(ACTION=OP_APP_CL,COM=REP,SEC=(/i_block+1/),ID=ID)
   io_BS_err=io_BS(iq,X,ID)
#endif
   !
   call live_timing(steps=BS_Block_size(i_block))
   !
 enddo block_loop
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
 !
 call timing('X T_space Kernel',OPR='stop')

contains

!==============================
function eval_bseK_exchange_resonant(iq, BS_n_g_exch, BS_T_grp_ip, i_Tp, &
&                                                     BS_T_grp_ik, i_Tk) result(H_x)
  !==============================
  !
  ! :::EXCHANGE    (resonant):::
  !=============================
  ! H_x=Vstar_dot_V_omp(BS_n_g_exch, BS_T_grp(i_Tgrp_p)%O_x(:,i_Tp),&
  !                                  BS_T_grp(i_Tgrp_k)%O_x(:,i_Tk)/bare_qpg(iq,:BS_n_g_exch)**2)
  !
  use pars,         ONLY: SP,rZERO
  use BS,           ONLY: BS_T_group
  use wrapper_omp,  ONLY: Vstar_dot_V_omp
  use R_lattice,    ONLY: bare_qpg
#ifdef _CUDA
  use R_lattice,    ONLY: bare_qpg_d
#endif
  implicit none
  !
  integer,                  intent(in) :: iq,BS_n_g_exch,i_Tp,i_Tk
  type(BS_T_group), target, intent(in) :: BS_T_grp_ip,BS_T_grp_ik
  complex(SP) :: H_x
  !
#ifdef _CUDA
  complex(SP), pointer, device :: BS_T_grp_ip_O_x_d(:,:)
  complex(SP), pointer, device :: BS_T_grp_ik_O_x_d(:,:)
  integer :: ig
  real(SP):: H_x_r,H_x_i
#endif

#ifdef _CUDA
  !
  BS_T_grp_ip_O_x_d => BS_T_grp_ip%O_x_d
  BS_T_grp_ik_O_x_d => BS_T_grp_ik%O_x_d
  !
  H_x_r=rZERO
  H_x_i=rZERO
  !$cuf kernel do(1) <<<*,*>>>
  do ig = 1, BS_n_g_exch
    H_x=conjg(BS_T_grp_ip_O_x_d(ig,i_Tp))*BS_T_grp_ik_O_x_d(ig,i_Tk)/bare_qpg_d(iq,ig)**2
    H_x_r=H_x_r+real(H_x,SP)
    H_x_i=H_x_i+aimag(H_x)
  enddo
  H_x=cmplx(H_x_r,H_x_i,SP)
  !
#else
  H_x=Vstar_dot_V_omp(BS_n_g_exch, BS_T_grp_ip%O_x(:,i_Tp),&
                                   BS_T_grp_ik%O_x(:,i_Tk)/bare_qpg(iq,:BS_n_g_exch)**2)
#endif
  !
end function eval_bseK_exchange_resonant

!==============================
function eval_bseK_exchange_coupling(iq, BS_n_g_exch, BS_T_grp_ip, i_Tp, &
&                                                     BS_T_grp_ik, i_Tk) result(H_x)
  !==============================
  !
  ! :::EXCHANGE    (coupling):::
  !=============================
  !H_x=V_dot_V_omp(BS_n_g_exch, BS_T_grp(i_Tgrp_p)%O_x( minus_G(1:BS_n_g_exch),i_Tp),&
  !                             BS_T_grp(i_Tgrp_k)%O_x(:,i_Tk)/bare_qpg(iq,:BS_n_g_exch)**2)
  !
  use pars,         ONLY: SP,rZERO
  use BS,           ONLY: BS_T_group
  use wrapper_omp,  ONLY: V_dot_V_omp
  use R_lattice,    ONLY: bare_qpg, minus_G
#ifdef _CUDA
  use R_lattice,    ONLY: bare_qpg_d, minus_G_d
#endif
  implicit none
  !
  integer,                  intent(in) :: iq, BS_n_g_exch,i_Tp,i_Tk
  type(BS_T_group), target, intent(in) :: BS_T_grp_ip,BS_T_grp_ik
  complex(SP) :: H_x
  !
#ifdef _CUDA
  complex(SP), pointer, device :: BS_T_grp_ip_O_x_d(:,:)
  complex(SP), pointer, device :: BS_T_grp_ik_O_x_d(:,:)
  integer :: ig
  real(SP):: H_x_r,H_x_i
#endif
         
#ifdef _CUDA
  !
  BS_T_grp_ip_O_x_d => BS_T_grp_ip%O_x_d
  BS_T_grp_ik_O_x_d => BS_T_grp_ik%O_x_d
  !
  H_x_r=rZERO
  H_x_i=rZERO
  !$cuf kernel do(1) <<<*,*>>>
  do ig = 1, BS_n_g_exch
    H_x = BS_T_grp_ip_O_x_d( minus_G_d(ig),i_Tp) *BS_T_grp_ik_O_x_d(ig,i_Tk)/bare_qpg_d(iq,ig)**2
    H_x_r=H_x_r+real(H_x,SP)
    H_x_i=H_x_i+aimag(H_x)
  enddo
  H_x=cmplx(H_x_r,H_x_i,SP)
#else
  H_x=V_dot_V_omp(BS_n_g_exch, BS_T_grp_ip%O_x( minus_G(1:BS_n_g_exch),i_Tp),&
                               BS_T_grp_ik%O_x(:,i_Tk)/bare_qpg(iq,:BS_n_g_exch)**2)
#endif
 !
end function eval_bseK_exchange_coupling

 !
end subroutine K
