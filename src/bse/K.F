!
!        Copyright (C) 2000-2014 the YAMBO team
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
 use pars,         ONLY:SP,pi,IP
 use parser_m,     ONLY:parser
 use LOGO,         ONLY:pickup_a_random
 use memory_m,     ONLY:mem_est
 use drivers,      ONLY:l_bs_fxc,l_col_cut,l_td_hf
 use frequency,    ONLY:w_samp
 use electrons,    ONLY:levels,n_sp_pol,spin_occ,spin
 use FFT_m,        ONLY:fft_size
 use stderr,       ONLY:intc
 use interfaces,   ONLY:WF_load
 use functions,    ONLY:K_scatter
 use D_lattice,    ONLY:nsym,DL_vol,i_time_rev,sop_tab,sop_inv,i_space_inv
 use R_lattice,    ONLY:G_m_G,g_rot,qindx_B,qindx_X,bz_samp,RIM_qpg,&
&                       RIM_anisotropy,bare_qpg,minus_G
 use parallel_m,   ONLY:PP_redux_wait,myid,ncpu
 use com,          ONLY:msg,warning,error
 use LIVE_t,       ONLY:live_timing
 use X_m,          ONLY:X_alloc,X_t,X_mat
 use BS,           ONLY:BS_bands,BS_eh_E,BS_res_K_corr,BS_W_is_diagonal,&
&                       BS_res_K_exchange,O_n_c_states,&
&                       BS_n_g_W,BS_nT_at_k,O_table,BS_mat,BS_mat_diagonal,&
&                       O_phase,O_c_state,O_v_state,O_ng,BS_eh_table,BS_columns,&
&                       BS_n_g_exch,BS_identifier,BS_K_coupling,O_n_v_states,&
&                       BS_K_dim,BS_eh_win,BS_k_and_row_restart,&
&                       BS_K_is_ALDA,BS_cpl_mat,&
&                       BS_cpl_K_exchange,BSS_q0,BSS_uses_RIM,&
&                       O_n_scatt,BS_O,BS_cpl_K_corr
 use collision,    ONLY:ggwinfo,collision_reset
 use IO_m,         ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,OP_RD,&
&                       RD,RD_CL,OP_WR_CL,OP_APP_WR_CL,RD_CL_IF_END
 use matrix_operate,ONLY:UP_matrix_index
 use TDDFT,         ONLY:FXC_K_diagonal,FXC,FXC_n_g_corr,ioBS_Fxc
 use xc_functionals,ONLY:F_xc,XC_potential_driver
 use wrapper,       ONLY:V_dot_V,Vstar_dot_V,M_by_V
 use global_XC,     ONLY:WF_kind,WF_xc_functional
 use wave_func,     ONLY:WF
 !
 use BS,            ONLY:n_BS_blks,BS_blk,BS_T_grp
 !
 implicit none
 type(levels)  ::Ken 
 type(bz_samp) ::Xk,q
 type(X_t)     ::X
 type(w_samp)  ::Xw,W_bss
 integer       ::iq
 !
 ! Kernel loop
 !
 integer    :: ikk1,ikk2,live_timing_steps,live_timing_step_size,live_timing_accumulate,&
&              is1xis2,ik1bz,iv1,ic1,ik1,is1,inv_s1,is1p,&
&              i_sp1,i_sp2,ik2bz,iv2,ic2,ik2,is2,is2p,&
&              icv1,icv2,iOvv,iOcc,i1,i2,i3,iqs,iq_W,ig0,bands_to_load(2),&
&              iBSE1,iBSE2
 integer    :: iOcv, iOvc
 complex(SP):: Co,H_res_x,H_res_c,H_cpl_x,H_cpl_c
 !
 integer    :: i_block,i_k_bz,i_p_bz,i_c_k,i_v_k,i_c_p,i_v_p,i_k_s,i_p_s,&
&              i_Tgrp_k,i_Tgrp_p,i_Tp,i_Tk,i_k_spin,i_p_spin,i_k,i_p
 !
 ! Kernel
 !
 integer(8) :: n_K_elements(ncpu),tot_n_K_elements,i8nc,i8id
 real(SP)   :: E_eh_range(2),S_eh_range(2)
 complex(SP),allocatable ::BS_W(:,:,:)
 complex(SP),   external ::tddft_alda_r_space
 logical    :: W_is_em1s,W_is_pp
 !
 ! Oscillators
 !
 integer    :: O_dim
 integer    :: O_x_dim,alloc_err(2),x_fft_size,c_fft_size
 logical    :: load_O_X,l_USE_all_Gexx
 complex(SP),allocatable::Ovv(:),Occ(:),Cc(:)
 complex(SP),        allocatable::O1x(:,:),O2x(:,:)
 complex(SP),allocatable::Ovc(:),Ocv(:)
 type(ggwinfo)          ::isc
 !
 ! I/O
 !
 integer           ::ioX_err,ioBS_Fxc_err,ioBS_err,XID,ID
 integer, external ::ioX,ioBS
 !
 ! Constants & Setup
 !==================
 !
 Co=4._SP*pi/DL_vol/real(q%nbz,SP)
 ioBS_err    =-1
 ioBS_Fxc_err=-1
 call parser('ALLGexx',l_USE_all_Gexx)
 !
 ! Titles
 !========
 !
 if (.not.l_bs_fxc) call section('=','Bethe-Salpeter Kernel')
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
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
   !
   ioBS_Fxc_err=ioBS_Fxc(iq,W_bss,ID,X=X)
   !
 else
   !
   ! GPL_EXCLUDE_END
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
   ioBS_err=ioBS(iq,X,ID)
   !
   ! GPL_EXCLUDE_START
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
! if (ioBS_Fxc_err==0.or.ioBS_err==0) then
!   if (BS_res_K_corr) then
!     deallocate(O_v_state,O_c_state,O_n_c_states,O_n_v_states)
!     call mem_est("O_v_state O_c_state O_n_c_states O_n_v_states")
!   endif
!   return
! endif
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
   ioX_err=ioX(X,Xw,XID) 
   !
   if (ioX_err/=0) call warning('BS section skipped. PP/Em1s DB does not fit/exist')
   !
   ! X%ng is set to BS_n_g_W to VERIFY if there are enough bands.
   ! From now on it is used to keep track of the actual sixe of W in the DB
   !
   X%ng=X%ng_db
   !
   if (ioX_err/=0) return
   !
 endif
 !
 ! Kernel filling
 !
! E_eh_range=(/minval(abs(BS_eh_E))-1.E-5,maxval(abs(BS_eh_E))/)
! S_eh_range=(/BS_eh_win(1)/100.*(E_eh_range(2)-E_eh_range(1)),& 
!&             BS_eh_win(2)/100.*(E_eh_range(2)-E_eh_range(1))/)
! !
! call K_filling(E_eh_range,S_eh_range,n_K_elements,tot_n_K_elements,&
!&               live_timing_steps,live_timing_step_size)
! !
! if (all(n_K_elements==0)) return
 !
 ! TDDFT xc-kernel Setup
 !======================
 if (l_bs_fxc) then
   !  
   if (iq==1) call Dipole_driver(Ken,Xk,X,BSS_q0)
   !
   ! Allocation
   !-----------
   allocate(FXC_K_diagonal(BS_K_dim),FXC(FXC_n_g_corr,FXC_n_g_corr,W_bss%n_freqs))
   call mem_est("FXC_K_diagonal FXC",(/BS_K_dim,size(FXC)/),(/SP,2*SP/))
   FXC_K_diagonal=0._SP
   FXC=(0._SP,0._SP)
   !
 endif
 !
 ! Oscillators Setup
 !===================
 O_ng=maxval(G_m_G)
 !
 if (any((/BS_res_K_corr,BS_cpl_K_corr/)).and..not.l_USE_all_Gexx.and..not.l_bs_fxc) then
   call fft_setup(O_ng,1,.true.)
   c_fft_size=fft_size
   call fft_setup(BS_n_g_exch,maxval(qindx_X(iq,:,2)),.true.)
   x_fft_size=fft_size
   if (x_fft_size>c_fft_size) then
     call warning('Exchange FFT size is too big. RL vectors reduced to '//intc(O_ng))
     BS_n_g_exch=O_ng
   endif
 endif
 !
 ! Wave Functions
 !================
 !
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
 ! Spatial Inversion Test
 !========================
 call WF_spatial_invertion(Ken,Xk)
 !
! allocate(O_table(BS_bands(2)-BS_bands(1)+1,nsym,&
!&                 BS_bands(2)-BS_bands(1)+1,nsym,n_sp_pol),stat=alloc_err(1))
! !
! call mem_est('O_table',(/size(O_table)/),elements_kind=(/SP/),errors=(/alloc_err(1)/))
! !
! O_dim=-1
! O_x_dim=maxval(BS_nT_at_k)
! do ik2=1,BS_columns
!   do ik1=ik2,1,-1
!     !
!     if (ik2<BS_k_and_row_restart(2)) cycle
!     if (ik2==BS_k_and_row_restart(2).and.ik1>=BS_k_and_row_restart(1)) cycle
!     !
!     call K_scattering(iq,ik1,ik2,Xk,q)  
!     !
!     O_dim=max(O_dim,O_n_scatt)
!     !
!   enddo
! enddo
! !
! ! GPL_EXCLUDE_START
! !
! if (l_bs_fxc) call mem_est("BS_K_2_FXC",&
!&                          (/2*6*FXC_n_g_corr*O_x_dim+2*FXC_n_g_corr**2+2*O_x_dim/),&
!&                          (/SP/))
! !
! ! GPL_EXCLUDE_END
 !
 ! ALDA
 !======
 if (BS_K_is_ALDA) then
   allocate(F_xc(fft_size))
   call XC_potential_driver(Ken,Xk,WF_KIND,WF_xc_functional,2)
 endif
 !
 ! Screened interaction
 !======================
 !
 if (BS_res_K_corr) then
   !
   call collision_reset(isc)
   !
   X%ng=BS_n_g_W
   if (.not.l_td_hf) then
     if (W_is_em1s) call X_alloc('X',(/BS_n_g_W,BS_n_g_W,1/))
     if (W_is_pp)   call X_alloc('X',(/BS_n_g_W,BS_n_g_W,2/))
   endif
   !
   i1=BS_n_g_W
   if (BS_W_is_diagonal) i1=1
   !
   ! When TR is present but not the SI X_mat indexes need to be exchanged 
   ! when the TR is applied
   !
   if (i_space_inv==0.and.i_time_rev==1.and..not.BS_W_is_diagonal) then
     allocate(BS_W(BS_n_g_W,i1,2*q%nibz),stat=alloc_err(1))
   else
     allocate(BS_W(BS_n_g_W,i1,q%nibz),stat=alloc_err(1))
   endif
   call mem_est('BS_W',(/size(BS_W)/),errors=(/alloc_err(1)/))
   !
   allocate(isc%gamp(i1,BS_n_g_W))
   !
   if (.not.l_td_hf) then
     call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=XID)
     ioX_err=ioX(X,Xw,XID)
   endif
   !
   do iq_W=1,q%nibz
     !
     isc%qs(2)=iq_W
     call scatterGamp(isc,'c')
     !
     if (.not.l_td_hf) then
       call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iq_W,2*iq_W+1/),ID=XID)
       ioX_err=ioX(X,Xw,XID)
       !
       forall(i2=1:BS_n_g_W) X_mat(i2,i2,1)=X_mat(i2,i2,1)+1.
       do i2=1,BS_n_g_W
         do i3=1,BS_n_g_W
           if (.not.BS_W_is_diagonal) then
             BS_W(i2,i3,iq_W)=X_mat(i2,i3,1)*isc%gamp(i2,i3)
             if (i_space_inv==0.and.i_time_rev==1) &
&               BS_W(i2,i3,q%nibz+iq_W)=X_mat(i3,i2,1)*isc%gamp(i2,i3)
           endif
           if (BS_W_is_diagonal.and.i2==i3) BS_W(i2,1,iq_W)=real(X_mat(i2,i2,1))*isc%gamp(1,i2)
         enddo
       enddo
     else
       forall(i2=1:BS_n_g_W) BS_W(i2,1,iq_W)=isc%gamp(1,i2)
     endif
     ! 
   enddo
   !
   ! Anisotropy correction. Note that isc%gamp(1,1)=RIM_anisotropy while
   ! the \delta(G,G') term must be multiplied by the standard RIM_qpg(1,1)
   !
   if (RIM_anisotropy/=0.) BS_W(1,1,1)=BS_W(1,1,1)+RIM_qpg(1,1,1)/2.-RIM_anisotropy/2.
   !
   deallocate(isc%gamp)
   !
   if (.not.l_td_hf) call X_alloc('X')
   !
   X%ng=X%ng_db
   call collision_reset(isc)
   !
 endif
 !
 ! DB identifier 
 !===============
 !
 BS_identifier=pickup_a_random(10000._SP)
 !
 call section('=','Kernel loop')
 !==============================
 !
 ! Allocations
 !-------------
 !if (BS_res_K_corr.or.BS_cpl_K_corr) then
 !  allocate(BS_O(O_ng,O_dim),stat=alloc_err(1))
 !  call mem_est('BS_O',(/size(BS_O)/),errors=(/alloc_err(1)/))
 !  allocate(Ovv(BS_n_g_W),Occ(BS_n_g_W),Cc(BS_n_g_W))
 !  call mem_est('O_RES_WS',(/3*BS_n_g_W/))
 !  if (BS_cpl_K_corr) then
 !    allocate(Ovc(BS_n_g_W),Ocv(BS_n_g_W))
 !    call mem_est('O_CPL_WS',(/2*BS_n_g_W/))
 !  endif
 !endif
 !!
 !if (load_O_X) then
 !  allocate(O2x(BS_n_g_exch,O_x_dim),stat=alloc_err(2))
 !  call mem_est('O2x',(/size(O2x)/),errors=(/alloc_err(2)/))
 !  allocate(O1x(BS_n_g_exch,O_x_dim),stat=alloc_err(1))
 !  call mem_est('O1x',(/size(O1x)/),errors=(/alloc_err(1)/))
 !endif
 !
 !if (.not.BS_K_coupling) call mem_est('BS_mat',(/O_x_dim**2/))
 !if (BS_K_coupling) call mem_est('BS_mat',(/2*O_x_dim**2/)) 
 !
 !if (BSS_uses_RIM) then
 !  allocate(BS_mat_diagonal(BS_K_dim))
 !  call mem_est('BS_mat_diagonal',(/size(BS_mat_diagonal)/))
 !endif
 !
 if (.not.l_bs_fxc) call live_timing('Kernel',n_BS_blks)
 if (l_bs_fxc) call live_timing('Kernel->Fxc',n_BS_blks)
 !
 do i_block=1,n_BS_blks
   !
   i_k_bz=BS_blk(i_block)%ik
   i_p_bz=BS_blk(i_block)%ip
   !
   i_k  =Xk%sstar(i_k_bz,1)
   i_k_s=Xk%sstar(i_k_bz,1)
   i_p  =Xk%sstar(i_p_bz,1)
   i_p_s=Xk%sstar(i_p_bz,1)
   !
   i_Tgrp_k=BS_blk(i_block)%iT_k
   i_Tgrp_p=BS_blk(i_block)%iT_p
   !
   do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
     !
     do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
       !
       if (i_k_bz==i_p_bz.and.i_Tk<i_Tp) cycle
       !
       i_v_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,1)
       i_c_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,2)
       !
       i_v_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,1)
       i_c_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,2)
       !
       i_k_spin=BS_T_grp(i_Tgrp_k)%table(i_Tk,3)
       i_p_spin=BS_T_grp(i_Tgrp_p)%table(i_Tp,3)
       !
     enddo 
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing( )
 !
 !if(allocated(BS_mat_diagonal)) deallocate(BS_mat_diagonal)
 !
 ! Live Timing finalize
 !
 !if (mod(n_K_elements(myid+1),int(live_timing_step_size,8))/=0) call live_timing(steps=1)
 !call live_timing()
 !call PP_redux_wait
 !
 ! GLOBAL CLEANING 
 !=================
 !
 !deallocate(O_table)
 !call mem_est("O_table")
 !call mem_est("BS_mat")
 !!
 !if (load_O_X) then
 !  deallocate(O1x)
 !  call mem_est("O1x")
 !  deallocate(O2x)
 !  call mem_est("O2x")
 !endif
 !!
 !if (BS_res_K_corr.or.BS_cpl_K_corr) then
 !  deallocate(O_v_state,O_c_state,O_n_c_states,O_n_v_states)
 !  call mem_est("O_v_state O_c_state O_n_c_states O_n_v_states")
 !  deallocate(O_phase)
 !  call mem_est("O_phase")
 !  deallocate(Ovv,Occ,Cc)
 !  call mem_est("O_RES_WS")
 !  deallocate(BS_O)
 !  call mem_est("BS_O")
 !  if (BS_cpl_K_corr) then
 !    deallocate(Ovc,Ocv)
 !    call mem_est("O_CPL_WS")
 !  endif
 !endif
 !
 if (l_bs_fxc) then
   !
   ! GPL_EXCLUDE_START
   !
   deallocate(FXC_K_diagonal,FXC)
   call mem_est("FXC_K_diagonal FXC")
   call mem_est("BS_K_2_FXC")
   !
   ! GPL_EXCLUDE_END
   !
 else if (BS_K_is_ALDA) then
   deallocate(F_xc)
 endif
 !
 ! Final CLEAN
 !
 call WF_free(WF)
 !
end subroutine
