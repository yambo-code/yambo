!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
!====================================
 use pars,         ONLY:SP,pi,IP
 use memory_m,     ONLY:mem_est
 use drivers,      ONLY:l_bs_fxc
 use frequency,    ONLY:w_samp
 use electrons,    ONLY:levels
 use FFT_m,        ONLY:fft_size
 use wave_func,    ONLY:wf_load,WF_free
 use functions,    ONLY:K_scatter
 use D_lattice,    ONLY:nsym,DL_vol,i_time_rev,sop_tab,sop_inv
 use R_lattice,    ONLY:G_m_G,g_rot,qindx_B,qindx_X,bz_samp,RIM_qpg,&
&                       RIM_anisotropy,ik_is_table,nXkibz
 use par_proc_m,   ONLY:pp_redux_wait,myid,ncpu
 use com,          ONLY:msg,warning
 use timing,       ONLY:live_timing,nhash
 use X_m,          ONLY:X_alloc,X_t,X_mat
 use BS,           ONLY:BS_bands,BS_eh_E,BS_res_K_corr,BS_W_is_diagonal,&
&                       BS_res_K_exchange,O_n_c_states,&
&                       O_n_cc_scatt,O_n_vv_scatt,&
&                       BS_n_g_W,O_cc,O_vv,BS_blk_dim,O_table,BS_mat,&
&                       O_phase,O_c_state,O_v_state,O_ng,BS_eh_table,BS_columns,&
&                       BS_n_g_exch,BS_identifier,BS_K_coupling,O_n_v_states,&
&                       BS_K_dim,BS_eh_win,BS_blk_coord,BSS_n_descs,&
&                       BSS_description,cumulative_index,BS_K_is_ALDA,BS_cpl_mat,&
&                       BS_cpl_K_exchange,BS_cpl_K_corr,BSS_q0
 use collision,    ONLY:ggwinfo,collision_reset
 use IO_m,         ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,OP_RD,&
&                       RD,RD_CL,OP_WR_CL,OP_APP_WR_CL,RD_CL_IF_END
 use TDDFT,        ONLY:FXC_K_diagonal,FXC,FXC_n_descs,FXC_description,&
&                       FXC_n_g_corr
 use xc_functionals, ONLY:GS_xc_KIND,GS_xc_FUNCTIONAL,F_xc
 implicit none
 type(levels)  ::Ken 
 type(bz_samp) ::Xk,q
 type(X_t)     ::X
 type(w_samp)  ::Xw,W_bss
 integer       ::iq
!
! Kernel loop
!
 integer    :: ikk1,ikk2,live_timing_steps,is1xis2
 integer    :: ik1bz,iv1,ic1,ik1,is1,inv_s1,is1p
 integer    :: ik2bz,iv2,ic2,ik2,is2,is2p
 integer    :: icv1,icv2,iOvv,iOcc
 integer    :: i1,i2,i3,iqs,iqibz,ig0
#if defined DP
 complex(DP):: zdotc,zdotu
#else
 complex(SP):: cdotc,cdotu
#endif
 complex(SP):: Co,H_res_x,H_res_c,H_cpl_x,H_cpl_c
!
! Kernel
!
 integer(8) :: n_K_elements(ncpu),tot_n_K_elements,i8nc,i8id
 real(SP)   :: E_eh_range(2),S_eh_range(2)
 real(SP)   :: filling
 complex(SP),allocatable ::BS_W(:,:,:)
 integer,       external ::rand_identifier
 complex(SP),   external ::tddft_alda_r_space
!
! Oscillators
!
 integer    :: O_cc_dim,O_vv_dim,O_x_dim,alloc_err(2),x_fft_size,c_fft_size
 logical    :: load_O_X
 complex(SP),allocatable::Ovv(:),Occ(:),Cc(:),O1x(:,:),O2x(:,:)
 type(ggwinfo)          ::isc
!
! I/O
!
 integer           ::ioX_err,ioPP_err,ioBS_FXC_err,XID,ID
 integer, external ::ioX,ioBS,ioBS_Fxc

 Co=4._SP*pi/DL_vol/real(q%nbz,SP)

!----------------------------------------------------------------
 if (.not.l_bs_fxc) call section('*','Bethe-Salpeter Kernel')
 if (l_bs_fxc)      call section('*','Bethe-Salpeter Kernel -> xc-kernel')
!
! Setups
!========
!
 if (BS_columns<=0.or.BS_columns>Xk%nibz) BS_columns=Xk%nibz
 BS_blk_coord=0
!
! Dimensions and Tables
!=======================
! 
 allocate(BS_blk_dim(Xk%nibz))

 call mem_est("BS_blk_dim",(/Xk%nbz/),(/IP/))
 if (BS_res_K_corr) then
   allocate(O_n_c_states(Xk%nbz),O_n_v_states(Xk%nbz))
   call mem_est("O_n_c_states O_n_v_states",(/Xk%nbz,Xk%nbz/),(/IP,IP/))
 endif

 call K_eh_setup(iq,Ken,Xk)
!
! Descriptions
!--------------
 call K_descriptions(iq,q,X)
!
! BS/FXC DB
!==========
 if (l_bs_fxc) then
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
   ioBS_Fxc_err=ioBS_Fxc(iq,W_bss,ID)
 else
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
   ioBS_FXC_err=ioBS(iq,X,ID)
 endif
 if (ioBS_FXC_err==0) then
   if (BS_res_K_corr) then
     deallocate(O_v_state,O_c_state,O_n_c_states,O_n_v_states)
     call mem_est("O_v_state O_c_state O_n_c_states O_n_v_states")
   endif
   return
 endif
!
! Exchange
!==========
 load_O_X=BS_res_K_exchange.or.l_bs_fxc

 if (.not.BS_K_coupling) then
   call             msg('rsn','[BSE] Kernel dimension    :',BS_K_dim)
 else
   call             msg('rsn','[BSE] Kernel dimension    :',2*BS_K_dim)
 endif
 if (load_O_X) call msg('r','[BSE] Exchange components :',BS_n_g_exch)
!
! PP/Epsm1S DBs
!===============
 if (BS_res_K_corr) then
   call section('+','Screneed interaction header I/O')
!
! X%ng are the G's in the X DB while BS_n_g_W the G's I want to read.
! Now to read only BS_n_g_W components I need to redefine X%ng
!
   X%ng=BS_n_g_W
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=XID)
   ioX_err=ioX(X,Xw,XID) 
   ioPP_err=-1
   if (ioX_err/=0) then
     X%whoami=4
     X%ordering='c'
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=XID)
     ioPP_err=ioX(X,Xw,XID)
     if (ioPP_err/=0) call warning('BS section skipped. PP/Em1s DB does not fit/exist')
   endif
   X%whoami=2
   X%ng=X%ng_db
   if (ioPP_err/=0.and.ioX_err/=0) return
 endif
!
! Kernel filling
!================
 E_eh_range=(/minval(BS_eh_E)-1.E-5,maxval(BS_eh_E)/)
 S_eh_range=(/BS_eh_win(1)/100.*(E_eh_range(2)-E_eh_range(1)),& 
&             BS_eh_win(2)/100.*(E_eh_range(2)-E_eh_range(1))/)
 call K_filling(E_eh_range,S_eh_range,n_K_elements,tot_n_K_elements)

 if (all(n_K_elements==0)) return

! TDDFT xc-kernel Setup
!=======================
 if (l_bs_fxc) then
!  
   if (iq==1) call X_O_strengths(Ken,Xk,X,BSS_q0)
!
! Allocation
!------------
   allocate(FXC_K_diagonal(BS_K_dim),FXC(FXC_n_g_corr,FXC_n_g_corr,W_bss%n(2)))
   call mem_est("FXC_K_diagonal FXC",(/BS_K_dim,size(FXC)/),(/SP,2*SP/))
   call mem_est("BS_K_2_FXC",&
&               (/2*6*FXC_n_g_corr*O_x_dim+2*FXC_n_g_corr**2+2*O_x_dim/),&
&               (/SP/))
   FXC_K_diagonal=0._SP
   FXC=(0._SP,0._SP)
!
   FXC_description(FXC_n_descs+1:FXC_n_descs+BSS_n_descs-2)=BSS_description(3:BSS_n_descs)
   FXC_n_descs=FXC_n_descs+BSS_n_descs-2
 endif
!----------------------------------------------
 if (BS_res_K_corr) then
   call collision_reset(isc)
!
! Screneed interaction
!========================
   X%ng=BS_n_g_W
   if (ioPP_err==0) then
     Xw%n=2
     X%whoami=4
     X%ordering='c'
     call X_alloc('X',(/BS_n_g_W,BS_n_g_W,2/))
   else
      call X_alloc('X',(/BS_n_g_W,BS_n_g_W,1/))
   endif
   i1=BS_n_g_W
   if (BS_W_is_diagonal) i1=1
   allocate(BS_W(BS_n_g_W,i1,q%nibz),stat=alloc_err(1))
   call mem_est('BS_W',(/size(BS_W)/),errors=(/alloc_err(1)/))

   allocate(isc%gamp(i1,BS_n_g_W))

   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=XID)
   ioX_err=ioX(X,Xw,XID)

   do i1=1,q%nibz

     isc%qs(2)=i1
     call scatterGamp(isc,'c')

     call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*i1,2*i1+1/),ID=XID)
 
     ioX_err=ioX(X,Xw,XID)

     forall(i2=1:BS_n_g_W) X_mat(i2,i2,1)=X_mat(i2,i2,1)+1.
     do i2=1,BS_n_g_W
       do i3=1,BS_n_g_W
         if (.not.BS_W_is_diagonal) BS_W(i2,i3,i1)=real(X_mat(i2,i3,1))*isc%gamp(i2,i3)
         if (BS_W_is_diagonal.and.i2==i3) BS_W(i2,1,i1)=real(X_mat(i2,i2,1))*isc%gamp(1,i2)
       enddo
     enddo
   enddo
!
! Anisotropy correction. Note that isc%gamp(1,1)=qpg_anisotropy while
! the \delta(G,G') term must be multiplied by the standard RIM_qpg(1,1)
!
   if (allocated(RIM_qpg)) BS_W(1,1,1)=BS_W(1,1,1)+RIM_qpg(1,1,1)/2.-RIM_anisotropy/2.
!
   deallocate(isc%gamp)
   call X_alloc('X')
   Xw%n=1
   X%whoami=2
   X%ng=X%ng_db
   call collision_reset(isc)
 endif
 
! Oscillators Setup
!===================
 O_ng=maxval(G_m_G)

 call fft_setup(O_ng,1,.true.)
 c_fft_size=fft_size
 call fft_setup(BS_n_g_exch,maxval(qindx_X(iq,:,2)),.true.)
 x_fft_size=fft_size
 if (x_fft_size>c_fft_size) BS_n_g_exch=O_ng

 if (l_bs_fxc) then
   call wf_load(max(O_ng,BS_n_g_exch),1,BS_bands,(/1,BS_columns/),&
&               space='R',title='-BSK->Fxc')
 else
   call wf_load(max(O_ng,BS_n_g_exch),1,BS_bands,(/1,BS_columns/),&
&               space='R',title='-BSK')
 endif

 allocate(O_table(BS_bands(2)-BS_bands(1)+1,nsym,&
&                 BS_bands(2)-BS_bands(1)+1,nsym),stat=alloc_err(1))
 call mem_est('O_table',(/size(O_table)/),elements_kind=(/SP/),errors=(/alloc_err(1)/))

 O_cc_dim=-1
 O_vv_dim=-1
 O_x_dim=maxval(BS_blk_dim)
 do ik2=1,BS_columns
   do ik1=ik2,1,-1
!
     if (any((/ik2<BS_blk_coord(2),ik2==BS_blk_coord(2).and.ik1>BS_blk_coord(1)/))) cycle
!
     call K_scattering(iq,ik1,ik2,Xk,q)  
     O_cc_dim=max(O_cc_dim,O_n_cc_scatt)
     O_vv_dim=max(O_vv_dim,O_n_vv_scatt)
   enddo
 enddo
!
! ALDA
!======
 if (BS_K_is_ALDA) then
   allocate(F_xc(fft_size))
   call xc_lda_driver(Ken,Xk,GS_xc_KIND,GS_xc_FUNCTIONAL,2)
 endif
!
! DB identifier 
!===============
 BS_identifier=rand_identifier()

 call section('=','Main loop')
!=============================
!
!***********
!allocations
!************************************************************************
 if (BS_res_K_corr) then
   allocate(O_cc(O_ng,O_cc_dim),stat=alloc_err(1))
   allocate(O_vv(O_ng,O_vv_dim),stat=alloc_err(2))
   call mem_est('O_cc O_vv',(/size(O_vv),size(O_cc)/),errors=alloc_err(:2))
   allocate(Ovv(BS_n_g_W),Occ(BS_n_g_W),Cc(BS_n_g_W))
   call mem_est('O_temp',(/3*BS_n_g_W/))
 endif

 if (load_O_X) then
   allocate(O1x(BS_n_g_exch,O_x_dim),stat=alloc_err(1))
   allocate(O2x(BS_n_g_exch,O_x_dim),stat=alloc_err(2))
   call mem_est('O1x O2x',(/size(O1x),size(O2x)/),errors=alloc_err(:2))
 endif

 if (.not.BS_K_coupling) call mem_est('BS_mat',(/O_x_dim**2/))
 if (BS_K_coupling) call mem_est('BS_mat',(/2*O_x_dim**2/))

!************************************************************************
 if (.not.l_bs_fxc) call live_timing('BSK',int(n_K_elements(myid+1),4))
 if (l_bs_fxc) call live_timing('BSK->Fxc',int(n_K_elements(myid+1),4))

 tot_n_K_elements=0
 live_timing_steps=0
 i8nc=ncpu
 i8id=myid

 call pp_redux_wait
 do ik2=1,BS_columns
   ikk2=sum(BS_blk_dim(:ik2-1))
   if (load_O_X) call K_exchange(.true.,iq,ik2,Xk,O2x,O_x_dim) 
   do ik1=ik2,1,-1
!
     if (any((/ik2<BS_blk_coord(2),ik2==BS_blk_coord(2).and.ik1>BS_blk_coord(1)/))) cycle
!
     ikk1=sum(BS_blk_dim(:ik1-1))

     allocate(BS_mat(BS_blk_dim(ik1),BS_blk_dim(ik2)))
     if (BS_K_coupling) allocate(BS_cpl_mat(BS_blk_dim(ik1),BS_blk_dim(ik2)))

     if (BS_res_K_corr) call K_scattering(iq,ik1,ik2,Xk,q)  

     if (load_O_X.and.ik1/=ik2) call K_exchange(.true.,iq,ik1,Xk,O1x,O_x_dim)  
     if (load_O_X.and.ik1==ik2) O1x=O2x

     O1x(1,:)=(0.,0.)
     O2x(1,:)=(0.,0.)
     BS_mat=(0.,0.)
     if (BS_K_coupling) BS_cpl_mat=(0.,0.)
     call pp_redux_wait
     do icv1=1,BS_blk_dim(ik1)
       ik1bz=BS_eh_table(ikk1+icv1,1)
       iv1=BS_eh_table(ikk1+icv1,2)
       ic1=BS_eh_table(ikk1+icv1,3)
       is1=Xk%sstar(ik1bz,2)

       is1p=Xk%sstar( qindx_X(iq,ik1bz,1) ,2)

       do icv2=1,BS_blk_dim(ik2)

         if (ik1==ik2.and.icv2<icv1) cycle

! ::: E/h energy window :::
         if (.not.K_scatter(BS_eh_E(ikk1+icv1),BS_eh_E(ikk2+icv2),&
&                           E_eh_range,S_eh_range)) cycle

! ::: Parallel Switch :::
         tot_n_K_elements=tot_n_K_elements+1
         if (mod(tot_n_K_elements-i8id,i8nc)/=1.and.ncpu>1)  cycle
! ::: Timing START :::
         live_timing_steps=live_timing_steps+1
         if (live_timing_steps==n_K_elements(myid+1)/10/nhash) then
           call live_timing(steps=live_timing_steps)
           live_timing_steps=0
         endif
! ::: Timing END :::
         ik2bz=BS_eh_table(ikk2+icv2,1)
         iv2=BS_eh_table(ikk2+icv2,2)
         ic2=BS_eh_table(ikk2+icv2,3)
         is2=Xk%sstar(ik2bz,2)
         is2p=Xk%sstar( qindx_X(iq,ik2bz,1) ,2)
         ig0=qindx_B(ik1bz,ik2bz,2)
         iqibz=q%sstar( qindx_B(ik1bz,ik2bz,1) ,1)
         iqs=  q%sstar( qindx_B(ik1bz,ik2bz,1) ,2)
         H_res_x=(0.,0.)
         H_res_c=(0.,0.)
         H_cpl_x=(0.,0.)
         H_cpl_c=(0.,0.)
! :::Exchange    (resonant):::
#if defined DP
         if (BS_res_K_exchange) H_res_x=zdotc(BS_n_g_exch,O2x(1,icv2),1,O1x(1,icv1),1)
#else
         if (BS_res_K_exchange) H_res_x=cdotc(BS_n_g_exch,O2x(1,icv2),1,O1x(1,icv1),1)
#endif
! :::Exchange    (coupling):::
#if defined DP
         if (BS_cpl_K_exchange) H_cpl_x=zdotu(BS_n_g_exch,&
&                               O2x( g_rot(nsym/2+1,1:BS_n_g_exch),icv2),1,O1x(1,icv1),1)
#else
         if (BS_cpl_K_exchange) H_cpl_x=cdotu(BS_n_g_exch,&
&                               O2x( g_rot(nsym/2+1,1:BS_n_g_exch),icv2),1,O1x(1,icv1),1)
#endif
! :::ALDA        (resonant):::
         if (BS_K_is_ALDA) H_res_x=H_res_x+&
&                          tddft_alda_R_space(iq,(/ic1,ic2/),(/ik1,ik2,ik1,ik2/),&
&                                                (/iv1,iv2/),(/is1,is2,is1,is2/),1)
! :::ALDA        (coupling):::
         if (BS_K_is_ALDA) H_cpl_x=H_cpl_x+&
&                          tddft_alda_R_space(iq,(/ic1,ic2/),(/ik1,ik2,ik1,ik2/),&
&                                                (/iv1,iv2/),(/is1,is2,is1,is2/),2)
! :::Correlation (resonant):::
         if (BS_res_K_corr) then

           iOcc=O_table(ic1-BS_bands(1)+1,is1, ic2-BS_bands(1)+1,is2)
           iOvv=O_table(iv1-BS_bands(1)+1,is1p,iv2-BS_bands(1)+1,is2p)
           inv_s1=sop_inv(is1)
           is1xis2=sop_tab(sop_inv(is1),is2)
           forall(i1=1:BS_n_g_W) Occ(i1)=&
&                O_cc(g_rot(inv_s1,G_m_G(g_rot(iqs,i1),ig0)),iOcc)*O_phase(is1xis2,ic2)
           forall(i1=1:BS_n_g_W) Ovv(i1)=&
&                O_vv(g_rot(inv_s1,G_m_G(g_rot(iqs,i1),ig0)),iOvv)*O_phase(is1xis2,iv2)
           if (is1>nsym/(i_time_rev+1)) Ovv=conjg(Ovv)
           if (is1>nsym/(i_time_rev+1)) Occ=conjg(Occ)
           if (BS_W_is_diagonal) forall(i1=1:BS_n_g_W) Cc(i1)=Occ(i1)*BS_W(i1,1,iqibz)
           if (.not.BS_W_is_diagonal) then
             do i1=1,BS_n_g_W 
#if defined DP
               Cc(i1)=zdotu(BS_n_g_W,Occ,1,BS_W(1,i1,iqibz),1)
#else
               Cc(i1)=cdotu(BS_n_g_W,Occ,1,BS_W(1,i1,iqibz),1)
#endif
             enddo
           endif
#if defined DP
           H_res_c=zdotc(BS_n_g_W,Ovv,1,Cc,1)*4.*pi
#else
           H_res_c=cdotc(BS_n_g_W,Ovv,1,Cc,1)*4.*pi
#endif
         endif
         BS_mat(icv1,icv2)=H_res_x*2.*Co-H_res_c
         if (BS_K_coupling) BS_cpl_mat(icv1,icv2)=H_cpl_x*2.*Co-H_cpl_c

       enddo
     enddo
     call pp_redux_wait(BS_mat)
     if (BS_K_coupling) call pp_redux_wait(BS_cpl_mat)

     if (l_bs_fxc) then
       call TDDFT_BSK_2_FXC(iq,ik1,ik2,&
&                           O1x(:FXC_n_g_corr,:),O2x(:FXC_n_g_corr,:),O_x_dim,W_bss)

     else
       if (ik2==1.and.ik1==1) then
         call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,cumulative_index(1,1)/),ID=ID)
       else
         call io_control(ACTION=OP_APP_WR_CL,COM=REP,SEC=(/cumulative_index(ik1,ik2)/),ID=ID)
       endif
       ioBS_FXC_err=ioBS(iq,X,ID)
     endif

     deallocate(BS_mat)
     if (BS_K_coupling) deallocate(BS_cpl_mat)
   enddo
 enddo
!
! Live Timing finalize
!======================
!
 live_timing_steps=mod( n_K_elements(myid+1) , int(10*nhash,KIND=KIND(n_K_elements)) )
 if (live_timing_steps>0) call live_timing(steps=live_timing_steps)
 call live_timing()
 call pp_redux_wait
!
! CLEAN
!
 deallocate(O_table)
 call mem_est("O_table")

 call WF_free()

 if (BS_res_K_corr) then
   deallocate(O_v_state,O_c_state,O_n_c_states,O_n_v_states)
   deallocate(BS_W,O_cc,O_vv,Ovv,Occ,Cc,O_phase,ik_is_table)
   call mem_est("O_v_state O_c_state")
   call mem_est("O_n_c_states O_n_v_states")
   call mem_est("BS_mat")
   call mem_est("O_cc O_vv O_temp O_phase")
   call mem_est("BS_W ik_is_table")
 endif

 if (load_O_X) then
   deallocate(O1x,O2x)
   call mem_est("O1x O2x")
 endif

 if (l_bs_fxc) then
   deallocate(FXC_K_diagonal,FXC)
   call mem_est("FXC_K_diagonal FXC")
   call mem_est("BS_K_2_FXC")
 else if (BS_K_is_ALDA) then
   deallocate(F_xc)
 endif

 end subroutine
