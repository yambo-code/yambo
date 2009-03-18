!
! Copyright (C) 2000-2008 D. Sangalli and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine K_doubles(iq,Ken,Xk,q,X,Xw,W_bss)
 !
 ! K =  2V+Kc if nsppol==1.  BSE Kc=-W;       TDDFT Kc=fuu+fud; RPA Kc=0; TDHF Kc=-v; DOUBLES Kc=2*S-F
 ! K =   V+Kc if nsppol==2.  BSE Kc=-W\delta; TDDFT Kc=fst;     RPA Kc=0; TDHF Kc=-v; DOUBLES Kc=S-F\delta
 !
 use pars,         ONLY:SP,pi,IP
 use LOGO,         ONLY:pickup_a_random
 use memory_m,     ONLY:mem_est
 use drivers,      ONLY:l_col_cut,l_td_hf  
 use frequency,    ONLY:w_samp
 use electrons,    ONLY:levels,n_sp_pol,spin_occ,spin
 use FFT_m,        ONLY:fft_size
 use stderr,       ONLY:intc
 use wave_func,    ONLY:wf_load,WF_free
 use functions,    ONLY:K_scatter
 use D_lattice,    ONLY:DL_vol
 use R_lattice,    ONLY:G_m_G,bz_samp,ik_is_table,bare_qpg
 use par_proc_m,   ONLY:pp_redux_wait,myid,ncpu
 use com,          ONLY:msg,warning,error
 use timing,       ONLY:live_timing
 use X_m,          ONLY:X_alloc,X_t,X_mat
 use BS,           ONLY:BS_bands,BS_eh_E,BS_res_K_corr,BS_W_is_diagonal,&
&                       BS_res_K_exchange,O_n_c_states,BS_n_g_W,BS_blk_dim,&
&                       O_table,BS_mat,O_phase,O_c_state,O_v_state,O_ng,&
&                       BS_eh_table,BS_n_g_exch,BS_identifier,O_n_v_states,&
&                       BS_K_dim,BS_eh_win,BS_blk_coord,cumulative_index,&
&                       BSS_q0,O_n_scatt,BS_O,BS_columns

 ! Davide doubles
 use BS_doubles,   ONLY:BS_mat_doubles,BS_res_K_doubles,BS_eh_table_m1

 !
 use collision,    ONLY:ggwinfo,collision_reset
 use IO_m,         ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,OP_RD,&
&                       RD,RD_CL,OP_WR_CL,OP_APP_WR_CL,RD_CL_IF_END
 

 implicit none
 type(levels)  ::Ken 
 type(bz_samp) ::Xk,q
 type(X_t)     ::X
 type(w_samp)  ::Xw,W_bss
 integer       ::iq
 !
 ! Kernel loop
 !
 integer    :: live_timing_steps,live_timing_step_size,live_timing_accumulate,&
&              iv1,ic1,i_sp1,i_sp2,iv2,ic2,&
&              icv1,icv2,iOvv,iOcc,i1,i2,i3,bands_to_load(2)
 ! Davide doubles
 integer    :: icv,ic,iv,nc,nv,icv_loop
 complex(SP),allocatable::  Interaction_cccv(:,:,:),Interaction_vvcv(:,:,:)
 !
#if defined _DOUBLE
 complex(SP):: zdotc,zdotu
#else
 complex(SP):: cdotc,cdotu
#endif
 complex(SP):: Co,H_res_x,H_res_c
 !
 ! Kernel
 !
 integer(8) :: n_K_elements(ncpu),tot_n_K_elements,i8nc,i8id
 real(SP)   :: E_eh_range(2),S_eh_range(2)
 real(SP)   :: filling
 complex(SP),allocatable ::BS_W(:,:,:)
 logical    :: W_is_em1s,W_is_pp
 !
 ! Oscillators
 !
 integer    :: O_dim
 integer    :: O_x_dim,alloc_err(2),x_fft_size,c_fft_size
 complex(SP),allocatable::Ovv(:),Occ(:),O1x(:,:),O2x(:,:)
 type(ggwinfo)          ::isc
 !
 ! I/O
 !
 integer           ::ioX_err,ioBS_Fxc_err,XID,ID,io_err
 integer, external ::ioX,ioBS,ioBS_doubles
 !
 ! Davide: Here I set all variables not useful for gamma =1
 BS_columns=1 
 isc%qs(2)=1

 ! Davide doubles
 ! preparation lines:
 BS_res_K_doubles=.TRUE.
 nc=BS_bands(2)-Ken%nbf
 nv=Ken%nbf-BS_bands(1)+1
 !Davide Debug
 !open(unit=3456, file='debug.txt', status='unknown')
 open(unit=3457, file='interaction_cccv.txt', status='unknown')
 open(unit=3458, file='interaction_vvcv.txt', status='unknown')
 open(unit=3459, file='BS_mat_doubles.txt', status='unknown')
 !
 Co=4._SP*pi/DL_vol/real(1,SP)
 !
 call section('*','Bethe-Salpeter Kernel + DOUBLES')
 !
 ! Setups
 !
 BS_blk_coord=0
 !
 ! Dimensions and Tables
 ! 
 allocate(BS_blk_dim(1))
 !
 call mem_est("BS_blk_dim",(/1/),(/IP/))
 if (BS_res_K_corr) then
   !
   allocate(O_n_c_states(1,n_sp_pol),O_n_v_states(1,n_sp_pol))
   call mem_est("O_n_c_states O_n_v_states",&
&               (/1,n_sp_pol,1,n_sp_pol/),(/IP,IP,IP,IP/)) !to check 
   !
 endif
 !
 call K_eh_setup(iq,Ken,Xk)
 if (any(BS_blk_dim==0)) then
   call error(' Null BSE kernel block dimension(s) found. Increase e/h range')
 endif
 !
 ! BS DB
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 ioBS_Fxc_err=ioBS(iq,X,ID)
 !
 if (ioBS_Fxc_err==0) then
   if (BS_res_K_corr) then
     deallocate(O_v_state,O_c_state,O_n_c_states,O_n_v_states)
     call mem_est("O_v_state O_c_state O_n_c_states O_n_v_states")
   endif
   return
 endif
 !
 ! Exchange
 !
 call             msg('rsn','[BSE] Kernel dimension    :',BS_K_dim)
 if (BS_res_K_exchange) call msg('r','[BSE] Exchange components :',BS_n_g_exch)
 !
 ! PP/Epsm1S DBs
 !
 W_is_em1s=X%whoami==2
 W_is_pp  =X%whoami==4
 if (BS_res_K_corr.and.(.not.l_td_hf).and.(.not.BS_res_K_doubles)) then
   call section('+','Screneed interaction header I/O')
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
   ! X%ng is set to BS_n_g_W to VERIFY if there wnough bands.
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
 E_eh_range=(/minval(abs(BS_eh_E))-1.E-5,maxval(abs(BS_eh_E))/)
 S_eh_range=(/BS_eh_win(1)/100.*(E_eh_range(2)-E_eh_range(1)),& 
&             BS_eh_win(2)/100.*(E_eh_range(2)-E_eh_range(1))/)
 call K_filling(E_eh_range,S_eh_range,n_K_elements,tot_n_K_elements,&
&               live_timing_steps,live_timing_step_size)
 !
 if (all(n_K_elements==0)) return
 !
 !
 ! Oscillators Setup
 !
 O_ng=maxval(G_m_G)
 !
 call fft_setup(O_ng,1,.true.)
 c_fft_size=fft_size
 call fft_setup(BS_n_g_exch,maxval((/1/)),.true.)    !qindx_X(iq,:,2)),.true.)
 x_fft_size=fft_size
 if (x_fft_size>c_fft_size) then
   call warning('Exchange FFT size is too big. RL vectors reduced to '//intc(O_ng))
   BS_n_g_exch=O_ng
 endif
 !
 !
 bands_to_load=BS_bands
 !
 call wf_load(max(O_ng,BS_n_g_exch),1,bands_to_load,(/1,1/),&    
&               space='R',title='-BSK')
 ! 
 !
 allocate(O_table(BS_bands(2)-BS_bands(1)+1,1,&
&                 BS_bands(2)-BS_bands(1)+1,1,n_sp_pol),stat=alloc_err(1))
 !
 call mem_est('O_table',(/size(O_table)/),elements_kind=(/SP/),errors=(/alloc_err(1)/))
 !
 O_dim=-1
 O_x_dim=maxval(BS_blk_dim)
 call K_scattering(iq,1,1,Xk,q)  
 O_dim=max(O_dim,O_n_scatt)
     !
 !
 if (BS_res_K_corr) then
   call collision_reset(isc)
   !
   ! Screneed interaction
   !
   X%ng=BS_n_g_W
   if (W_is_em1s) call X_alloc('X',(/BS_n_g_W,BS_n_g_W,1/))
   if (W_is_pp)   call X_alloc('X',(/BS_n_g_W,BS_n_g_W,2/))
   !
   i1=BS_n_g_W
   if (BS_W_is_diagonal) i1=1
   !
   ! When TR is present but not the SI X_mat indexes need to be exchanged 
   ! when the TR is applied
   !
   allocate(BS_W(BS_n_g_W,i1,q%nibz),stat=alloc_err(1))
   call mem_est('BS_W',(/size(BS_W)/),errors=(/alloc_err(1)/))
   !
   allocate(isc%gamp(i1,BS_n_g_W))
   !
   if (.not.l_td_hf.and.(.not.BS_res_K_doubles)) then
     call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=XID)
     ioX_err=ioX(X,Xw,XID)
   endif
   !
   call scatterGamp(isc,'c')
   !
   if (l_td_hf.or.BS_res_K_doubles) then 
       X_mat=(0.,0.)
   else
     call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2,3/),ID=XID)
     ioX_err=ioX(X,Xw,XID)
   endif
   !
   forall(i2=1:BS_n_g_W) X_mat(i2,i2,1)=X_mat(i2,i2,1)+1.
   do i2=1,BS_n_g_W
     do i3=1,BS_n_g_W
       if (.not.BS_W_is_diagonal) then
         BS_W(i2,i3,1)=X_mat(i2,i3,1)*isc%gamp(i2,i3)
       endif
       if (BS_W_is_diagonal.and.i2==i3) BS_W(i2,1,1)=real(X_mat(i2,i2,1))*isc%gamp(1,i2)
     enddo
   enddo
   !
   !
   deallocate(isc%gamp)
   call X_alloc('X')
   !
   X%ng=X%ng_db
   call collision_reset(isc)
 endif
 !
 ! DB identifier 
 !
 BS_identifier=pickup_a_random(10000._SP)
 !
 call section('=','Main loop')
 !
 !allocations
 !***********
 if (BS_res_K_corr) then
   allocate(BS_O(O_ng,O_dim),stat=alloc_err(1))
   call mem_est('BS_O',(/size(BS_O)/),errors=(/alloc_err(1)/))
   allocate(Ovv(BS_n_g_W),Occ(BS_n_g_W))
   call mem_est('O_RES_WS',(/3*BS_n_g_W/))
 endif
 !
 if (BS_res_K_exchange) then
   allocate(O1x(BS_n_g_exch,O_x_dim),stat=alloc_err(1))
   allocate(O2x(BS_n_g_exch,O_x_dim),stat=alloc_err(2))
   call mem_est('O1x O2x',(/size(O1x),size(O2x)/),errors=alloc_err(:2))
 endif
 !
 !
 ! Davide doubles
 if (BS_res_K_doubles) then
   allocate(Interaction_cccv(nc,nc,BS_K_dim))
   allocate(Interaction_vvcv(nv,nv,BS_K_dim))
   call mem_est('Interaction_cccv',(/size(interaction_cccv)*2/))
   call mem_est('Interaction_vvcv',(/size(interaction_vvcv)*2/))
   allocate(BS_mat_doubles(BS_K_dim,BS_K_dim,BS_K_dim,8))
   call mem_est('BS_mat_doubles',(/size(BS_mat_doubles)*2/))
   allocate(BS_eh_table_m1(nv,nc))
   call mem_est('BS_eh_table_m1',(/size(BS_eh_table_m1)/))
   !
   BS_mat_doubles(:,:,:,:)=(0.,0.)
   Interaction_cccv(:,:,:)=(0.,0.)
   Interaction_vvcv(:,:,:)=(0.,0.)
   BS_eh_table_m1(:,:)=0
   !
   do icv=1,BS_K_dim
     ic=BS_eh_table(icv,3)
     iv=BS_eh_table(icv,2)
     BS_eh_table_m1(iv,ic)=icv
   enddo
 endif

 call mem_est('BS_mat',(/O_x_dim**2/))
 !
 call live_timing('BSK',live_timing_steps)
 !
 tot_n_K_elements=0
 live_timing_accumulate=0
 i8nc=ncpu
 i8id=myid
 !
 call pp_redux_wait
 !
 if (BS_res_K_exchange) call K_exchange(.not.l_col_cut,iq,1,Xk,O2x,O_x_dim)
 !
 allocate(BS_mat(BS_K_dim,BS_K_dim))
 !
 if (BS_res_K_corr) call K_scattering(iq,1,1,Xk,q)  
 !
 if (BS_res_K_exchange) then
   !
   O1x=O2x
   !
   ! When a cutoffed coulomb interaction is used bare_qpg(:,:) elements
   ! may be complex. In this case I cannot multiply both O1x and O2x by
   ! 1./bare_qpg as O2x is conjugated in the cdotc call.
   !
   if(l_col_cut) then 
     do i1=1,BS_K_dim
       do i2=1,BS_n_g_exch
         O1x(i2,i1)=O1x(i2,i1)/bare_qpg(iq,i2)**2
       enddo
     enddo
   endif 
   !
   ! Even if the cutoff coulomb interaction is used and this term should
   ! be in principle included, they are vanishing due to the q->0 in the
   ! oscillator and the finitness of qpg(1,1) term.
   !
   O1x(1,:)=(0.,0.)
   O2x(1,:)=(0.,0.)
   !
 endif
 !
 BS_mat=(0.,0.)
 !
 call pp_redux_wait
 do icv1=1,BS_K_dim
   iv1=BS_eh_table(icv1,2)
   ic1=BS_eh_table(icv1,3)
   i_sp1=spin(BS_eh_table(icv1,:))
   !
   do icv2=1,BS_K_dim
     !
     if (icv2<icv1) cycle
     !
     ! ::: E/h energy window :::
     !
     if (.not.K_scatter(abs(BS_eh_E(icv1)),abs(BS_eh_E(icv2)),&
&                       E_eh_range,S_eh_range)) cycle
     !
     ! ::: Parallel Switch :::
     !
     tot_n_K_elements=tot_n_K_elements+1
     if (mod(tot_n_K_elements-i8id,i8nc)/=1.and.ncpu>1)  cycle
     !
     ! ::: Timing START :::
     !
     live_timing_accumulate=live_timing_accumulate+1
     if (live_timing_accumulate==live_timing_step_size) then
       call live_timing(steps=1)
       live_timing_accumulate=0
     endif
     !
     ! ::: Timing END :::
     !
     iv2=BS_eh_table(icv2,2)
     ic2=BS_eh_table(icv2,3)
     i_sp2=spin(BS_eh_table(icv2,:))
     !
     H_res_x=(0.,0.)
     H_res_c=(0.,0.)
     ! 
     ! :::Exchange    (resonant):::
     !
#if defined _DOUBLE
     if (BS_res_K_exchange) H_res_x=zdotc(BS_n_g_exch,O2x(1,icv2),1,O1x(1,icv1),1)
#else
     if (BS_res_K_exchange) H_res_x=cdotc(BS_n_g_exch,O2x(1,icv2),1,O1x(1,icv1),1)
#endif
     !
     !
     ! :::Correlation (resonant):::
     !
     if (BS_res_K_corr.and.i_sp1==i_sp2) then
       iOcc=O_table(ic1-BS_bands(1)+1,1, ic2-BS_bands(1)+1,1,i_sp1)
       iOvv=O_table(iv1-BS_bands(1)+1,1,iv2-BS_bands(1)+1,1,i_sp2)
       !
       Ovv(:)= BS_O(:,iOvv)
       !
       if (BS_W_is_diagonal) then
         !
         forall(i1=1:BS_n_g_W) Occ(i1)=BS_O(i1,iOcc)*BS_W(i1,1,1)
       else
         !
         do i1=1,BS_n_g_W 
#if defined _DOUBLE
           Occ(i1)=zdotu(BS_n_g_W,BS_O(:,iOcc),1,BS_W(1,i1,1),1)
#else
           Occ(i1)=cdotu(BS_n_g_W,BS_O(:,iOcc),1,BS_W(1,i1,1),1)
#endif
         enddo
       endif
       !
       !
#if defined _DOUBLE
       H_res_c=zdotc(BS_n_g_W,Ovv,1,Occ,1)*4.*pi
#else
       H_res_c=cdotc(BS_n_g_W,Ovv,1,Occ,1)*4.*pi
#endif
       !
     endif
     !
     ! Impose the kernel to be hermitian
     !
     if (icv1==icv2) H_res_c=real(H_res_c)
     !
     BS_mat(icv1,icv2)=H_res_x*real(spin_occ)*Co-H_res_c
     !
   enddo
 enddo
 !
 !  ********************************************
 !  ********************************************
 !  *******                              *******
 !  *******        Davide doubles        *******
 !  *******                              *******
 !  ********************************************
 !  ********************************************
 !  Main loop
 !
 ! Davide doubles
 if (BS_res_K_doubles.and.(.not.l_col_cut)) then
  do i1=1,BS_K_dim
    do i2=1,BS_n_g_exch
      O2x(i2,i1)=O2x(i2,i1)*bare_qpg(iq,i2)
    enddo
  enddo 
 endif
 !
 if (BS_res_K_doubles) then
   do icv=1,BS_K_dim
     iv=BS_eh_table(icv,2)
     ic=BS_eh_table(icv,3)
     !
     !  Step 1: Here the code constructs U_{c1c2,cv} and U_{v1v2,cv} 
     !          starting O2x, Occ and Ovv
     !
     !          1.a: U_{cccv}
     !
     do ic1=1+Ken%nbf,BS_bands(2)
       do ic2=1+Ken%nbf,BS_bands(2)
         iOcc=O_table(ic1-BS_bands(1)+1,1,ic2-BS_bands(1)+1,1,1)
         !
         if (BS_W_is_diagonal) then
           forall(i1=1:BS_n_g_W) Occ(i1)=BS_O(i1,iOcc)*BS_W(i1,1,1)
         else
           !
           do i1=1,BS_n_g_W 
#if defined _DOUBLE
             Occ(i1)=zdotu(BS_n_g_W,BS_O(:,iOcc),1,BS_W(1,i1,1),1)  
#else
             Occ(i1)=cdotu(BS_n_g_W,BS_O(:,iOcc),1,BS_W(1,i1,1),1)
#endif
           enddo
         endif
         !
#if defined _DOUBLE
         Interaction_cccv(ic1-Ken%nbf,ic2-Ken%nbf,icv)=&
&                            zdotc(BS_n_g_W,O2x,1,Occ,1)*4.*pi       
#else                                                         
         Interaction_cccv(ic1-Ken%nbf,ic2-Ken%nbf,icv)=&
&                            cdotc(BS_n_g_W,O2x,1,Occ,1)*4.*pi       
#endif
         !Davide debug
         write(3457,*) ic1,ic2,ic,iv,&
&             Interaction_cccv(ic1-Ken%nbf,ic2-Ken%nbf,icv)
       enddo
     enddo
     !
     !          1.b: U_{vvcv}
     !
     do iv1=BS_bands(1),Ken%nbf
       do iv2=BS_bands(1),Ken%nbf
         iOvv=O_table(iv1-BS_bands(1)+1,1,iv2-BS_bands(1)+1,1,1)
         !
         if (BS_W_is_diagonal) then
           forall(i1=1:BS_n_g_W) Ovv(i1)=BS_O(i1,iOvv)*BS_W(i1,1,1) 
         else
           !
           do i1=1,BS_n_g_W 
#if defined _DOUBLE
             Ovv(i1)=zdotu(BS_n_g_W,BS_O(:,iOvv),1,BS_W(1,i1,1),1)
#else
             Ovv(i1)=cdotu(BS_n_g_W,BS_O(:,iOvv),1,BS_W(1,i1,1),1)
#endif
           enddo
         endif
#if defined _DOUBLE
         Interaction_vvcv(iv1-BS_bands(1)+1,iv2-BS_bands(1)+1,icv)=&
&                                        zdotc(BS_n_g_W,O2x,1,Ovv,1)*4.*pi       
#else                                                         
         Interaction_vvcv(iv1-BS_bands(1)+1,iv2-BS_bands(1)+1,icv)=&
&                                        cdotc(BS_n_g_W,O2x,1,Ovv,1)*4.*pi       
#endif
         !Davide debug
         write(3458,*) iv1,iv2,ic,iv,&
&             Interaction_vvcv(iv1-BS_bands(1)+1,iv2-BS_bands(1)+1,icv)
       enddo
     enddo
   enddo
   !
   !  Step 2: Here the code constructs the half-diagrams
   !          D1(icv,idoubles) and D2(icv,idoubles),
   !          starting from U_{cc1,c2v2} and U_{vv1,c2v2}
   !
   !          c  ===<===:===<===  c1              c  ===<===x===<===  c1
   !                /|\ :                                   
   !              q  |  :
   !                    :===<===  c2                          ==<===  c2
   !      D1             \                    D2             /
   !                      ==>===  v2                        :===>===  v2
   !                                                  q  |  :
   !                                                    \|/ :
   !          v  ===>===x===>===  v1              v  ===>===:===>===  v1
   !
   !          and the final simmetrized expression:
   !          C_{sd}=0.25*[D1+D2 -(c1<->c2) -(v1<->v2) +(c1,v1<->c2,v2)]
   !
   do icv1=1,BS_K_dim
     ic1=BS_eh_table(icv1,3)
     iv1=BS_eh_table(icv1,2)
     do icv2=1,BS_K_dim
       ic2=BS_eh_table(icv2,3)
       iv2=BS_eh_table(icv2,2)
       !
       if ((iv1==iv2).or.(ic1==ic2)) cycle
       do icv=1,BS_K_dim
         ic=BS_eh_table(icv,3)
         iv=BS_eh_table(icv,2)
         if (iv==iv1) then
           icv_loop=BS_eh_table_m1(iv2,ic1)
           BS_mat_doubles(icv,icv1,icv2,1)=&
&                 Interaction_cccv(ic-Ken%nbf,ic1-Ken%nbf,icv2)                    !D1
           BS_mat_doubles(icv,icv1,icv2,2)=&
&                -Interaction_cccv(ic-Ken%nbf,ic2-Ken%nbf,icv_loop)                !D1c
         endif
         if (iv==iv2) then 
           icv_loop=BS_eh_table_m1(iv1,ic2)
           BS_mat_doubles(icv,icv1,icv2,4)=&
&                 Interaction_cccv(ic-Ken%nbf,ic2-Ken%nbf,icv1)                    !D1vc
           BS_mat_doubles(icv,icv1,icv2,3)=&
&                -Interaction_cccv(ic-Ken%nbf,ic1-Ken%nbf,icv_loop)                !D1v
         endif
         if (ic==ic1) then
           icv_loop=BS_eh_table_m1(iv1,ic2)
           BS_mat_doubles(icv,icv1,icv2,5)=&
&                 Interaction_vvcv(iv-BS_bands(1)+1,iv1-BS_bands(1)+1,icv2)        !D2
           BS_mat_doubles(icv,icv1,icv2,7)=&
                 -Interaction_vvcv(iv-BS_bands(1)+1,iv2-BS_bands(1)+1,icv_loop)    !D2v
         endif
         if (ic==ic2) then 
           icv_loop=BS_eh_table_m1(iv2,ic1)
           BS_mat_doubles(icv,icv1,icv2,8)=&
&                 Interaction_vvcv(iv-BS_bands(1)+1,iv2-BS_bands(1)+1,icv1)        !D2vc
           BS_mat_doubles(icv,icv1,icv2,6)=&
&                -Interaction_cccv(iv-BS_bands(1)+1,iv1-BS_bands(1)+1,icv_loop)    !D2c
         endif
         !Davide debug
         write(3459,*) BS_mat_doubles(icv,icv1,icv2,i1)
       enddo
     enddo
   enddo
   !
   BS_mat_doubles(:,:,:,:)=0.25*BS_mat_doubles(:,:,:,:)
   !
 endif
 ! 
 call pp_redux_wait(BS_mat)
 !
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,cumulative_index(1,1)/),ID=ID)
 ioBS_Fxc_err=ioBS(iq,X,ID)
 !
 deallocate(BS_mat)
 !
 ! Live Timing finalize
 !
 if (mod(n_K_elements(myid+1),int(live_timing_step_size,8))/=0) call live_timing(steps=1)
 call live_timing()
 call pp_redux_wait
 !
 ! CLEAN
 !
 deallocate(O_table)
 call mem_est("O_table")
 !
 call WF_free()
 call mem_est("BS_mat")
 !
 if (BS_res_K_corr) then
   deallocate(O_v_state,O_c_state,O_n_c_states,O_n_v_states)
   call mem_est("O_v_state O_c_state O_n_c_states O_n_v_states")
   deallocate(BS_W,O_phase,ik_is_table)
   call mem_est("BS_W O_phase ik_is_table")
   deallocate(Ovv,Occ)
   call mem_est("O_RES_WS")
   deallocate(BS_O)
   call mem_est("BS_O")
   !
   !
 endif
 !
 if (BS_res_K_exchange) then
   deallocate(O1x,O2x)
   call mem_est("O1x O2x")
 endif
 !
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 io_err=ioBS_doubles(ID)
 !
 ! Davide doubles
 if (BS_res_K_doubles) then
   deallocate(Interaction_cccv,Interaction_vvcv)
   call mem_est('Interaction_cccv Interaction_vvcv')
   deallocate(BS_mat_doubles)
   call mem_est('BS_mat_doubles')
   deallocate(BS_eh_table_m1)
   call mem_est('BS_eh_table_m1')
 endif
 !
 ! DEBUG >
 !close(unit=3456)
 !close(unit=3457)
 !close(unit=3458)
 !close(unit=3459)
 ! DEBUG <
 !
end subroutine
