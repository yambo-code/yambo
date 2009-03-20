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
 use BS,           ONLY:BS_bands,BS_eh_E,BS_W_is_diagonal,&
&                       O_n_c_states,BS_n_g_W,BS_blk_dim,&
&                       O_table,O_phase,O_c_state,O_v_state,O_ng,&
&                       BS_eh_table,BS_n_g_exch,BS_identifier,O_n_v_states,&
&                       BS_K_dim,BS_eh_win,BS_blk_coord,&
&                       BSS_q0,O_n_scatt,BS_O,BS_columns,BS_mat
 use BS_doubles,   ONLY:BS_mat_doubles,BS_res_K_doubles
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
&              icv1,icv2,iOvv,iOcc,i1,i2,i3,&
&              icv,ic,iv,nc,nv,icv_loop
 integer    ,allocatable ::  BS_eh_table_m1(:,:)
 complex(SP),allocatable ::  Interaction_cccv(:,:,:),Interaction_vvcv(:,:,:)
 !
#if defined _DOUBLE
 complex(SP):: zdotc,zdotu
#else
 complex(SP):: cdotc,cdotu
#endif
 complex(SP):: Co
 !
 ! Kernel
 !
 ! Parallel stuff <
 !integer(8) :: n_K_elements(ncpu),tot_n_K_elements,i8nc,i8id
 !real(SP)   :: E_eh_range(2),S_eh_range(2)
 !real(SP)   :: filling
 ! Parallel stuff >
 complex(SP),allocatable ::BS_W(:,:,:)
 logical    :: W_is_em1s,W_is_pp
 !
 ! Oscillators
 !
 integer    :: O_x_dim,alloc_err(2)
 complex(SP),allocatable::Ovv(:),Occ(:),O2x(:,:)
 type(ggwinfo)          ::isc
 !
 ! I/O
 !
 integer           ::ioX_err,XID,io_err,ID      !ioX,ioX_err,XID         for screening
 integer, external ::ioX,ioBS_doubles           !ioBS_doubles,io_err,ID  for doubles
 !
 ! Here I set all variables not useful for gamma =1
 isc%qs(2)=1
 !
 ! Davide doubles
 ! preparation lines:
 BS_res_K_doubles=.TRUE.
 BS_W_is_diagonal=.TRUE.
 nc=BS_bands(2)-Ken%nbf
 nv=Ken%nbf-BS_bands(1)+1
 Co=4._SP*pi/DL_vol/real(1,SP)
 O_x_dim=BS_K_dim
 !
 call section('*','Bethe-Salpeter Kernel + DOUBLES')
 !
 ! Setups
 !
 BS_blk_coord=0
 !
 if (BS_K_dim==0) then
   call error(' Null BSE kernel block dimension(s) found. Increase e/h range')
 endif
 !
 ! We want to check for the doubles DB
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_err=ioBS_doubles(ID)
 !
 if (io_err==0) then
   deallocate(O_n_c_states,O_n_v_states)
   call mem_est("O_n_c_states O_n_v_states")
   return
 endif
 !
 call msg('rsn','[BSE] Kernel dimension    :',BS_K_dim)
 !
 ! For screened interaction, 
 !
 ! PP/Epsm1S DBs
 !
 W_is_em1s=X%whoami==2
 W_is_pp  =X%whoami==4
! if (.not.BS_res_K_doubles) then
!   call section('+','Screneed interaction header I/O')
!   !
!   ! X%ng are the G's in the X DB while BS_n_g_W the G's I want to read.
!   ! Now to read only BS_n_g_W components I need to redefine X%ng
!   !
!   X%ng=BS_n_g_W
!   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=XID)
!   ioX_err=ioX(X,Xw,XID) 
!   !
!   if (ioX_err/=0) call warning('BS section skipped. PP/Em1s DB does not fit/exist')
!   !
!   ! X%ng is set to BS_n_g_W to VERIFY if there wnough bands.
!   ! From now on it is used to keep track of the actual sixe of W in the DB
!   !
!   X%ng=X%ng_db
!   !
!   if (ioX_err/=0) return
!   !
! endif
 !
 ! Parallel stuff  <
 !
 !E_eh_range=(/minval(abs(BS_eh_E))-1.E-5,maxval(abs(BS_eh_E))/)
 !S_eh_range=(/BS_eh_win(1)/100.*(E_eh_range(2)-E_eh_range(1)),&
!&             BS_eh_win(2)/100.*(E_eh_range(2)-E_eh_range(1))/)                  
 !call K_filling(E_eh_range,S_eh_range,n_K_elements,tot_n_K_elements,&            
!&               live_timing_steps,live_timing_step_size)                         
 !if (all(n_K_elements==0)) return                                                
 !
 ! Parallel stuff >
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
 allocate(BS_W(BS_n_g_W,i1,1),stat=alloc_err(1))
 call mem_est('BS_W',(/size(BS_W)/),errors=(/alloc_err(1)/))
 !
 allocate(isc%gamp(i1,BS_n_g_W))
 !
 if (.not.BS_res_K_doubles) then
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=XID)
   ioX_err=ioX(X,Xw,XID)
 endif
 !
 call scatterGamp(isc,'c')
 !
 if (BS_res_K_doubles) then 
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
 deallocate(isc%gamp)
 call X_alloc('X')
 !
 X%ng=X%ng_db
 call collision_reset(isc)
 !
 !
 call section('=','Main loop')
 !
 ! Allocations
 allocate(Ovv(BS_n_g_W),Occ(BS_n_g_W))
 call mem_est('O_RES_WS',(/2*BS_n_g_W/))
 allocate(O2x(BS_n_g_exch,O_x_dim))
 call mem_est('O2x',(/size(O2x)/))

 ! Davide doubles
 allocate(Interaction_cccv(nc,nc,BS_K_dim))
 allocate(Interaction_vvcv(nv,nv,BS_K_dim))
 call mem_est('Interaction_cccv',(/size(interaction_cccv)*2/))
 call mem_est('Interaction_vvcv',(/size(interaction_vvcv)*2/))
 allocate(BS_mat_doubles(BS_K_dim,BS_K_dim,BS_K_dim,8))
 call mem_est('BS_mat_doubles',(/size(BS_mat_doubles)*2/))
 allocate(BS_eh_table_m1(Ken%nbf,BS_bands(2)))
 call mem_est('BS_eh_table_m1',(/size(BS_eh_table_m1)/))
 !
 BS_mat_doubles(:,:,:,:)=(0.,0.)
 Interaction_cccv(:,:,:)=(0.,0.)
 Interaction_vvcv(:,:,:)=(0.,0.)
 BS_eh_table_m1(:,:)=0
 !
 ! Preparation of BS_eh_table_m1
 do icv=1,BS_K_dim
   ic=BS_eh_table(icv,3)
   iv=BS_eh_table(icv,2)
   BS_eh_table_m1(iv,ic)=icv
 enddo
 !
 !  Parallel stuff <
 !
 !call live_timing('BSK',live_timing_steps)
 !
 !tot_n_K_elements=0
 !live_timing_accumulate=0
 !i8nc=ncpu
 !i8id=myid
 !
 !call pp_redux_wait
 !
 ! Parallel stuff >
 !
 call K_exchange(.false.,iq,1,Xk,O2x,O_x_dim)   ! I need this for O2x
 !
 ! Even if the cutoff coulomb interaction is used and this term should
 ! be in principle included, they are vanishing due to the q->0 in the
 ! oscillator and the finitness of qpg(1,1) term.
 O2x(1,:)=(0.,0.)
 ! 
 !  ********************************************
 !  *******          Main loop           *******
 !  ********************************************
 !
 ! Parallel stuff   <
 !
 !if (icv2<icv1) cycle
 !!
 !! ::: E/h energy window :::
 !!
 !if (.not.K_scatter(abs(BS_eh_E(icv1)),abs(BS_eh_E(icv2)),&
!&                       E_eh_range,S_eh_range)) cycle
 !!
 !! ::: Parallel Switch :::
 !!
 !tot_n_K_elements=tot_n_K_elements+1
 !if (mod(tot_n_K_elements-i8id,i8nc)/=1.and.ncpu>1)  cycle
 !!
 !! ::: Timing START :::
 !!
 !live_timing_accumulate=live_timing_accumulate+1
 !if (live_timing_accumulate==live_timing_step_size) then
 !  call live_timing(steps=1)
 !  live_timing_accumulate=0
 !endif
 !!
 !! ::: Timing END :::
 !!
 !iv2=BS_eh_table(icv2,2)
 !ic2=BS_eh_table(icv2,3)
 !i_sp2=spin(BS_eh_table(icv2,:))
 !
 ! Parallel stuff >
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
             Occ(i1)=zdotu(BS_n_g_W,BS_O(1,iOcc),1,BS_W(1,i1,1),1)  
#else
             Occ(i1)=cdotu(BS_n_g_W,BS_O(1,iOcc),1,BS_W(1,i1,1),1)
#endif
           enddo
         endif
         !
#if defined _DOUBLE
         Interaction_cccv(ic1-Ken%nbf,ic2-Ken%nbf,icv)=&
&                            zdotc(BS_n_g_W,O2x(1,icv),1,Occ,1)*4.*pi       
#else                                                         
         Interaction_cccv(ic1-Ken%nbf,ic2-Ken%nbf,icv)=&
&                            cdotc(BS_n_g_W,O2x(1,icv),1,Occ,1)*4.*pi       
#endif
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
             Ovv(i1)=zdotu(BS_n_g_W,BS_O(1,iOvv),1,BS_W(1,i1,1),1)
#else
             Ovv(i1)=cdotu(BS_n_g_W,BS_O(1,iOvv),1,BS_W(1,i1,1),1)
#endif
           enddo
         endif
#if defined _DOUBLE
         Interaction_vvcv(iv1-BS_bands(1)+1,iv2-BS_bands(1)+1,icv)=&
&                                        zdotc(BS_n_g_W,O2x(1,icv),1,Ovv,1)*4.*pi       
#else                                                         
         Interaction_vvcv(iv1-BS_bands(1)+1,iv2-BS_bands(1)+1,icv)=&
&                                        cdotc(BS_n_g_W,O2x(1,icv),1,Ovv,1)*4.*pi       
#endif
       enddo
     enddo
   enddo
   !
   !  Step 2: Here the code constructs the half-diagrams
   !          D1(icv,idoubles) and D2(icv,idoubles),
   !          starting from U_{cc1,c2v2} and U_{vv1,c2v2}
   !
   !          c  ===<===:===<===  c1              c  ===<===x===<===  c1
   !                    :                                   
   !                q  /:\                                    ==<===  c2
   !                    :                                    /
   !        D1          :===<===  c2          D2             ===>===  v2
   !                     \                                  : 
   !                      ==>===  v2                     q \:/
   !                                                        :
   !          v  ===>===x===>===  v1                 ===>===:===>===  v1
   !
   !          and the final simmetrized expression:
   !          C_{sd}=0.25*[D1+D2 -(c1<->c2) -(v1<->v2) +(c1,v1<->c2,v2)]
   !
   !
   do icv1=1,BS_K_dim
     ic1=BS_eh_table(icv1,3)
     iv1=BS_eh_table(icv1,2)
     do icv2=1,BS_K_dim
       ic2=BS_eh_table(icv2,3)
       iv2=BS_eh_table(icv2,2)
       !
       if ((iv1==iv2).or.(ic1==ic2)) then
         cycle
       endif
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
&                -Interaction_vvcv(iv-BS_bands(1)+1,iv1-BS_bands(1)+1,icv_loop)    !D2c
         endif
       enddo
     enddo
   enddo
   !
   BS_mat_doubles(:,:,:,:)=0.25*BS_mat_doubles(:,:,:,:)
   !
 endif
 !
 ! 
 ! Parallel stuff  <
 ! call pp_redux_wait(BS_mat_doubles)
 !
 ! Live Timing finalize (paralle stuff)
 !
 !if (mod(n_K_elements(myid+1),int(live_timing_step_size,8))/=0) call live_timing(steps=1)
 !call live_timing()
 !call pp_redux_wait
 !
 ! Parallel stuff >
 !
 ! CLEAN
 !
 !
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 io_err=ioBS_doubles(ID)
 !
 deallocate(Interaction_cccv,Interaction_vvcv)
 call mem_est('Interaction_cccv Interaction_vvcv')
 deallocate(BS_mat_doubles)
 call mem_est('BS_mat_doubles')
 deallocate(BS_eh_table_m1)
 call mem_est('BS_eh_table_m1')
 deallocate(O2x)
 call mem_est("O2x")
 deallocate(Ovv,Occ)
 call mem_est("O_RES_WS")
 deallocate(BS_W)
 call mem_est("BS_W")
 !
 !
end subroutine
