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
&                       O_phase,O_c_state,O_v_state,O_ng,&
&                       BS_eh_table,BS_n_g_exch,BS_identifier,O_n_v_states,&
&                       BS_K_dim,BS_eh_win,BS_blk_coord,&
&                       BSS_q0,O_n_scatt,BS_O,BS_columns,BS_mat
 use BS_doubles,   ONLY:BS_mat_doubles,BS_res_K_doubles,BS_K_doubles_dim,  &
                        BS_doubles_table,BS_bands_doubles,BS_doubles_E
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
&              icv,ic,iv,nc,nv,nv_doubles,nc_doubles,i_doubles
 complex(SP),allocatable ::  Interaction_cccv(:,:,:,:),Interaction_vvcv(:,:,:,:)
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
 integer    :: alloc_err
 complex(SP),allocatable::Ovv(:,:,:),Occ(:,:,:),Ocv(:,:,:)
 type(ggwinfo)          ::isc
 !
 ! I/O
 !
 integer           ::ioX_err,XID,io_err,ID      !ioX,ioX_err,XID         for screening
 integer, external ::ioX,ioBS_doubles           !ioBS_doubles,io_err,ID  for doubles
 !
 ! Davide doubles
 ! preparation lines:
 BS_res_K_doubles=.TRUE.
 BS_W_is_diagonal=.TRUE.
 BS_bands_doubles=BS_bands(2)+2
 !
 nc=BS_bands(2)-Ken%nbf
 nv=Ken%nbf-BS_bands(1)+1
 nc_doubles=BS_bands_doubles-Ken%nbf
 nv_doubles=Ken%nbf
 !
 BS_K_doubles_dim=(nc_doubles*nv_doubles)**2
 !
 Co=4._SP*pi/DL_vol/real(1,SP)
 !
 !
 call section('*','Bethe-Salpeter Kernel + DOUBLES')
 !
 ! Setups
 !
 BS_blk_coord=0
 !
 if (BS_K_doubles_dim==0) then
   call error(' Null BSE doubles kernel block dimension(s) found. Increase doubles range')
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
 call msg('rsn','[BSE] Doubles dimension      :',BS_K_doubles_dim)
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
 allocate(BS_W(BS_n_g_W,i1,1),stat=alloc_err)
 call mem_est('BS_W',(/size(BS_W)/),errors=(/alloc_err/))
 !
 allocate(isc%gamp(i1,BS_n_g_W))
 !
 if (.not.BS_res_K_doubles) then
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=XID)
   ioX_err=ioX(X,Xw,XID)
 endif
 !
 isc%qs(2)=1
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
 ! Load wave funtions
 !
 X%ng=X%ng_db           !     <---------     ???????
 !
 call wf_load(BS_n_g_W,1,(/1,BS_bands_doubles/),(/1,1/),&
&             space='R',title='-BSK')
 !
 ! 
 ! ****** Allocations *****
 !
 ! Allocation: Table and energies
 allocate(BS_doubles_table(BS_K_doubles_dim,4))
 call mem_est('BS_doubles_table',(/size(BS_doubles_table)/))
 BS_doubles_table(:,:)=0
 allocate(BS_doubles_E(BS_K_doubles_dim))
 call mem_est('BS_E_table',(/size(BS_doubles_E)*2/))
 BS_doubles_E(:)=(0.,0.)
 !
 ! Allocation: Oscillators
 allocate(Ovv(BS_n_g_W,nv,nv_doubles),Occ(BS_n_g_W,nc,nc_doubles))
 call mem_est('Occ',(/size(Occ)*2/))
 call mem_est('Ovv',(/size(Ovv)*2/))
 allocate(Ocv(BS_n_g_W,nc_doubles,nv_doubles))
 call mem_est('Ocv',(/size(Ocv)*2/))
 Occ(:,:,:)=(0.,0.)
 Ovv(:,:,:)=(0.,0.)
 Ocv(:,:,:)=(0.,0.)
 !
 ! Allocation: Interaction and BS_mat_doubles
 allocate(Interaction_cccv(nc,nc_doubles,nc_doubles,nv_doubles))
 allocate(Interaction_vvcv(nv,nv_doubles,nc_doubles,nv_doubles))
 call mem_est('Interaction_cccv',(/size(interaction_cccv)*2/))
 call mem_est('Interaction_vvcv',(/size(interaction_vvcv)*2/))
 allocate(BS_mat_doubles(BS_K_dim,BS_K_doubles_dim,8))
 call mem_est('BS_mat_doubles',(/size(BS_mat_doubles)*2/))
 BS_mat_doubles(:,:,:)=(0.,0.)
 Interaction_cccv(:,:,:,:)=(0.,0.)
 Interaction_vvcv(:,:,:,:)=(0.,0.)
 !
 !
 !***** Before the main loop *****
 !
 ! Doubles table
 i_doubles=0
 do iv1=1,nv_doubles
   do iv2=1,nv_doubles
     do ic1=nv_doubles+1,BS_bands_doubles
       do ic2=nv_doubles+1,BS_bands_doubles
         i_doubles=i_doubles+1
         BS_doubles_table(i_doubles,1)=iv1
         BS_doubles_table(i_doubles,2)=iv2
         BS_doubles_table(i_doubles,3)=ic1
         BS_doubles_table(i_doubles,4)=ic2
         BS_doubles_E(i_doubles)=Ken%E(ic1,1,1)+Ken%E(ic2,1,1)-Ken%E(iv1,1,1)-Ken%E(iv2,1,1)
       enddo
     enddo
   enddo
 enddo
 !
 !
 ! Oscillators construction
 call collision_reset(isc)
 isc%is(:)=1
 isc%os(:)=1
 isc%qs(:)=1
 isc%ngrho=BS_n_g_W
 isc%iqref=1
 allocate(isc%rhotw(BS_n_g_W))
 allocate(isc%gamp(1,BS_n_g_W))
 !
 do iv=BS_bands(1),Ken%nbf
   do iv1=1,nv_doubles
     isc%is(1)=iv1
     isc%os(1)=iv
     call scatterBamp(isc)
     forall(i1=1:BS_n_g_W) Ovv(i1,iv-BS_bands(1)+1,iv1)=isc%rhotw(i1)
   enddo
 enddo
 !
 do ic=1+Ken%nbf,BS_bands(2)
   do ic1=1+Ken%nbf,BS_bands_doubles
     isc%is(1)=ic
     isc%os(1)=ic1
     call scatterBamp(isc)
     forall(i1=1:BS_n_g_W) Occ(i1,ic-Ken%nbf,ic1-Ken%nbf)=isc%rhotw(i1)
   enddo
 enddo
 !
 do iv2=1,nv_doubles
   do ic2=1+Ken%nbf,BS_bands_doubles
     isc%is(1)=ic2
     isc%os(1)=iv2
     call scatterBamp(isc)
     forall(i1=1:BS_n_g_W) Ocv(i1,ic2-Ken%nbf,iv2)=conjg(isc%rhotw(i1))
   enddo
 enddo
 call collision_reset(isc)
 !
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
 ! Even if the cutoff coulomb interaction is used and this term should
 ! be in principle included, they are vanishing due to the q->0 in the
 ! oscillator and the finitness of qpg(1,1) term.
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
   !
   !  Step 1: Here the code constructs U_{cc1,c2v2} and U_{vv1,c2v2} 
   !          starting Ocv, Occ and Ovv
   !
   !          1.a: U_{c c1 c2 v2}
   !
   do ic=1,BS_bands(2)-Ken%nbf
     do ic1=1,BS_bands_doubles-nv_doubles
       !
       if (BS_W_is_diagonal) then
         forall(i1=1:BS_n_g_W) Occ(i1,ic,ic1)=Occ(i1,ic,ic1)*BS_W(i1,1,1)
       else
         !
         do i1=1,BS_n_g_W 
#if defined _DOUBLE
           Occ(i1,ic,ic1)=zdotu(BS_n_g_W,Occ(1,ic,ic1),1,BS_W(1,i1,1),1)  
#else
           Occ(i1,ic,ic1)=cdotu(BS_n_g_W,Occ(1,ic,ic1),1,BS_W(1,i1,1),1)
#endif
         enddo
       endif
       do iv2=1,nv_doubles
         do ic2=1,BS_bands_doubles-nv_doubles
           !
#if defined _DOUBLE
           Interaction_cccv(ic,ic1,ic2,iv2)=zdotc(BS_n_g_W,Ocv(1,ic2,iv2),1,Occ(1,ic,ic1),1)*4.*pi       
#else                                                         
           Interaction_cccv(ic,ic1,ic2,iv2)=cdotc(BS_n_g_W,Ocv(1,ic2,iv2),1,Occ(1,ic,ic1),1)*4.*pi       
#endif
         enddo
       enddo
     enddo
   enddo
   !
   !          1.b: U_{v v1 c2 v2}
   !
   do iv=1,Ken%nbf-BS_bands(1)+1
     do iv1=1,nv_doubles
       !
       if (BS_W_is_diagonal) then
         forall(i1=1:BS_n_g_W) Ovv(i1,iv,iv1)=Ovv(i1,iv,iv1)*BS_W(i1,1,1) 
       else
         !
         do i1=1,BS_n_g_W 
#if defined _DOUBLE
           Ovv(i1,iv,iv1)=zdotu(BS_n_g_W,Ovv(1,iv,iv1),1,BS_W(1,i1,1),1)
#else
           Ovv(i1,iv,iv1)=cdotu(BS_n_g_W,Ovv(1,iv,iv1),1,BS_W(1,i1,1),1)
#endif
         enddo
       endif
       do iv2=1,nv_doubles
         do ic2=1,BS_bands_doubles-nv_doubles
#if defined _DOUBLE
           Interaction_vvcv(iv,iv1,ic2,iv2)=zdotc(BS_n_g_W,Ocv(1,ic2,iv2),1,Ovv(1,iv,iv1),1)*4.*pi       
#else                                                         
           Interaction_vvcv(iv,iv1,ic2,iv2)=cdotc(BS_n_g_W,Ocv(1,ic2,iv2),1,Ovv(1,iv,iv1),1)*4.*pi
#endif
         enddo
       enddo
     enddo
   enddo 
   !
   !  Step 2: Here the code constructs the half-diagrams
   !          D1(icv,i_doubles) and D2(icv,i_doubles),
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
   !
   do i_doubles=1,BS_K_doubles_dim
     iv1=BS_doubles_table(i_doubles,1)
     iv2=BS_doubles_table(i_doubles,2)
     ic1=BS_doubles_table(i_doubles,3)-Ken%nbf
     ic2=BS_doubles_table(i_doubles,4)-Ken%nbf
     !
     do icv=1,BS_K_dim
       iv=BS_eh_table(icv,2)
       ic=BS_eh_table(icv,3)-Ken%nbf
       if (iv==iv1) then
         BS_mat_doubles(icv,i_doubles,1)=&
&               Interaction_cccv(ic,ic1,ic2,iv2)                      !D1
         BS_mat_doubles(icv,i_doubles,2)=&
&              -Interaction_cccv(ic,ic2,ic1,iv2)                      !D1c
       endif
       if (iv==iv2) then 
         BS_mat_doubles(icv,i_doubles,3)=&
&               Interaction_cccv(ic,ic1,ic2,iv1)                      !D1v
         BS_mat_doubles(icv,i_doubles,4)=&
&              -Interaction_cccv(ic,ic2,ic1,iv1)                      !D1cv
       endif
       if (ic==ic1) then
         BS_mat_doubles(icv,i_doubles,5)=&
&               Interaction_vvcv(iv-BS_bands(1)+1,iv1,ic2,iv2)        !D2
         BS_mat_doubles(icv,i_doubles,7)=&
               -Interaction_vvcv(iv-BS_bands(1)+1,iv2,ic2,iv1)        !D2v
       endif
       if (ic==ic2) then 
         BS_mat_doubles(icv,i_doubles,6)=&
&               Interaction_vvcv(iv-BS_bands(1)+1,iv1,ic1,iv2)        !D2c
         BS_mat_doubles(icv,i_doubles,8)=&
&              -Interaction_vvcv(iv-BS_bands(1)+1,iv2,ic1,iv1)        !D2cv
       endif
     enddo
   enddo
   !
   BS_mat_doubles(:,:,:)=0.25*BS_mat_doubles(:,:,:)
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
 deallocate(Occ,Ovv,Ocv)
 call mem_est("Occ Ovv Ocv")
 deallocate(BS_W)
 call mem_est("BS_W")
 deallocate(BS_doubles_table)
 call mem_est("BS_doubles_table")
 !
 !
end subroutine
