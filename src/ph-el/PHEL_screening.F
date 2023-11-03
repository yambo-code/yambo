!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PHEL_screening(E,q,DONE)
 !
 use pars,           ONLY:SP,cZERO,schlen,pi
 use com,            ONLY:fat_log
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,k_the_nearest,nqbz,bare_qpg
 use frequency,      ONLY:w_samp
 use X_m,            ONLY:X_mat,X_t,X_ALLOC_elemental
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:DUMP,NONE,REP,RD_CL_IF_END,manage_action,OP_RD,VERIFY,OP_RD_CL
 use LIVE_t,         ONLY:live_timing
 use ELPH,           ONLY:PH_qpt,elph_branches
 use PHEL,           ONLY:PH_Q,PH_energy
 use parallel_int,   ONLY:PARALLEL_index,PP_redux_wait
 use com,            ONLY:msg
 use parallel_m,     ONLY:PP_indexes,PP_indexes_reset,myid
 use vec_operate,    ONLY:iku_v_norm
 use wrapper,        ONLY:M_by_V,V_dot_V
 use stderr,         ONLY:intc,real2ch
 use OUTPUT,         ONLY:OUTPUT_driver
 !
#include<memory.h>
 !
 type(levels) ,intent(in) :: E
 type(bz_samp),intent(in) :: q
 logical                  :: DONE
 !
 ! Work Space
 !
 !... map
 integer      ::Q_local_map(PH_Q(2)),i_f,il
 real(SP)     ::q_dist(nqbz),min_q_modulus
 !
 !... functions
 integer , external ::io_X
 character(schlen)  ::Q_map_report
 !
 ! ...V's
 !
 complex(SP), allocatable :: W_static(:,:),W_dyn(:,:,:),V(:,:)
 !
 ! ...X's
 type(X_t)    ::X(2) ! ! 1:Xo 2:em1d 
 type(w_samp) ::Xw(2)
 complex(SP), allocatable :: EM1D(:,:,:),Xo(:,:,:)
 !
 ! I/O
 integer      ::io_err(PH_Q(2),2),i_db,ID(2),iq,iq_ph,IO_ACT
 !
 ! ...X x V's
 complex(SP), allocatable :: Xo_Ws(:,:),X_V(:,:),Xo_Wd(:,:),Ws_Xo(:,:)
 !
 ! ...Output and naming
 integer           ::ig,iw,iw_Wph(PH_Q(2),elph_branches(2))
 real(SP)          ::data(5)
 character(schlen) ::fname,desc(5)
 character(2)      ::prefix
 type(PP_indexes)  ::px
 complex(SP), allocatable :: Ws_Xo_Ws(:,:),V_Xo_Wd(:,:),V_X_V(:,:),V_Xo_Ws(:,:)
 !
 call section("=","Static-Screening test&correct")
 !=================================================
 !
 ! Correspondance PH_q <-> q%
 !----------------------------
 call k_ibz2bz(q,'i',.false.)
 min_q_modulus=10000.
 do iq=2,q%nbz
   if (iku_v_norm(q%ptbz(iq,:))<min_q_modulus) min_q_modulus=iku_v_norm(q%ptbz(iq,:))
 enddo
 do iq=PH_Q(1),PH_Q(2)
   Q_local_map(iq)=q%sstar(k_the_nearest(PH_qpt(iq,:),q%ptbz,q%nbz,i_err=io_err(iq,1),k_dist=q_dist),1)
   Q_map_report='PH_Q ['//trim(intc(iq))//']: '//trim(intc(Q_local_map(iq)))//' (IBZ) '//&
&               trim(real2ch(q_dist(Q_local_map(iq))/min_q_modulus))//' (distance/minimum Q modulus)'
   call msg('r',trim(Q_map_report))
 enddo
 call k_ibz2bz(q,'d',.false.)
 !
 ! DB's check 
 !-------------
 X(:)%whoami=1
 X(1)%KERNEL="IP"
 do i_db=1,2
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=DUMP,ID=ID(i_db))
   io_err(1,i_db)=io_X(X(i_db),Xw(i_db),ID(i_db))
 enddo
 if (any(io_err(1,:)<0)) then
   call warning("X and/or Xo databases not found. Static screening validation procedure skipped.")
   DONE=.FALSE.
   return
 endif
 do iq_ph=PH_Q(1),PH_Q(2)
   do i_db=1,2
     iq= Q_local_map(iq_ph)
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2*iq/),MODE=VERIFY,ID=ID(i_db))
     io_err(iq_ph,i_db)=io_X(X(i_db),Xw(i_db),ID(i_db))
   enddo
 enddo
 !
 ! Alloc
 !-------
 !... X (I/O)
 call X_ALLOC_elemental('X',(/X(1)%ng,X(1)%ng,Xw(1)%n_freqs/))
 !
 !... V's
 YAMBO_ALLOC(V,(PH_Q(2),X(1)%ng))
 YAMBO_ALLOC(W_static,(PH_Q(2),X(1)%ng))
 YAMBO_ALLOC(W_dyn,(PH_Q(2),X(1)%ng,Xw(1)%n_freqs))
 !
 !... X's (compute)
 YAMBO_ALLOC(EM1D,(X(1)%ng,X(1)%ng,Xw(1)%n_freqs))
 YAMBO_ALLOC(Xo,(X(1)%ng,X(1)%ng,Xw(1)%n_freqs))
 !
 !... X.V
 YAMBO_ALLOC(Xo_Ws,(X(1)%ng,Xw(1)%n_freqs))
 YAMBO_ALLOC(Ws_Xo,(X(1)%ng,Xw(1)%n_freqs))
 YAMBO_ALLOC(Xo_Wd,(X(1)%ng,Xw(1)%n_freqs))
 YAMBO_ALLOC(X_V,(X(1)%ng,Xw(1)%n_freqs))
 Xo_Ws=cZERO
 Ws_Xo=cZERO
 Xo_Wd=cZERO
 X_V  =cZERO
 !
 !... V.X.V (compute)
 !
 YAMBO_ALLOC(Ws_Xo_Ws,(PH_Q(2),Xw(1)%n_freqs))
 YAMBO_ALLOC(V_Xo_Ws,(PH_Q(2),Xw(1)%n_freqs))
 YAMBO_ALLOC(V_Xo_Wd,(PH_Q(2),Xw(1)%n_freqs))
 YAMBO_ALLOC(V_X_V,(PH_Q(2),Xw(1)%n_freqs))
 Ws_Xo_Ws=cZERO
 V_Xo_Ws=cZERO
 V_Xo_Wd=cZERO
 V_X_V  =cZERO
 !
 do iq_ph=PH_Q(1),PH_Q(2)
   !
   if (any(io_err(iq_ph,:)<0)) cycle
   !
   iq=Q_local_map(iq_Ph)
   !
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/Xw(1)%n_freqs/))
   !
   call live_timing('Potential(W)@Q'//trim(intc(iq_ph)),px%n_of_elements(myid+1))
   !                 ==========
   IO_ACT=manage_action(RD_CL_IF_END,iq,1,PH_Q(2))
   do i_db=1,2
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2*iq,2*iq+1/),MODE=VERIFY,ID=ID(i_db))
     io_err(iq_ph,i_db)=io_X(X(i_db),Xw(i_db),ID(i_db))
     if (i_db==1) Xo(:,:,:)=X_mat
     if (i_db==2) then
       do ig=1,X(1)%ng
         EM1D(ig,:,:)=4.*pi/bare_qpg(iq,ig)**2*X_mat(ig,:,:) ! 1 + v X
         EM1D(ig,ig,:)=EM1D(ig,ig,:)+1.
       enddo
     endif
   enddo
   do ig=1,X(1)%ng
     V(iq_ph,ig)=4.*pi/bare_qpg(iq,ig)
   enddo
   W_static(iq_ph,:) =cZERO
   W_dyn(iq_ph,:,:)  =cZERO
   call M_by_V('N',X(1)%ng,EM1D(:,:,1),V(iq_ph,:),W_static(iq_ph,:))
   W_static(iq_ph,:)=abs( W_static(iq_ph,:) )
   do iw=1,Xw(1)%n_freqs
     if (.not.px%element_1D(iw)) cycle
     call M_by_V('N',X(1)%ng,EM1D(:,:,iw),V(iq_ph,:),W_dyn(iq_ph,:,iw))
     call live_timing(steps=1)
   enddo
   !
   call PP_redux_wait(W_dyn)
   call live_timing( )
   !
   iw_Wph(iq_ph,:)=-1
   do il=elph_branches(1),elph_branches(2)
     do iw=1,Xw(1)%n_freqs-1
       if (real(Xw(1)%p(iw))<PH_energy(q,il,iq_ph).and.PH_energy(q,il,iq_ph)<real(Xw(1)%p(iw+1))) then
         iw_Wph(iq_ph,il)=iw
       endif
     enddo
   enddo
   !
   call live_timing('V.Xo.W@Q'//trim(intc(iq_ph)),px%n_of_elements(myid+1))
   !
   do iw=1,Xw(1)%n_freqs
     !
     if (.not.px%element_1D(iw)) cycle
     !
     call M_by_V('N',X(1)%ng,Xo(:,:,iw),W_static(iq_ph,:),Xo_Ws(:,iw))
     call M_by_V('T',X(1)%ng,Xo(:,:,iw),W_static(iq_ph,:),Ws_Xo(:,iw))
     call M_by_V('N',X(1)%ng,Xo(:,:,iw),W_dyn(iq_ph,:,iw),Xo_Wd(:,iw))
     call M_by_V('N',X(1)%ng,X_mat(:,:,iw),V(iq_ph,:),X_V(:,iw))
     !
     !...Ws_Xo_Ws (CPM)
     Ws_Xo_Ws(iq_ph,iw)=V_dot_V(X(1)%ng,W_static(iq_ph,:),Xo_Ws(:,iw))
     !
     !...V_Xo_Wd
     V_Xo_Wd(iq_ph,iw) =V_dot_V(X(1)%ng,V(iq_ph,:),Xo_Wd(:,iw))
     !
     !...V_X_V
     V_X_V(iq_ph,iw) =V_dot_V(X(1)%ng,V(iq_ph,:),X_V(:,iw))
     !
     !V_Xo_Ws+Ws_Xo_V
     V_Xo_Ws(iq_ph,iw)=(V_dot_V(X(1)%ng,V(iq_ph,:),Xo_Ws(:,iw))+V_dot_V(X(1)%ng,Ws_Xo(:,iw),V(iq_ph,:)))/2.
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call PP_redux_wait(Ws_Xo_Ws)
   call PP_redux_wait(V_Xo_Wd)
   call PP_redux_wait(V_Xo_Ws)
   call PP_redux_wait(V_X_V)
   !
 enddo 
 !
 call live_timing( )
 !
 ! Output
 !
 call live_timing('Output',Xw(1)%n_freqs*(PH_Q(2)-PH_Q(1)+1)*2)
 !
 do iq_ph=PH_Q(1),PH_Q(2)
   !
   if (any(io_err(iq_ph,:)<0)) cycle
   !
   do il=elph_branches(1),elph_branches(2)
     if (.not.fat_log) cycle
     fname="potentials_q"//trim(intc(iq_ph))//"_branch"//trim(intc(il))
     call OUTPUT_driver(trim(fname),action="open")
     desc(1)="|q+G|[a.u]"
     desc(2)="V_bare[a.u]"
     desc(3)="W_static[a.u]"
     desc(4)="W_dyn(W_ph)[a.u]" 
     do ig=1,X(1)%ng
       data(:4)=(/bare_qpg(iq_ph,ig),V(iq_ph,ig),W_static(iq_ph,ig),W_dyn(iq_ph,ig,iw_Wph(iq_ph,il))/)
       call OUTPUT_driver(trim(fname),TITLES=desc(:4),R_VALUES=data(:4))
       call OUTPUT_driver(trim(fname),action="write")
     enddo
     call OUTPUT_driver(trim(fname),action="close")
   enddo
   !
   do i_f=1,2
     !
     if (i_f==1) prefix="RE"
     if (i_f==2) prefix="IM"
     !
     fname="static_screening-"//prefix//"_q"//trim(intc(iq_ph))
     call OUTPUT_driver(trim(fname),action="open")
     !
     do iw=1,Xw(1)%n_freqs
       call OUTPUT_driver(trim(fname),TITLES=(/"Energy"/),R_VALUES=(/real(Xw(1)%p(iw))/),UNIT="eV")
       if (i_f==1) data(:3)= real((/V_X_V(iq_ph,iw),V_Xo_Ws(iq_ph,iw),Ws_Xo_Ws(iq_ph,iw)/))
       if (i_f==2) data(:3)= aimag((/V_X_V(iq_ph,iw),V_Xo_Ws(iq_ph,iw),Ws_Xo_Ws(iq_ph,iw)/))
       data(4) = data(1)/data(2)
       data(5) = data(1)/data(3)
       desc(1)=prefix//"[V.X.V]"
       desc(2)=prefix//"[V.Xo.Ws]"
       desc(3)=prefix//"[Ws.Xo.Ws]"
       desc(4)=prefix//"[X]/"//prefix//"[V.Xo.Ws]"
       desc(5)=prefix//"[X]/"//prefix//"[Ws.Xo.Ws]"
       call OUTPUT_driver(trim(fname),TITLES=desc(:5),R_VALUES=data(:5))
       call OUTPUT_driver(trim(fname),action="write")
       call live_timing( steps=1 )
     enddo
     call OUTPUT_driver(trim(fname),action="close")
     !
   enddo
   !
 enddo
 !
 call live_timing( )
 !
 ! Free
 call PP_indexes_reset(px)
 call X_ALLOC_elemental('X')
 YAMBO_FREE(V)
 YAMBO_FREE(W_static)
 YAMBO_FREE(W_dyn)
 YAMBO_FREE(EM1D)
 YAMBO_FREE(Xo)
 YAMBO_FREE(Xo_Ws)
 YAMBO_FREE(Ws_Xo)
 YAMBO_FREE(Xo_Wd)
 YAMBO_FREE(X_V)
 YAMBO_FREE(Ws_Xo_Ws)
 YAMBO_FREE(V_Xo_Ws)
 YAMBO_FREE(V_Xo_Wd)
 YAMBO_FREE(V_X_V)
 !
 DONE=.TRUE.
 !
end subroutine PHEL_screening
