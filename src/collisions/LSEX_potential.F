!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AC
!
subroutine LSEX_potential(q,X,Xw) 
 !
 ! Construct the screeend potential W(G=G'=0,q)
 !
 use nl_optics,           ONLY:NL_bands,Correlation,LSEX
 use collision_ext,       ONLY:W
 use LIVE_t,              ONLY:live_timing
 use pars,                ONLY:SP,cZERO,pi
 use frequency,           ONLY:w_samp,W_reset
 use R_lattice,           ONLY:bz_samp,qindx_S
 use D_lattice,           ONLY:DL_vol
 use electrons,           ONLY:levels,spin_occ
 use collision_el,        ONLY:elemental_collision,elemental_collision_free, &
&                              elemental_collision_alloc
 use X_m,                 ONLY:X_mat,X_ALLOC_elemental,X_t
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:OP_RD_CL,REP,VERIFY,NONE,RD_CL,OP_RD,RD_CL_IF_END,manage_action, &
&                              deliver_IO_error_message         
 use matrix_operate,      ONLY:hermitian
 use collision_ext,       ONLY:ng_oscll
 use stderr,              ONLY:intc
 use parallel_int,        ONLY:PP_wait
 !
#include<y_memory.h>
 !
 type(bz_samp), intent(in) :: q
 type(w_samp)              :: Xw
 type(X_t)                 :: X
 !
 ! Work Space
 !
 integer                    :: iqbz,iqibz,iqs,iqref
 integer                    :: io_err,ID,ig1,ig2,IO_ACT
 integer, external          :: io_X
 type(elemental_collision)  :: isc 
 !
 call PP_wait()   
 !
 ! The number of G in X(q,G,G') is read from input
 !
 X%ng      =ng_oscll
 isc%ngrho =X%ng  
 isc%iqref =0
 !
 call elemental_collision_free(isc)
 call elemental_collision_alloc(isc,NG=X%ng,GAMP_NG=(/X%ng,X%ng/),TITLE="Wlr") 
 !
 call X_ALLOC_elemental('X',(/X%ng,X%ng,1/))
 ! 
 call live_timing('[NL] W potential :',q%nbz)
 !
 if(Correlation==LSEX) then
   call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
   io_err=io_X(X,Xw,ID)
   if (io_err<0) call error('Incomplete and/or broken tatic diel. fun. database')
 endif
 !
 YAMBO_ALLOC(W,(q%nbz,ng_oscll,ng_oscll))
 iqref=0
 !
!$OMP WORKSHARE 
 W=cZERO
!$OMP END WORKSHARE 
 ! 
 do iqbz=1,q%nbz
   !
   isc%qs(2:)=(/q%sstar(iqbz,1),q%sstar(iqbz,2)/)
   iqibz=isc%qs(2)
   iqs  =isc%qs(3)
   !
   call scatter_Gamp(isc,'x')
   !
   ! The bare exchange v
   !
!$OMP WORKSHARE 
   forall(ig1=1:X%ng)
     W(iqbz,ig1,ig1)=-4._SP/spin_occ*pi*isc%gamp(ig1,ig1)
   end forall
!$OMP END WORKSHARE 
   !
   if(Correlation==LSEX) then
     !      
     call scatter_Gamp(isc,'c')
     !
     if(iqibz/=iqref) then
       !      
       IO_ACT=manage_action(RD_CL_IF_END,iqibz,1,q%nibz)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
       io_err=io_X(X,Xw,ID)
       !
       call deliver_IO_error_message(io_err,'PP/Em1s',STOP_it=.TRUE.)
       iqref=iqibz
       !
     endif
     !
     !  Screened part of the exchange  \tilde W = W - v
     !
!$OMP WORKSHARE 
     forall(ig1=1:X%ng,ig2=1:X%ng)
         W(iqbz,ig1,ig2)=W(iqbz,ig1,ig2)-4._SP/spin_occ*pi*X_mat(ig1,ig2,1)*isc%gamp(ig1,ig2)
     end forall
!$OMP END WORKSHARE 
     !
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 call elemental_collision_free(isc)
 call X_ALLOC_elemental('X')
 !
end subroutine LSEX_potential
