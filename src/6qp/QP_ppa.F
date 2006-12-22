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
 subroutine QP_ppa(X,Xk,en,k,q,qp,Xw,GW_iter)
!============================================
 use pars,          ONLY:SP,pi,HARTREE,schlen
 use stderr,        ONLY:string_pack
 use memory_m,      ONLY:mem_est
 use frequency,     ONLY:w_samp
#if defined PJ_SPIN
 use electrons,     ONLY:levels,spin_occ,n_spinor
#else
 use electrons,     ONLY:levels
#endif
 use timing,        ONLY:live_timing
 use com,           ONLY:msg
 use drivers,       ONLY:Finite_Tel
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use collision,     ONLY:ggwinfo,collision_reset
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,RD_CL,OP_RD,RD_CL_IF_END
 use QP_m,          ONLY:QP_t,pp_taylor_terms,QP_n_G_bands,QP_nb,QP_dSc_steps,&
&                        QP_Sc,QP_n_states,QP_G_damp,QP_table,QP_dSc_delta
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use parser_m,      ONLY:parser
 use wave_func,     ONLY:wf_load,wf_free
 use R_lattice,     ONLY:qindx_S,bz_samp,nqibz
 implicit none
 type(levels) ::en
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 type(w_samp) ::Xw
 integer      ::GW_iter
!ws
 integer :: i1,i2,i3,i4,iqbz,iqibz,ib,ig1,ig2,alloc_err
 real(SP):: PPaPtlr(q%nibz)
 real(SP),    allocatable :: PPaP(:,:),PPaPm(:,:),scconv(:)
 complex(SP), allocatable :: PPaR(:,:,:),PPaM(:),lrhotw(:),lsc(:,:,:),E(:),dc(:)
#if defined _DOUBLE
 complex(SP):: zdotc,zdotu
#else
 complex(SP):: cdotc,cdotu
#endif
 type(ggwinfo)    ::isc,iscp
 type(pp_indexes) ::px
 integer          ::gfmf,iv4(4),io_err,ID
 integer, external::ioX,QP_state_extract
 character(schlen)::ch,Sc_name
!
 call collision_reset(isc)
 call collision_reset(iscp)
 call pp_indexes_reset(px)
!--------------------------------------------------------------------------------
 if (GW_iter<9) write (Sc_name,'(a,i1.1)') 'G',GW_iter
 if (GW_iter>9) write (Sc_name,'(a,i2.2)') 'G',GW_iter
 ch=trim(string_pack(Sc_name,'W0 : the P(lasmon) P(ole) A(pproximation)'))
 if (GW_iter==0) then
   call section('+',trim(ch))
 else
   call section('=',trim(ch))
 endif
!=================================================================================
 if (GW_iter==0) then
   call msg('r', '[GW/PPA] Bands range     :',QP_n_G_bands)
   call msg('r', '[GW/PPA] G damping   [ev]:',QP_G_damp*HARTREE)
   call msg('r','')
   iv4=(/1,1,0,0/)
   do while(QP_state_extract(iv4)>0)
     write (ch,'(4(a,i3.3))') 'QP @ K ',iv4(1),' - ',iv4(2),' : b ',iv4(3),' - ',iv4(4)
     call msg('r',trim(ch))
   enddo
   call msg('r','')
 endif
 call k_expand(k)
!
! WFs
!==================
 call wf_load(X%ng,maxval(qindx_S(:,:,2)),&
&             (/1,max(QP_n_G_bands(2),QP_nb)/),(/1,k%nibz/),title='-Sc')
!
! Plasmon-Pole DB
!==================
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioX(X,Xw,ID)
 if (io_err/=0) return

!###########
!ALLOCATION
!################################################################################
 call X_alloc('X',(/X%ng,X%ng,2/))
!
 allocate(PPaP(X%ng,X%ng),PPaR(X%ng,X%ng,max(pp_taylor_terms,1)),stat=alloc_err)
 call mem_est("PPaP",(/size(PPaP)+2*size(PPaR)/),(/SP/),errors=(/alloc_err/))

 allocate(isc%gamp(X%ng,X%ng),isc%rhotw(X%ng),iscp%rhotw(X%ng),stat=alloc_err)
 call mem_est("ISC-GAMP",(/X%ng**2+2*X%ng/),errors=(/alloc_err/))

 allocate(E(QP_dSc_steps),dc(QP_dSc_steps),stat=alloc_err)
 call mem_est("PPA-E",(/QP_dSc_steps*2/),errors=(/alloc_err/))

 if (pp_taylor_terms>0) then
   allocate(PPaPm(X%ng,X%ng),scconv(pp_taylor_terms),&
&           PPaM(pp_taylor_terms),lrhotw(X%ng),lsc(pp_taylor_terms,QP_n_states,QP_dSc_steps),&
&           stat=alloc_err)
   lsc=cmplx(0.,0.,SP) 
   scconv=real(0.,SP)
   call mem_est("PPaPm",(/X%ng**2+pp_taylor_terms,pp_taylor_terms+X%ng+size(lsc)/),&
&               elements_kind=(/SP,2*SP/),errors=(/alloc_err,alloc_err/))
 endif
!################################################################################
!
 QP_Sc=cmplx(0.,0.,SP)

 isc%ngrho=X%ng
 isc%iqref=0

 call par_indexes(px,(/q%nbz,QP_n_G_bands(2)/),(/1,QP_n_G_bands(1)/))
 call pp_redux_wait

 call live_timing(trim(string_pack(Sc_name,'W0 PPA')),px%stps(myid+1)*QP_n_states)

 do iqbz=1,q%nbz
   isc%qs(2:)=(/q%sstar(iqbz,1),q%sstar(iqbz,2)/)
   iqibz=isc%qs(2)
   if (iqibz/=isc%iqref) then
     call scatterGamp(isc,'c')

     if (iqibz==1) call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1,2,3/),ID=ID)
     if (q%nbz==1) call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3/),ID=ID)
     if (iqibz> 1) call io_control(ACTION=RD_CL_IF_END,COM=NONE,&
&                                  SEC=(/2*isc%qs(2),2*isc%qs(2)+1/),ID=ID)

     io_err=ioX(X,Xw,ID)
     forall(i1=1:X%ng,i2=1:X%ng) &
&          PPaP(i1,i2)=PPaPf(X_mat(i1,i2,1),X_mat(i1,i2,2),X%ppaE)
     forall(i1=1:X%ng,i2=1:X%ng) &
&          PPaR(i1,i2,1)=-X_mat(i1,i2,1)/2.*PPaP(i1,i2)*isc%gamp(i1,i2)
     if (pp_taylor_terms>0) then
       forall(i1=1:X%ng,i2=1:X%ng) PPaPm(i1,i2)=PPaP(i1,i2)*abs(PPaR(i1,i2,1))
       PPaPtlr(isc%qs(2))=sum(PPaPm)/sum(abs(PPaR(:,:,1)))
       forall(i1=1:X%ng,i2=1:X%ng) PPaP(i1,i2)=PPaPtlr(isc%qs(2))-PPaP(i1,i2)
       do i3=2,pp_taylor_terms 
         forall(i1=1:X%ng,i2=1:X%ng) PPaR(i1,i2,i3)=PPaR(i1,i2,i3-1)*PPaP(i1,i2)
       enddo
     endif
   endif
   do i1=1,QP_n_states
#if defined PJ_SPIN
     isc%is=(/QP_table(i1,1),QP_table(i1,3),1,QP_table(i1,4)/)
     isc%os(2:)=(/k%sstar(qindx_S(isc%is(2),iqbz,1),:),QP_table(i1,4)/)
     iscp%is=(/QP_table(i1,2),QP_table(i1,3),1,QP_table(i1,4)/)
#else
     isc%is=(/QP_table(i1,1),QP_table(i1,3),1,1/)
     isc%os(2:)=(/k%sstar(qindx_S(isc%is(2),iqbz,1),:),1/)
     iscp%is=(/QP_table(i1,2),QP_table(i1,3),1,1/)
#endif
     isc%qs(1)=qindx_S(QP_table(i1,3),iqbz,2)
     iscp%qs=isc%qs
     dc=cmplx(0.,0.,SP)
     do ib=QP_n_G_bands(1),QP_n_G_bands(2)
       if (.not.px%i2p(iqbz,ib)) cycle
       isc%os(1)=ib
       iscp%os=isc%os
       call live_timing(steps=1)
       call scatterBamp(isc)
       iscp%rhotw=isc%rhotw
       if (any(isc%is/=iscp%is)) call scatterBamp(iscp)
       forall (i4=1:QP_dSc_steps) E(i4)=en%E(isc%is(1),isc%is(2),isc%is(4))+&
&                                       (i4-1)*QP_dSc_delta+cmplx(0.,QP_G_damp,SP)
       if (pp_taylor_terms>0) then
         gfmf=1
         do i3=1,pp_taylor_terms
           do ig1=1,X%ng
#if defined _DOUBLE
             lrhotw(ig1)=zdotu(X%ng,isc%rhotw,1,PPaR(1,ig1,i3),1)
#else
             lrhotw(ig1)=cdotu(X%ng,isc%rhotw,1,PPaR(1,ig1,i3),1)
#endif
           enddo
#if defined _DOUBLE
           PPaM(i3)=2.*pi*zdotc(X%ng,iscp%rhotw,1,lrhotw,1)
#else
           PPaM(i3)=2.*pi*cdotc(X%ng,iscp%rhotw,1,lrhotw,1)
#endif
           do i4=1,QP_dSc_steps
             lsc(i3,i1,i4)=lsc(i3,i1,i4)+PPaM(i3)*(&
&                          gfmf*Gfm(i3,E(i4),en,isc%os,PPaPtlr(isc%qs(2)))+&
&                               Gfp(i3,E(i4),en,isc%os,PPaPtlr(isc%qs(2))) )
           enddo
           gfmf=-gfmf
         enddo
       else
         dc=(0.,0.)
         do ig1=1,X%ng
           do ig2=1,X%ng
             do i4=1,QP_dSc_steps
#if defined PJ_SPIN
               dc(i4)=dc(i4)+4./spin_occ*pi*isc%rhotw(ig1)*&
#else
               dc(i4)=dc(i4)+2.*pi*isc%rhotw(ig1)*&
#endif
&                     PPaR(ig1,ig2,1)*conjg(iscp%rhotw(ig2))*&
&                     (Gfm(1,E(i4),en,isc%os,PPaP(ig1,ig2))+&
&                      Gfp(1,E(i4),en,isc%os,PPaP(ig1,ig2)))
             enddo
           enddo
         enddo
         QP_Sc(i1,:)=QP_Sc(i1,:)+dc(:)
       endif
     enddo
   enddo
 enddo
#if defined PJ_SPIN
 do i1=1,QP_n_states,n_spinor
   if (n_spinor==2) then
     QP_Sc(i1,:)  = QP_Sc(i1,:)+QP_Sc(i1+1,:)
     QP_Sc(i1+1,:)= (0.,0.)
   endif
 enddo
#endif

 call live_timing()

 if (pp_taylor_terms>0) then
   call pp_redux_wait(lsc)
   call pp_redux_wait
   do i1=pp_taylor_terms,2,-1
     do i2=1,i1-1
       lsc(i1,:,:)=lsc(i1,:,:)+lsc(i2,:,:)
     enddo
   enddo
   do i1=pp_taylor_terms,2,-1
     do i2=1,QP_n_states
       scconv(i1)=scconv(i1)+abs( (lsc(i1,i2,1)-lsc(i1-1,i2,1))/lsc(i1,i2,1))
     enddo
     scconv(i1)=scconv(i1)/real(QP_n_states)
   enddo
   QP_Sc(:,:)=lsc(pp_taylor_terms,:,:)
   if (pp_taylor_terms>1) call msg('r',&
&      'Average PPA convergence (2nd-nth Taylor Term) [o/o]:',scconv(2:))
 else
   call pp_redux_wait(QP_Sc)
 endif
!
! CLEAN
 deallocate(PPaP,PPaR,isc%gamp,isc%rhotw,iscp%rhotw,E,dc)
 if (pp_taylor_terms>0) deallocate(PPaPm,scconv,PPaM,lrhotw,lsc)
 call mem_est("PPaP ISC-GAMP PPA-E PPaPm")
 call X_alloc('X')
 call wf_free()
 call collision_reset(isc)
 call collision_reset(iscp)
 call pp_indexes_reset(px)

 contains

   pure function PPaPf(e1,e2,Eo)
!===============================
   implicit none
   real(SP)   :: PPaPf
   real(SP),intent(in)   :: Eo
   complex(SP),intent(in):: e1,e2
   PPaPf=Eo/sqrt(e1/e2-1.)
   if (real(e1/e2)<=1.d0) PPaPf=1.d0
   end function

   function Gfm(n,W,e,vs,Ep)
!===========================
   implicit none
   type(levels) ::e
   integer      ::vs(4)
   complex(SP)  ::W
   real(SP)     ::Ep
   integer      ::n
   complex(SP)  ::Gfm
   complex(SP)  ::lW !ws
   lW=W
   if (Finite_Tel) lW=conjg(W)
#if defined PJ_SPIN
   Gfm=(real(spin_occ)-e%f(vs(1),vs(2),vs(4))+bose_f(Ep))/&
&      (lW-e%E(vs(1),vs(2),vs(4))-Ep)**n
#else
   Gfm=(2.-e%f(vs(1),vs(2),vs(4))+bose_f(Ep))/(lW-e%E(vs(1),vs(2),vs(4))-Ep)**n
#endif
   end function

   function Gfp(n,W,e,vs,Ep)
!===========================
   implicit none
   type(levels) ::e
   integer      ::vs(4)
   complex(SP)  ::W
   real(SP)     ::Ep
   integer      ::n
   complex(SP)  ::Gfp
   Gfp=(e%f(vs(1),vs(2),vs(4))+bose_f(Ep))/(conjg(W)-e%E(vs(1),vs(2),vs(4))+Ep)**n
   end function

 end subroutine

