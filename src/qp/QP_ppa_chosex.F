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
subroutine QP_ppa_chosex(X,Xk,E,k,q,qp,Xw,GW_iter)
 !
 ! Plasomon Pole Correlation Self-Energy
 !
 use pars,          ONLY:SP,pi,HARTREE,schlen
 use memory_m,      ONLY:mem_est
 use stderr,        ONLY:intc
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin,n_met_bands
 use timing,        ONLY:live_timing
 use com,           ONLY:msg,error
 use drivers,       ONLY:Finite_Tel,l_ppa,l_chosex,l_sc_run
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use collision,     ONLY:ggwinfo,collision_reset
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,RD_CL,OP_RD,RD_CL_IF_END
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_nb,QP_dSc_steps,&
&                        QP_Sc,QP_n_states,QP_G_damp,QP_table,QP_dSc_delta,&
&                        CHOSEX_use_empties
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use wave_func,     ONLY:wf_load,wf_free
 use R_lattice,     ONLY:qindx_S,bz_samp,G_m_G
 use D_lattice,     ONLY:nsym,i_time_rev,i_space_inv
 !
 implicit none
 type(levels) ::E
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 type(w_samp) ::Xw
 integer      ::GW_iter
 !
 ! Work Space
 !
 integer :: i1,i2,i4,iqbz,iqibz,ib,ig1,ig2,alloc_err,iqs,iscs_save(2,4)
 real(SP),    allocatable :: PPaP(:,:)
 complex(SP), allocatable :: PPaR(:,:),E_(:),dc(:),PPaR_ws(:,:)
#if defined _DOUBLE
 complex(SP):: zdotc,zdotu
#else
 complex(SP):: cdotc,cdotu
#endif
 type(ggwinfo)    ::isc,iscp
 type(pp_indexes) ::px
 integer          ::iv4(4),io_err,ID
 integer, external::ioX,QP_state_extract
 character(schlen)::ch,Sc_name
 logical          ::PPaR_is_TR_rotated
 !
 ! Reset
 !
 call collision_reset(isc)
 call collision_reset(iscp)
 call pp_indexes_reset(px)
 !
 ! CHOSEX: bands setup
 !
 if (l_chosex.and.(.not.CHOSEX_use_empties)) QP_n_G_bands(2)=max(maxval(QP_table(:,:2)),n_met_bands)
 !
 ! Section
 !
 Sc_name='G'//trim(intc(GW_iter))
 if (l_sc_run) Sc_name='CHOSEX@it'//trim(intc(-GW_iter))
 !
 ch=trim(Sc_name)//'W0 : the P(lasmon) P(ole) A(pproximation)'
 if (l_chosex)  ch=trim(Sc_name)//'W0 : CHOSEX'
 !
 if (GW_iter==0) then
   call section('+',trim(ch))
 else if (GW_iter>0) then
   call section('=',trim(ch))
 endif
 !
 if (GW_iter==0) then
   call msg('r', '[GW/PPA] Bands range     :',QP_n_G_bands)
   if (l_ppa) then
     call msg('r', '[GW/PPA] G damping   [ev]:',QP_G_damp*HARTREE)
     call msg('r','')
   endif
   iv4=(/1,1,0,0/)
   do while(QP_state_extract(iv4)>0)
     write (ch,'(4(a,i3.3))') 'QP @ K ',iv4(1),' - ',iv4(2),' : b ',iv4(3),' - ',iv4(4)
     call msg('r',trim(ch))
   enddo
   call msg('r','')
 endif
 !
 call k_expand(k)
 !
 ! Dimensions...
 !
 isc%iqref=0
 isc%ngrho=X%ng
 if (l_chosex.and..not.CHOSEX_use_empties) isc%ngrho=maxval(G_m_G)
 iscp%ngrho=isc%ngrho
 !
 if (.not.l_sc_run) then
   !
   ! WFs
   !
   call wf_load(isc%ngrho,maxval(qindx_S(:,:,2)),&
&               (/1,max(QP_n_G_bands(2),QP_nb)/),(/1,k%nibz/),title='-Sc')
 endif
 !
 ! Plasmon-Pole/Static interaction DB I/O
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioX(X,Xw,ID)
 if (io_err/=0) call error('Incomplete and/or broken PPA/Static diel. fun. database')
 !
 ! GPL_EXCLUDE_START
 !
 ! Test the spatial Inversion
 !   
 call space_inv_by_wf(E,Xk)
 !
 ! GPL_EXCLUDE_END
 !
 ! ALLOCATION
 !------------
 !
 ! GPL_EXCLUDE_START
 !
 if (l_chosex) then
   !
   call X_alloc('X',(/X%ng,X%ng,1/))
   allocate(dc(2),PPaR_ws(X%ng,X%ng),PPaR(X%ng,X%ng),stat=alloc_err)
   call mem_est("PPaR",(/2*size(PPaR_ws)/),errors=(/alloc_err/))
 else
   !
   ! GPL_EXCLUDE_END
   !
   call X_alloc('X',(/X%ng,X%ng,2/))
   allocate(E_(QP_dSc_steps),dc(QP_dSc_steps),stat=alloc_err)
   call mem_est("GW-E",(/QP_dSc_steps*2/),errors=(/alloc_err/))
   allocate(PPaP(X%ng,X%ng),PPaR_ws(X%ng,X%ng),PPaR(X%ng,X%ng),stat=alloc_err)
   call mem_est("PPaP",(/size(PPaP)+2*size(PPaR_ws),2*size(PPaR)/),&
&               (/SP/),errors=(/alloc_err/))
   !
   ! GPL_EXCLUDE_START
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 allocate(isc%gamp(X%ng,X%ng),isc%rhotw(isc%ngrho),iscp%rhotw(isc%ngrho),stat=alloc_err)
 call mem_est("ISC-GAMP",(/X%ng**2+2*isc%ngrho/),errors=(/alloc_err/))
 !
 QP_Sc=cmplx(0.,0.,SP)
 !
 call par_indexes(px,(/q%nbz,QP_n_G_bands(2)/),(/1,QP_n_G_bands(1)/))
 call pp_redux_wait
 !
 if (l_ppa) then
   call live_timing(trim(Sc_name)//'W0 PPA',px%stps(myid+1)*QP_n_states) 
 else if (.not.l_sc_run) then
   call live_timing(trim(Sc_name)//'W0 CHOSEX',px%stps(myid+1)*QP_n_states) 
 else
   call live_timing(trim(Sc_name),px%stps(myid+1)*QP_n_states) 
 endif
 !
 do iqbz=1,q%nbz 
   !
   isc%qs(2:)=(/q%sstar(iqbz,1),q%sstar(iqbz,2)/)
   iqibz=isc%qs(2)
   iqs  =isc%qs(3)
   !
   if (iqibz/=isc%iqref) then
     call scatterGamp(isc,'c')
     !
     ! I/O
     !
     if (iqibz==1) call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1,2,3/),ID=ID)
     if (q%nbz==1) call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3/),ID=ID)
     if (iqibz> 1) call io_control(ACTION=RD_CL_IF_END,COM=NONE,&
&                                  SEC=(/2*isc%qs(2),2*isc%qs(2)+1/),ID=ID)
     io_err=ioX(X,Xw,ID)
     !
     ! Poles and Residuals
     !
     if (l_ppa) then
       forall(i1=1:X%ng,i2=1:X%ng) &
&            PPaP(i1,i2)=PPaPf(X_mat(i1,i2,1),X_mat(i1,i2,2),X%ppaE)
       forall(i1=1:X%ng,i2=1:X%ng) &
&            PPaR(i1,i2)=-X_mat(i1,i2,1)/2.*PPaP(i1,i2)*isc%gamp(i1,i2)
     else
       forall(i1=1:X%ng,i2=1:X%ng) PPaR(i1,i2)=X_mat(i1,i2,1)*isc%gamp(i1,i2)
     endif
     !
     PPaR_is_TR_rotated=.false.
     !
   endif
   !
   if (iqs>nsym/(i_time_rev+1) .and. i_space_inv == 0 .and..not.PPaR_is_TR_rotated) then
     PPaR_is_TR_rotated=.true.
     forall(i1=1:X%ng,i2=1:X%ng) PPaR_ws(i2,i1)=PPaR(i1,i2)
     PPaR(:,:)=PPaR_ws(:,:)
   endif
   !
   do i1=1,QP_n_states
     !
     isc%is=(/QP_table(i1,1),QP_table(i1,3),1,spin(QP_table(i1,:))/)
     isc%os(2:)=(/k%sstar(qindx_S(isc%is(2),iqbz,1),:),spin(QP_table(i1,:))/)
     iscp%is=(/QP_table(i1,2),QP_table(i1,3),1,spin(QP_table(i1,:))/)
     !
     isc%qs(1)=qindx_S(QP_table(i1,3),iqbz,2)
     iscp%qs=isc%qs
     !
     dc=cmplx(0.,0.,SP)
     !
     ! CHO (using completeness relation)
     !
     ! GPL_EXCLUDE_START
     !
     if (l_chosex.and..not.CHOSEX_use_empties) then
       iscs_save(1,: )=isc%os
       iscs_save(2,:3)=isc%qs
       isc%os=isc%is
       isc%qs=(/1,1,1/)
       call scatterBamp(isc)
       do ig1=1,X%ng
         do ig2=1,X%ng
           dc(1)=dc(1)+2.*pi*isc%rhotw(G_m_G(ig1,ig2))*PPaR(ig1,ig2)
         enddo
       enddo
       !
       QP_Sc(i1,:)=QP_Sc(i1,:)+dc(1)
       dc=cmplx(0.,0.,SP)
       isc%os=iscs_save(1,: )
       isc%qs=iscs_save(2,:3)
     endif
     !
     ! GPL_EXCLUDE_END
     !
     do ib=QP_n_G_bands(1),QP_n_G_bands(2)
       !
       if (.not.px%i2p(iqbz,ib)) cycle
       !
       isc%os(1)=ib
       iscp%os=isc%os
       call live_timing(steps=1)
       call scatterBamp(isc)
       iscp%rhotw=isc%rhotw
       if (any(isc%is/=iscp%is)) call scatterBamp(iscp)
       !
       dc=(0.,0.)
       !
       if (l_ppa) then
         !
         forall (i4=1:QP_dSc_steps) E_(i4)=qp%E_bare(i1)+&
&                                          (i4-1)*QP_dSc_delta+cmplx(0.,QP_G_damp,SP)
         !
         do ig1=1,X%ng
           do ig2=1,X%ng
             do i4=1,QP_dSc_steps
               dc(i4)=dc(i4)+4./spin_occ*pi*isc%rhotw(ig1)*&
&                     PPaR(ig1,ig2)*conjg(iscp%rhotw(ig2))*&
&                     (Gfm(E_(i4),E,isc%os,PPaP(ig1,ig2))+&
&                      Gfp(E_(i4),E,isc%os,PPaP(ig1,ig2)))
             enddo
           enddo
         enddo
         !
         QP_Sc(i1,:)=QP_Sc(i1,:)+dc(:)
         !
         ! GPL_EXCLUDE_START
         !
       else
         !
         do ig1=1,X%ng
           do ig2=1,X%ng
             !
             ! SEX
             !
             dc(1)=dc(1)-4./spin_occ*pi*isc%rhotw(ig1)*&
&                   PPaR(ig1,ig2)*conjg(iscp%rhotw(ig2))*e%f(isc%os(1),isc%os(2),1)
             !
             ! CHO
             !
             dc(2)=dc(2)+2.*pi*isc%rhotw(ig1)*PPaR(ig1,ig2)*conjg(iscp%rhotw(ig2))
             !
           enddo
         enddo
         !
         if (.not.CHOSEX_use_empties) dc(2)=(0._SP,0._SP)
         !
         QP_Sc(i1,:)=QP_Sc(i1,:)+dc(1)+dc(2)
         !
         ! GPL_EXCLUDE_END
         !
       endif
       !
     enddo
   enddo
 enddo
 !
 call live_timing()
 !
 call pp_redux_wait(QP_Sc)
 !
 ! CLEAN
 !
 deallocate(isc%gamp,isc%rhotw,iscp%rhotw,dc,PPaR_ws,PPaR)
 if (l_ppa) deallocate(E_,PPaP)
 call mem_est("PPaP ISC-GAMP GW-E")
 call X_alloc('X')
 if (.not.l_sc_run) call wf_free()
 call collision_reset(isc)
 call collision_reset(iscp)
 call pp_indexes_reset(px)
 !
 contains
   !
   pure function PPaPf(e1,e2,Eo)
     !
     implicit none
     real(SP)   :: PPaPf
     real(SP),intent(in)   :: Eo
     complex(SP),intent(in):: e1,e2
     PPaPf=Eo/sqrt(e1/e2-1.)
     if (real(e1/e2)<=1.d0) PPaPf=1.d0
     !
   end function
   !
   function Gfm(W,e,vs,Ep)
     !
     implicit none
     type(levels) ::e
     integer      ::vs(4)
     complex(SP)  ::W
     real(SP)     ::Ep
     complex(SP)  ::Gfm
     complex(SP)  ::lW !ws
     lW=W
     if (Finite_Tel) lW=conjg(W)
     Gfm=(real(spin_occ)-e%f(vs(1),vs(2),vs(4))+bose_f(Ep))/&
&        (lW-e%E(vs(1),vs(2),vs(4))-Ep)
     !
   end function
   !
   function Gfp(W,e,vs,Ep)
     !
     implicit none
     type(levels) ::e
     integer      ::vs(4)
     complex(SP)  ::W
     real(SP)     ::Ep
     complex(SP)  ::Gfp
     Gfp=(e%f(vs(1),vs(2),vs(4))+bose_f(Ep))/(conjg(W)-e%E(vs(1),vs(2),vs(4))+Ep)
     !
   end function
   !
end subroutine
