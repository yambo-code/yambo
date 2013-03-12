!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine QP_ppa_cohsex(X,Xk,E,k,q,qp,Xw,GW_iter)
 !
 ! Plasmon Pole & COHSEX Correlation Self-Energy
 !
 use pars,          ONLY:SP,pi,schlen,cZERO,cI
 use units,         ONLY:HA2EV
 use memory_m,      ONLY:mem_est
 use stderr,        ONLY:intc
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin,n_met_bands,n_sp_pol
 use timing,        ONLY:live_timing
 use com,           ONLY:msg,error
 use drivers,       ONLY:Finite_Tel,l_ppa,l_cohsex,l_sc_run,l_sc_srpa
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset,master_cpu
 use interfaces,    ONLY:PARALLEL_index
 use collision,     ONLY:ggwinfo,collision_reset
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,RD_CL,OP_RD,RD_CL_IF_END,OP_WR_CL
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_nb,QP_dSc_steps,&
&                        QP_Sc,QP_n_states,QP_G_damp,QP_table,QP_dSc_delta,&
&                        COHSEX_use_empties,QP_W_partially_done,PPa_terminator
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use wave_func,     ONLY:WF_load,WF_free
 use R_lattice,     ONLY:qindx_S,bz_samp,G_m_G
 use D_lattice,     ONLY:nsym,i_time_rev,i_space_inv,mag_syms
 use wrapper,       ONLY:M_by_V
 use WF_distribute, ONLY:WF_states_setup
#if defined _SC
 use SC,            ONLY:it_now
#endif
 !
 implicit none
 type(levels) ::E
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 type(w_samp) ::Xw
 integer      ::GW_iter
 !
 complex(SP), external :: QP_ppa_EET_terminator
 !
 ! Work Space
 !
 integer                  ::i_qp,i1,i2,i4,iqbz,iqibz,ib,ig1,ig2,alloc_err,iqs,iscs_save(2,4),&
&                           i_qp_to_start,iq_to_start,is
 real(SP),    allocatable ::PPaP(:,:)
 complex(SP), allocatable ::PPaR(:,:),E_(:),dc(:),PPaR_ws(:,:),eet_factor(:,:)
 type(ggwinfo)            ::isc,iscp
 type(PP_indexes)         ::px
 integer          ::iv4(4),io_err,ID, timing_states,qp_ID
 integer, external::ioX,QP_state_extract,ioQP
 logical, external::stop_now
 character(schlen)::ch,Sc_name
 logical          ::PPaR_is_TR_rotated
 complex(SP)      ::local_rhotw(X%ng),pre_factor
 real(SP)         ::eet_cutoff(n_sp_pol)
 !
 ! Reset
 !
 call collision_reset(isc)
 call collision_reset(iscp)
 call PP_indexes_reset(px)
 i_qp_to_start=1
 iq_to_start  =1
 QP_Sc        =cZERO
 !
 ! COHSEX: bands setup
 !
 if ((l_sc_srpa.or.l_cohsex).and.(.not.COHSEX_use_empties)) &
&                                  QP_n_G_bands(2)=max(maxval(QP_table(:,:2)),n_met_bands)
 !
 ! Restart
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=VERIFY,ID=qp_ID)
 io_err=ioQP('QP',qp,qp_ID)
 !
 if (QP_W_partially_done.and.io_err>0) then
   !
   ! Assuming i_qp_to_start=QP_n_states I get ...
   !
   iq_to_start   = io_err/QP_n_states
   i_qp_to_start = io_err-(iq_to_start-1)*QP_n_states
   !
   ! If i_qp_to_start/QP_n_states I get ...
   !
   if (i_qp_to_start/=QP_n_states) then
     iq_to_start   = iq_to_start+1
     i_qp_to_start = io_err-(iq_to_start-1)*QP_n_states
   endif
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/-1/),ID=qp_ID)
   io_err=ioQP('QP',qp,qp_ID)
   !
   ! Only the master keeps in memory the QP_Sc in order 
   ! to avoid double countings in the final ALL 2 ALL
   !
   if(.not.master_cpu) QP_Sc=cZERO
   !
 endif
 !
 ! Section
 !
 Sc_name='G'//trim(intc(GW_iter))
 if (l_sc_run) Sc_name='COHSEX'
 !
 ch=trim(Sc_name)//'W0 : the P(lasmon) P(ole) A(pproximation)'
 if (l_cohsex)  ch=trim(Sc_name)//'W0 : COHSEX'
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
     call msg('r', '[GW/PPA] G damping   [ev]:',QP_G_damp*HA2EV)
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
 if (((l_sc_srpa.or.l_cohsex).and..not.COHSEX_use_empties).or.PPa_terminator) isc%ngrho=maxval(G_m_G)
 iscp%ngrho=isc%ngrho
 !
 ! Quasi-particle list
 ! (all processors have all qp-states)
 !
 call WF_states_setup(qp_to_load=(/1,QP_n_states/),par_loop=.FALSE.)
 !
 ! Scattering states are distributed
 ! 
 call WF_states_setup(b_to_load=QP_n_G_bands,qp_to_load=(/1,QP_n_states/),q_to_load=(/iq_to_start,q%nbz/),k=k)
 !
 ! Report
 !
 call WF_states_setup(b_to_load=QP_n_G_bands,report=.TRUE.)
 !
 ! WFs
 !
 call WF_load(isc%ngrho,maxval(qindx_S(:,:,2)),(/1,max(QP_n_G_bands(2),QP_nb)/),(/1,k%nibz/),title='-SC')
 !
 ! Plasmon-Pole/Static interaction DB I/O
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioX(X,Xw,ID)
 if (io_err/=0) call error('Incomplete and/or broken PPA/Static diel. fun. database')
 !
 ! Test the spatial Inversion
 !   
 call WF_spatial_invertion(E,Xk)
 !
 ! ALLOCATION
 !------------
 !
 if (l_cohsex.or.l_sc_srpa) then
   !
   call X_alloc('X',(/X%ng,X%ng,1/))
   allocate(dc(2),PPaR_ws(X%ng,X%ng),PPaR(X%ng,X%ng),stat=alloc_err)
   call mem_est("PPaR",(/2*size(PPaR_ws)/),errors=(/alloc_err/))
 else
   !
   call X_alloc('X',(/X%ng,X%ng,2/))
   allocate(E_(QP_dSc_steps),dc(QP_dSc_steps),stat=alloc_err)
   call mem_est("GW-E",(/QP_dSc_steps*2/),errors=(/alloc_err/))
   allocate(PPaP(X%ng,X%ng),PPaR_ws(X%ng,X%ng),PPaR(X%ng,X%ng),stat=alloc_err)
   call mem_est("PPaP",(/size(PPaP)+2*size(PPaR_ws),2*size(PPaR)/),&
&               (/SP/),errors=(/alloc_err/))
   if(PPa_terminator) then
     allocate(eet_factor(X%ng,X%ng),stat=alloc_err)
     call mem_est("EET_factor",(/size(eet_factor)/),(/2*SP/),errors=(/alloc_err/))
   endif
   !
 endif
 !
 allocate(isc%gamp(X%ng,X%ng),isc%rhotw(isc%ngrho),iscp%rhotw(isc%ngrho),stat=alloc_err)
 call mem_est("ISC-GAMP",(/X%ng**2+2*isc%ngrho/),errors=(/alloc_err/))
 !
 ! Parallel Indexes & LIVE-TIMING STEPS
 !
 call PARALLEL_index(px,(/q%nbz,QP_n_G_bands(2)/),(/iq_to_start,QP_n_G_bands(1)/))
 !
 call PP_redux_wait
 !
#if defined _SC
 if (l_sc_run) then
   timing_states=QP_n_states*px%n_of_elements(myid+1)
   if (l_sc_srpa) timing_states=E%nbf*(maxval(QP_table(:,2))-E%nbf)*maxval(QP_table(:,3))*px%n_of_elements(myid+1)
   call live_timing(trim(Sc_name)//'@it'//trim(intc(it_now)),timing_states) 
 else 
#endif
   !
   timing_states=px%n_of_elements(myid+1)*(QP_n_states-i_qp_to_start+1)
   !
   if (l_ppa)    call live_timing(trim(Sc_name)//'W0 PPA',timing_states)
   if (l_cohsex) call live_timing(trim(Sc_name)//'W0 COHSEX',timing_states)
#if defined _SC
 endif
#endif
 !
 Q_loop: do iqbz=iq_to_start,q%nbz 
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
     if (iqbz==iq_to_start) call io_control(ACTION=OP_RD,COM=NONE,       SEC=(/1,2,2*iqibz+1/),ID=ID)
     if (q%nbz==1         ) call io_control(ACTION=OP_RD_CL,COM=NONE,    SEC=(/1,2,3/),ID=ID)
     if (iqbz> iq_to_start) call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
     io_err=ioX(X,Xw,ID)
     !
     ! Poles and Residuals
     !
     if (l_ppa) then
       forall(i1=1:X%ng,i2=1:X%ng) PPaP(i1,i2)=PPaPf(X_mat(i1,i2,1),X_mat(i1,i2,2),X%ppaE)
       forall(i1=1:X%ng,i2=1:X%ng) PPaR(i1,i2)=-X_mat(i1,i2,1)/2._SP*PPaP(i1,i2)*isc%gamp(i1,i2)
     else
       forall(i1=1:X%ng,i2=1:X%ng) PPaR(i1,i2)=X_mat(i1,i2,1)*isc%gamp(i1,i2)
     endif
     !
     PPaR_is_TR_rotated=.false.
     !
   endif
   !
   ! This additional rotation of the PP residuals arised from the particular
   ! case when TR is present but not the spatial inversion.
   ! In this case, indeed, 
   !
   !   X(-q,G,G') = X(q,-G',-G)
   !
   ! While the -1 is introduced in the collisions the reflection of the
   ! matrix must be done here.
   !
   if (iqs>nsym/(i_time_rev+1) .and. (i_space_inv==0.or.mag_syms) .and..not.PPaR_is_TR_rotated) then
     PPaR_is_TR_rotated=.true.
     forall(i1=1:X%ng,i2=1:X%ng) PPaR_ws(i2,i1)=PPaR(i1,i2)
     PPaR(:,:)=PPaR_ws(:,:)
   endif
   !
   QP_loop: do i_qp=i_qp_to_start,QP_n_states
     !
     ! i_qp must start from i_qp_to_start only during the first loop
     ! of the restart. Then it must be set to 1.
     !
     if (i_qp==QP_n_states) i_qp_to_start=1
     !
#if defined _SC  
     !
     ! In OEP only vc matrix elements so ... no cc' no vv' 
     ! (cv is not permitted by the order in QP_table)
     !                       (c)         (v)
     if (l_sc_srpa.and.(QP_table(i_qp,1)>E%nbf.or.QP_table(i_qp,2)<=E%nbf)) cycle
#endif
     !
     isc%is=(/QP_table(i_qp,1),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     isc%os(2:)=(/k%sstar(qindx_S(isc%is(2),iqbz,1),:),spin(QP_table(i_qp,:))/)
     isc%qs(1)=qindx_S(QP_table(i_qp,3),iqbz,2)
     !
     iscp%is=(/QP_table(i_qp,2),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     iscp%qs=isc%qs
     !
     dc=cZERO
     !
     ! COH (using completeness relation)
     !
     if (((l_sc_srpa.or.l_cohsex).and..not.COHSEX_use_empties).or.PPa_terminator) then
       !
       iscs_save(1,: )=isc%os
       iscs_save(2,:3)=isc%qs
       isc%os=(/QP_table(i_qp,2),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
       isc%qs=(/1,1,1/)
       call scatterBamp(isc)
       !
       if(PPa_terminator) then
         do is=1,n_sp_pol
           eet_cutoff(is)=minval(E%E(E%nbf+1,:,is))
         enddo
         eet_cutoff(1)=minval(eet_cutoff(:))
         eet_factor=cZERO
         if(master_cpu) forall(ig1=1:X%ng,ig2=1:X%ng) &
&          eet_factor(ig1,ig2)=isc%rhotw(G_m_G(ig1,ig2))
       else
         do ig1=1,X%ng
           do ig2=1,X%ng
             dc(1)=dc(1)+2._SP*pi*isc%rhotw(G_m_G(ig1,ig2))*PPaR(ig1,ig2)
           enddo
         enddo
         if (master_cpu) QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+dc(1)
         dc=cZERO
         !
       endif
       !
       isc%os=iscs_save(1,: )
       isc%qs=iscs_save(2,:3)
       !
     endif
     !
     do ib=QP_n_G_bands(1),QP_n_G_bands(2)
       !
       if (.not.px%element_2D(iqbz,ib)) cycle
       call live_timing(steps=1)
       !
       isc%os(1)=ib
       !
       call scatterBamp(isc)
       iscp%os=isc%os
       !
       iscp%rhotw=isc%rhotw
       if (any(isc%is/=iscp%is)) call scatterBamp(iscp)
       !
       dc=cZERO
       !
       if (l_ppa) then
         !
         if(PPa_terminator) then
           forall(ig1=1:X%ng,ig2=1:X%ng)
             eet_factor(ig1,ig2)=eet_factor(ig1,ig2)-isc%rhotw(ig1)*conjg(iscp%rhotw(ig2))
           end forall
         endif
         !
         forall (i4=1:QP_dSc_steps) E_(i4)=qp%E_bare(i_qp)+&
&                                          (i4-1._SP)*QP_dSc_delta+cI*QP_G_damp
         !
         do ig1=1,X%ng
           do ig2=1,X%ng
             do i4=1,QP_dSc_steps
               dc(i4)=dc(i4)+4._SP/spin_occ*pi*isc%rhotw(ig1)*PPaR(ig1,ig2)*conjg(iscp%rhotw(ig2))*&
&                     (Gfm(E_(i4),E,isc%os,PPaP(ig1,ig2))+Gfp(E_(i4),E,isc%os,PPaP(ig1,ig2)))
             enddo
           enddo
         enddo
         !
         QP_Sc(i_qp,:QP_dSc_steps)=QP_Sc(i_qp,:QP_dSc_steps)+dc(:QP_dSc_steps)
         !
       else
         !
         call M_by_V('N', X%ng, PPaR, conjg(iscp%rhotw), local_rhotw)
         !
         pre_factor=sum(isc%rhotw(1:X%ng)*local_rhotw(1:X%ng))
         !
         ! SEX
         !
         dc(1)=-4._SP/spin_occ*pi*pre_factor
         !
         dc(1)=dc(1)*e%f(isc%os(1),isc%os(2),isc%os(4))
         !
         ! COH
         !
         if (COHSEX_use_empties) dc(2)=2._SP*pi*pre_factor
         !
         QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+dc(1)+dc(2)
         !
       endif
       !
     enddo ! loop on scattering states
     !
     if(PPa_terminator) then
       forall (i4=1:QP_dSc_steps) E_(i4)=qp%E_bare(i_qp)+(i4-1._SP)*QP_dSc_delta+cI*QP_G_damp
       do ig1=1,X%ng
         do ig2=1,X%ng
           do i4=1,QP_dSc_steps
             QP_Sc(i_qp,i4)=QP_Sc(i_qp,i4)+4._SP/spin_occ*pi*PPaR(ig1,ig2)*eet_factor(ig1,ig2)* &
&                     QP_ppa_EET_terminator(E_(i4),E,isc%is,PPaP(ig1,ig2),ig1,ig2,isc%qs(2),eet_cutoff(1))
             enddo
           enddo
         enddo
     endif
     !
#if !defined _SC
     !
     ! ALL 2 ALL of QP_Sc
     !
     call PP_redux_wait(QP_Sc)
     if(.not.master_cpu) QP_Sc=cZERO
     !
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,- (i_qp+QP_n_states*(iqbz-1 ))/),ID=qp_ID)
     io_err=ioQP('QP',qp,qp_ID)
     !
     ! In parallel runs only few cpus have access to the DB. Therefore, as QP_W_partially_done
     ! is defined  in ioQP I need to redefine it here for the CPUs not involved in the I/O
     !
     QP_W_partially_done=.true.
     if (i_qp==QP_n_states.and.iqbz==q%nbz) QP_W_partially_done=.false.
     !
     if (stop_now(.FALSE.)) exit Q_loop
#endif
     !
   enddo QP_loop
   !
 enddo Q_loop 
 !
 call live_timing()
 !
 ! CLEAN
 !
 deallocate(isc%gamp,isc%rhotw,iscp%rhotw,dc,PPaR_ws,PPaR)
 if(l_ppa) then
   deallocate(E_,PPaP)
   if(PPa_Terminator) then
       deallocate(eet_factor)
       call mem_est("EET_factor")
   endif
 endif
 call mem_est("ISC-GAMP GW-E")
 call X_alloc('X')
 if (l_cohsex.or.l_sc_srpa) then
   call mem_est("PPaR")
 else
   call mem_est("PPaP")
 endif
 !
#if defined _SC 
 !
 ! ALL 2 ALL of QP_Sc
 !
 call PP_redux_wait(QP_Sc)
 !
#else
 !
 call WF_free()
 !
#endif
 !
 call collision_reset(isc)
 call collision_reset(iscp)
 call PP_indexes_reset(px)
 !
 contains
   !
   pure function PPaPf(e1,e2,Eo)
     !
     implicit none
     real(SP)   :: PPaPf
     real(SP),   intent(in)   :: Eo
     complex(SP),intent(in)   :: e1,e2
     PPaPf=Eo/sqrt(e1/e2-1._SP)
     if (real(e1/e2)<=1._SP) PPaPf=1._SP
     !
   end function
   !
   function Gfm(W,e,vs,Ep)
     !
     implicit none
     type(levels), intent(in) ::e
     integer     , intent(in) ::vs(4)
     complex(SP) , intent(in) ::W
     real(SP)    , intent(in) ::Ep
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
     type(levels), intent(in) ::e
     integer,      intent(in) ::vs(4)
     complex(SP),  intent(in) ::W
     real(SP),     intent(in) ::Ep
     complex(SP)  ::Gfp
     Gfp=(e%f(vs(1),vs(2),vs(4))+bose_f(Ep))/(conjg(W)-e%E(vs(1),vs(2),vs(4))+Ep)
     !
   end function
   !
end subroutine
