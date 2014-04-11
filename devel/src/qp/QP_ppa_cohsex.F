!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg,error
 use drivers,       ONLY:l_ppa,l_cohsex,l_sc_run,l_sc_srpa
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,&
&                        PAR_IND_Q,PAR_COM_QP_A2A,PAR_IND_Q_ID,PAR_IND_QP_ID,&
&                        PAR_COM_QP_A2A,PAR_Q_index,n_WF_bands_to_load,HEAD_QP_cpu
 use collision,     ONLY:ggwinfo,collision_reset
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,RD_CL,OP_RD,RD_CL_IF_END,&
&                        OP_WR_CL,io_X
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_nb,QP_dSc_steps,&
&                        QP_Sc,QP_n_states,QP_G_damp,QP_table,QP_dSc_delta,&
&                        COHSEX_use_empties,QP_W_partially_done,PPa_terminator
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use wave_func,     ONLY:WF
 use R_lattice,     ONLY:qindx_S,bz_samp,G_m_G
 use D_lattice,     ONLY:nsym,i_time_rev,i_space_inv,mag_syms
 use wrapper,       ONLY:M_by_V
 use interfaces,    ONLY:PARALLEL_global_indexes,PARALLEL_WF_index,WF_load,PARALLEL_WF_distribute
 use matrix_operate,ONLY:mat_transpose
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
#if defined _SC
 use SC,            ONLY:it_now,it_to_start
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
&                           i_qp_to_start,iq_to_start,is,iq_mem,X_range(2)
 complex(SP), allocatable ::W_(:),dc(:),eet_factor(:,:)
 type(ggwinfo)            ::isc,iscp
 integer          ::iv4(4),io_err,ID, timing_steps,qp_ID
 integer, external::ioX,QP_state_extract,ioQP
 logical, external::stop_now
 character(schlen)::ch,Sc_name
 logical          ::X_is_TR_rotated
 complex(SP)      ::local_rhotw(X%ng),pre_factor,sp_dummy
 real(SP)         ::eet_cutoff(n_sp_pol),E_kmq,f_kmq
 !
 ! Reset
 !
 call collision_reset(isc)
 call collision_reset(iscp)
 i_qp_to_start=1
 iq_to_start  =1
 QP_Sc        =cZERO
 !
 ! COHSEX: bands setup
 !
 if ((l_sc_srpa.or.l_cohsex  ).and.(.not.COHSEX_use_empties)) then
   QP_n_G_bands(2)=max(maxval(QP_table(:,:2)),n_met_bands)
 endif
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
   !if(.not.master_cpu) QP_Sc=cZERO
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
 ! WF distributed & load
 !=======================
 !
#if !defined _SC
 call PARALLEL_global_indexes(E,k,q,"Self_Energy")
#endif
 !
 ! Redefine iq_to_start to be CPU-dependent
 !
 do iqbz=1,q%nbz 
   if (PAR_IND_Q%element_1D(q%sstar(iqbz,1))) then
     iq_to_start=iqbz
     exit
   endif
 enddo
 !
#if !defined _SC
 call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_G_b,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
 call PARALLEL_WF_index( )
#endif
 call WF_load(WF,isc%ngrho,maxval(qindx_S(:,:,2)),(/1,n_WF_bands_to_load/),(/1,k%nibz/),&
#if defined _SC
&             title='-SC')
#else
&             title='-GW')
#endif
 !
#if defined _TIMING
 call timing('GW(ppa)',OPR='start')
#endif
 !
 ! Plasmon-Pole/Static interaction DB I/O
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioX(X,Xw,ID)
 if (io_err/=0.and.io_X) call error('Incomplete and/or broken PPA/Static diel. fun. database')
 !
 ! Test the spatial Inversion
 !   
 call WF_spatial_invertion(E,Xk)
 !
 ! ALLOCATION
 !------------
 !
 if (l_cohsex.or.l_sc_srpa) then
   if (io_X) call X_alloc('X',(/X%ng,X%ng,1/))
   allocate(dc(2))
 else
   if (io_X) call X_alloc('X',(/X%ng,X%ng,2/))
   allocate(W_(QP_dSc_steps),dc(QP_dSc_steps),stat=alloc_err)
   call mem_est("GW-E",(/QP_dSc_steps*2/),errors=(/alloc_err/))
   if(PPa_terminator) then
     allocate(eet_factor(X%ng,X%ng),stat=alloc_err)
     call mem_est("EET_factor",(/size(eet_factor)/),(/2*SP/),errors=(/alloc_err/))
   endif
 endif
 !
 allocate(isc%gamp(X%ng,X%ng),isc%rhotw(isc%ngrho),iscp%rhotw(isc%ngrho),stat=alloc_err)
 call mem_est("ISC-GAMP",(/X%ng**2+2*isc%ngrho/),errors=(/alloc_err/))
 !
 call PP_redux_wait
 !
 timing_steps=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)*&
&             PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1)*&
&             count( PAR_IND_G_b%element_1D(QP_n_G_bands(1):QP_n_G_bands(2)) )
 !
#if defined _SC
   call live_timing(trim(Sc_name)//'@it'//trim(intc(it_now)),timing_steps) 
#else
   if (l_ppa)    call live_timing(trim(Sc_name)//'W0 PPA',timing_steps)
   if (l_cohsex) call live_timing(trim(Sc_name)//'W0 COHSEX',timing_steps)
#endif
 !
 Q_loop: do iqbz=iq_to_start,q%nbz 
   !
   if (.not.PAR_IND_Q%element_1D(q%sstar(iqbz,1))) cycle
   !
   isc%qs(2:)=(/q%sstar(iqbz,1),q%sstar(iqbz,2)/)
   iqibz=isc%qs(2)
   iqs  =isc%qs(3)
   !
   if (iqibz/=isc%iqref) then
     !
     iq_mem=PAR_Q_index(iqibz)
     X_range=(/1,Xw%n_freqs/)
     if (.not.io_X) X_range=(/Xw%n_freqs*(iq_mem-1)+1,iq_mem*Xw%n_freqs/)
     !
     call scatterGamp(isc,'c')
     !
     ! I/O
     !
     if (io_X) then
       !
       if (iqbz==iq_to_start) call io_control(ACTION=OP_RD,COM=NONE,       SEC=(/1,2,2*iqibz+1/),ID=ID)
       if (q%nbz==1         ) call io_control(ACTION=OP_RD_CL,COM=NONE,    SEC=(/1,2,3/),ID=ID)
       if (iqbz> iq_to_start) call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
       !
       io_err=ioX(X,Xw,ID)
       !
     endif
     !
     ! Poles and Residuals
     !
#if defined _SC
     if (it_now==it_to_start.or.io_X) then
#endif
!$omp parallel do default(shared), private(i1,i2)
       do i1=1,X%ng
         do i2=1,X%ng
           if (l_ppa) then
             if (real(X_mat(i1,i2,X_range(1))/X_mat(i1,i2,X_range(2)))<=1._SP) then
               X_mat(i1,i2,X_range(2))=X%ppaE
             else
               X_mat(i1,i2,X_range(2))=real( sqrt(X_mat(i1,i2,X_range(1))/X_mat(i1,i2,X_range(2))-1) )
             endif
           else
             X_mat(i1,i2,X_range(1))=X_mat(i1,i2,X_range(1))*isc%gamp(i1,i2)
           endif
         enddo
       enddo
!$omp end parallel do
#if defined _SC
     endif
#endif
     !
     X_is_TR_rotated=.false.
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
   if (iqs>nsym/(i_time_rev+1) .and. (i_space_inv==0.or.mag_syms) .and..not.X_is_TR_rotated) then
     !
     ! Note (AF) that $omp directives are inside mat_transpose
     !
     call mat_transpose(X_mat(:,:,X_range(1)))
     if (l_ppa) call mat_transpose(X_mat(:,:,X_range(2)))
     X_is_TR_rotated=.true.
   endif
   !
   QP_loop: do i_qp=i_qp_to_start,QP_n_states
     !
     if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
     !
     ! i_qp must start from i_qp_to_start only during the first loop
     ! of the restart. Then it must be set to 1.
     !
     if (i_qp==QP_n_states) i_qp_to_start=1
     !
!#if defined _SC  
!     !
!     ! In OEP only vc matrix elements so ... no cc' no vv' 
!     ! (cv is not permitted by the order in QP_table)
!     !                       (c)         (v)
!     if (l_sc_srpa.and.(QP_table(i_qp,1)>E%nbf.or.QP_table(i_qp,2)<=E%nbf)) cycle
!#endif
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
!         do is=1,n_sp_pol
!           eet_cutoff(is)=minval(E%E(E%nbf+1,:,is))
!         enddo
!         eet_cutoff(1)=minval(eet_cutoff(:))
!         eet_factor=cZERO
!         if(master_cpu) forall(ig1=1:X%ng,ig2=1:X%ng) eet_factor(ig1,ig2)=isc%rhotw(G_m_G(ig1,ig2))
       else
         !
         sp_dummy = 0.0_SP
         !
!$omp parallel do default(shared), private(ig1,ig2), reduction(+:sp_dummy)
         do ig1=1,X%ng
           do ig2=1,X%ng
             sp_dummy=sp_dummy+2._SP*pi*isc%rhotw(G_m_G(ig1,ig2))*X_mat(ig1,ig2,X_range(1))
           enddo
         enddo
!$omp end parallel do
         !
         dc(1) = sp_dummy
         !
         ! Bug spotted by D.V. (April 2014). HEAD_QP_cpu is defined differently when
         ! the _SC flag is used or not. The point is that in SE calculations HEAD_QP_cpu is used
         ! in cases where no band loops are done (like here). In _SC instead 
         ! it is needed for a different purpose. This is why I use PAR_COM_QP_A2A%CPU_id in this case.
         !
#if defined _SC
         if (PAR_COM_QP_A2A%CPU_id==0) QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+dc(1)
#else
         if (HEAD_QP_cpu) QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+dc(1)
#endif
         !
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
       if (.not.PAR_IND_G_b%element_1D(ib)) cycle
       !
       if (q%sstar(iqbz,2)==1) call live_timing(steps=1)
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
!         if(PPa_terminator) then
!           forall(ig1=1:X%ng,ig2=1:X%ng)
!             eet_factor(ig1,ig2)=eet_factor(ig1,ig2)-isc%rhotw(ig1)*conjg(iscp%rhotw(ig2))
!           end forall
!         endif
         !
         forall (i4=1:QP_dSc_steps) W_(i4)=qp%E_bare(i_qp)+(i4-1._SP)*QP_dSc_delta+cI*QP_G_damp
         !
         E_kmq=E%E(isc%os(1),isc%os(2),isc%os(4))
         f_kmq=E%f(isc%os(1),isc%os(2),isc%os(4))
         !
         do i4=1,QP_dSc_steps
           !
           sp_dummy = 0.0_SP
           !
!$omp parallel do default(shared), private(ig1,ig2), reduction(+:sp_dummy)
           do ig1=1,X%ng
             do ig2=1,X%ng
               !
               !PPA_E=X%ppaE/real(X_mat(ig1,ig2,2))
               !PPA_R=-X_mat(ig1,ig2,1)/2.*PPA_E*isc%gamp(ig1,ig2)
               !
               sp_dummy=sp_dummy-4._SP/spin_occ*pi*isc%rhotw(ig1)*conjg(iscp%rhotw(ig2))*&
&                     X_mat(ig1,ig2,X_range(1))/2.*X%ppaE/real(X_mat(ig1,ig2,X_range(2)))*isc%gamp(ig1,ig2)*&
&                     ( (spin_occ-f_kmq)/(conjg(W_(i4))-E_kmq-X%ppaE/real(X_mat(ig1,ig2,X_range(2))))+&
&                     f_kmq/(conjg(W_(i4))-E_kmq+X%ppaE/real(X_mat(ig1,ig2,X_range(2)))))

             enddo
           enddo
!$omp end parallel do
           !
           dc(i4) = sp_dummy
           !
         enddo
         !
         QP_Sc(i_qp,:QP_dSc_steps)=QP_Sc(i_qp,:QP_dSc_steps)+dc(:QP_dSc_steps)
         !
       else
         !
         call M_by_V('N', X%ng, X_mat(:,:,X_range(1)), conjg(iscp%rhotw), local_rhotw)
         !
#if defined _OPENMP
         pre_factor=0.0_SP
!$omp parallel do default(shared), private(i1), reduction(+:pre_factor)
         do i1=1,X%ng
           pre_factor=pre_factor+isc%rhotw(i1)*local_rhotw(i1)
         enddo
!$omp end parallel do
#else
         pre_factor=sum(isc%rhotw(1:X%ng)*local_rhotw(1:X%ng))
#endif
         !
         ! SEX
         !
         dc(1)=-4._SP/spin_occ*pi*pre_factor
         !
         dc(1)=dc(1)*e%f(isc%os(1),isc%os(2),isc%os(4))
         !
         ! COH (when no empties are used the COH part is indeed calculated above)
         !
         if (COHSEX_use_empties) dc(2)=2._SP*pi*pre_factor
         !
         QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+dc(1)+dc(2)
         !
       endif
       !
     enddo ! loop on scattering states
     !
!     if(PPa_terminator) then
!       forall (i4=1:QP_dSc_steps) W_(i4)=qp%E_bare(i_qp)+(i4-1._SP)*QP_dSc_delta+cI*QP_G_damp
!       do ig1=1,X%ng
!         do ig2=1,X%ng
!           do i4=1,QP_dSc_steps
!             QP_Sc(i_qp,i4)=QP_Sc(i_qp,i4)+4._SP/spin_occ*pi*PPaR(ig1,ig2)*eet_factor(ig1,ig2)* &
!&                     QP_ppa_EET_terminator(W_(i4),E,isc%is,PPaP(ig1,ig2),ig1,ig2,isc%qs(2),eet_cutoff(1))
!             enddo
!           enddo
!         enddo
!     endif
     !
#if !defined _SC
     !
     ! ALL 2 ALL of QP_Sc
     !
     !call PP_redux_wait(QP_Sc,COMM=PAR_COM_QP_A2A%COMM)
     !if(.not.master_cpu) QP_Sc=cZERO
     !
!     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,- (i_qp+QP_n_states*(iqbz-1 ))/),ID=qp_ID)
!     io_err=ioQP('QP',qp,qp_ID)
     !
     ! In parallel runs only few cpus have access to the DB. Therefore, as QP_W_partially_done
     ! is defined  in ioQP I need to redefine it here for the CPUs not involved in the I/O
     !
!     QP_W_partially_done=.true.
!     if (i_qp==QP_n_states.and.iqbz==q%nbz) QP_W_partially_done=.false.
     !
     !if (stop_now(.FALSE.)) exit Q_loop
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
 deallocate(isc%gamp,isc%rhotw,iscp%rhotw,dc)
 if(l_ppa) then
   deallocate(W_)
!   if(PPa_Terminator) then
!       deallocate(eet_factor)
!       call mem_est("EET_factor")
!   endif
 endif
 call mem_est("ISC-GAMP GW-E")
#if defined _SC
 if (io_X) call X_alloc('X')
#else
 call X_alloc('X')
#endif
 !
#if defined _TIMING
 call timing('GW(ppa)',OPR='stop')
#endif
 !
 ! ALL 2 ALL of QP_Sc
 !
#if defined _TIMING
 call timing('GW(REDUX)',OPR='start')
#endif
 call PP_redux_wait(QP_Sc)
#if defined _TIMING
 call timing('GW(REDUX)',OPR='stop')
#endif
 !
#if !defined _SC
 call WF_free(WF)
#endif
 !
 call collision_reset(isc)
 call collision_reset(iscp)
 !
end subroutine
