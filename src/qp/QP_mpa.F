!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DAL, AF, AM
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
subroutine QP_mpa(X,Xk,E,k,q,qp,Xw,GW_iter)
 !
 ! Multi-pole approximation to the Correlation Self-Energy
 !
 use pars,          ONLY:SP,DP,pi,schlen,cZERO,cI
 use units,         ONLY:HA2EV
 use stderr,        ONLY:intc
 use frequency,     ONLY:w_samp ! Dario: I have to import the file from my repository 
 use electrons,     ONLY:levels,spin_occ,spin,n_met_bands,n_sp_pol
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg,error
 use drivers,       ONLY:l_ppa,l_sc_run
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,&
&                        PAR_IND_Q,PAR_IND_Q_ID,PAR_IND_QP_ID,&
&                        PAR_Q_index,n_WF_bands_to_load,HEAD_QP_cpu
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,RD_CL,OP_RD,RD_CL_IF_END,&
&                        io_RESPONSE,deliver_IO_error_message
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_dSc_steps,&
&                        QP_Sc,QP_n_states,QP_G_damp,QP_table,QP_dSc_delta
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use wave_func,     ONLY:WF
 use R_lattice,     ONLY:qindx_S,bz_samp,G_m_G
 use D_lattice,     ONLY:nsym,i_time_rev,i_space_inv,mag_syms
 use wrapper,       ONLY:M_by_V
 use interfaces,    ONLY:PARALLEL_global_indexes,PARALLEL_WF_index,WF_load,WF_free,&
&                        PARALLEL_WF_distribute,QP_state_print
 use matrix_operate,ONLY:mat_transpose,mat_transpose_r
 use timing_m,      ONLY:timing
 !
! use mod_exact_3p   ! Dario: module with exact solution up to 3 poles in the MPA
 use mod_sampling   ! Dario: module with the frequency sampling in the MPA
 !
#include<memory.h>
 !
 type(levels) ::E
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 type(w_samp) ::Xw
 integer      ::GW_iter
 !
 ! Work Space
 !
 integer                  :: i_qp,i_w,iqbz,iqibz,ib,ig1,ig2,alloc_err,iqs,iscs_save(2,4),&
&                            i_qp_to_start,iq_to_start,is,iq_mem,X_range(2),io_err,ID,timing_steps,&
&                            cond_g, cond_b
 complex(SP), allocatable :: W_(:),dc(:),eet_factor(:,:)
 type(elemental_collision):: isc,iscp
 character(schlen):: ch,SECTION_name
 logical          :: X_is_TR_rotated,l_X_alloc
 real(SP)         :: eet_cutoff0(n_sp_pol),E_kmq,f_kmq,eet_cutoff1(n_sp_pol)
 complex(SP)      :: local_rhotw(X%ng),pre_factor
 complex(DP)      :: dp_dummy,ctmp
 !
 integer                  :: i_np,npoles ! Dario: mpa variables
 !complex(SP), allocatable :: MPA_E_mat(:,:,:)
 complex(SP), allocatable :: MPA_R_mat(:,:,:)
 real(SP),    allocatable :: MPA_E_mat(:,:,:)
 real(SP),    allocatable :: MPA_freqs(:)
 complex(SP), allocatable :: MPA_pols(:),MPA_Q(:)
 !
 integer, external::io_X,QP_state_extract
 !
 npoles=1;
 ! Reset & checks
 !
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp)
 i_qp_to_start=1
 iq_to_start  =1
 QP_Sc        =cZERO
 !
 ! Section
 !
 SECTION_name=''
 if (.not.l_sc_run) then
   SECTION_name='G'//trim(intc(GW_iter))
   SECTION_name=trim(SECTION_name)//'W0 (W MPA)'
 endif
 !
 if (GW_iter==0) call section('+',trim(SECTION_name))
 if (GW_iter> 0) call section('=',trim(SECTION_name))
 !
 if (GW_iter==0) then
   call msg('r', '[  GW  ] Bands range     :',QP_n_G_bands)
   call msg('r', '[GW/MPA] G damping   [ev]:',QP_G_damp*HA2EV)
   call msg('r','')
   ch=trim(SECTION_name)
   !
   call QP_state_print( )
   !
 endif
 !
 call k_expand(k)
 !
 ! WF distributed & load
 !=======================
 !
 if ( .not.l_sc_run ) call PARALLEL_global_indexes(E,k,q,"Self_Energy")
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
 if( .not.l_sc_run ) then
   call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_G_b,CLEAN_UP=.TRUE.)
   call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
   call PARALLEL_WF_index( )
 endif
 !
 ch='-GW'
 !
 ! wf and collisions dimension
 !-----------------------------
 isc%ngrho=X%ng
 !
 call WF_load(WF,isc%ngrho,maxval(qindx_S(:,:,2)),(/1,n_WF_bands_to_load/),(/1,k%nibz/),title=trim(ch))
 !
 ! Elemental Collisions
 !====================== 
 isc%iqref=0
 call elemental_collision_alloc(isc,NG=isc%ngrho,NG_GAMP=(/X%ng,X%ng/),TITLE="GW")
 call elemental_collision_alloc(iscp,NG=isc%ngrho,TITLE="GW")
 !
 call timing('GW(mpa)',OPR='start')
 !
 ! Plasmon-Pole/Static interaction DB I/O
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=io_X(X,Xw,ID)
 if (io_err<0.and.io_RESPONSE) call error('Incomplete and/or broken MPA/Static diel. fun. database')
 !
 ! Test the spatial Inversion
 !   
 call WF_spatial_inversion(E,Xk)
 !
 ! ALLOCATION
 !------------
 !
 if (io_RESPONSE) call X_alloc('X',(/X%ng,X%ng,2/))
 allocate(W_(QP_dSc_steps))
 allocate(dc(QP_dSc_steps),stat=alloc_err)
 YAMBO_ALLOC(MPA_E_mat,(X%ng,X%ng,npoles))  ! Dario: allocating all the MPA arrays
 YAMBO_ALLOC(MPA_R_mat,(X%ng,X%ng,npoles))
 YAMBO_ALLOC(MPA_pols,(2*npoles))
 YAMBO_ALLOC(MPA_freqs,(2*npoles))
 YAMBO_ALLOC(MPA_Q,(2*npoles))
 call HPPS_all(npoles,2*(X%ppaE),MPA_freqs) ! Dario: sampling the frequency domain
 !
 call PP_redux_wait
 !
 timing_steps=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)*&
&             PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1)*&
&             count( PAR_IND_G_b%element_1D(QP_n_G_bands(1):QP_n_G_bands(2)) )
 !
 ch=trim(SECTION_name)
 call live_timing(trim(ch),timing_steps)
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
     if (.not.io_RESPONSE) X_range=(/Xw%n_freqs*(iq_mem-1)+1,iq_mem*Xw%n_freqs/)
     !
     call scatter_Gamp(isc,'c')
     !
     ! I/O
     !
     if (io_RESPONSE) then
       !
       if (iqbz==iq_to_start) then
         call io_control(ACTION=OP_RD,COM=NONE,       SEC=(/1,2,2*iqibz+1/),ID=ID)
       else if (q%nbz==1) then
         call io_control(ACTION=OP_RD_CL,COM=NONE,    SEC=(/1,2,3/),ID=ID)
       else if (iqbz> iq_to_start) then
         call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
       endif
       !
       io_err=io_X(X,Xw,ID)
       !
       call deliver_IO_error_message(io_err,'PP/Em1s',STOP_it=.TRUE.)
       !
     endif
     !
     ! Poles and Residuals
     !
     cond_b=0
     cond_g=0
     !
     !$omp parallel do default(shared), private(ig1,ig2), reduction(+:cond_b,cond_g), collapse(2)
     do ig2=1,X%ng
       do ig1=1,X%ng
           if (real(X_mat(ig1,ig2,X_range(1))/X_mat(ig1,ig2,X_range(2)))<=1._SP) then
             !X_mat(ig1,ig2,X_range(2))=X%ppaE
             MPA_E_mat(ig1,ig2,npoles)=1._SP
             MPA_R_mat(ig1,ig2,npoles)=-X_mat(ig1,ig2,X_range(1))/2._SP*MPA_E_mat(ig1,ig2,npoles)*isc%gamp(ig1,ig2)
             cond_b = cond_b + 1
           else
             !X_mat(ig1,ig2,X_range(2))=sqrt(X_mat(ig1,ig2,X_range(1))/X_mat(ig1,ig2,X_range(2))-1)
             MPA_pols(1)=X_mat(ig1,ig2,X_range(1)); MPA_pols(2)=X_mat(ig1,ig2,X_range(2));
             MPA_R_mat(ig1,ig2,npoles)=-(MPA_pols(1)*MPA_freqs(1)**2  -MPA_pols(2)*MPA_freqs(2)**2 )/ &
&                                       (MPA_pols(1) -MPA_pols(2))  
             MPA_E_mat(ig1,ig2,npoles)=real(sqrt(MPA_R_mat(ig1,ig2,npoles)))  
             MPA_R_mat(ig1,ig2,npoles)=-X_mat(ig1,ig2,X_range(1))/2._SP*MPA_E_mat(ig1,ig2,npoles)*isc%gamp(ig1,ig2)
             cond_g = cond_g + 1
           endif
       enddo
     enddo
     !$omp end parallel do
     !
     call msg('r',' PPA condition good [iqibz,count]: ', (/iqibz,cond_g/) )
     call msg('r',' PPA condition fail [iqibz,count]: ', (/iqibz,cond_b/) )
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
     !call mat_transpose(X_mat(:,:,X_range(1)))
     !call mat_transpose(X_mat(:,:,X_range(2)))
     !
     call mat_transpose_r(MPA_E_mat(:,:,npoles))
     call mat_transpose(MPA_R_mat(:,:,npoles))
     !
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
     !
     do ib=QP_n_G_bands(1),QP_n_G_bands(2)
       !
       if (.not.PAR_IND_G_b%element_1D(ib)) cycle
       !
       if (q%sstar(iqbz,2)==1) call live_timing(steps=1)
       !
       isc%os(1)=ib
       !
       call scatter_Bamp(isc)
       iscp%os=isc%os
       !
       iscp%rhotw=isc%rhotw
       if (any(isc%is/=iscp%is)) call scatter_Bamp(iscp)
       !
       dc=cZERO
       !
       !if (l_ppa) then
         !
         forall (i_w=1:QP_dSc_steps) W_(i_w)=qp%E_bare(i_qp)+real(i_w-1,SP)*QP_dSc_delta+cI*QP_G_damp
         !
         E_kmq=E%E(isc%os(1),isc%os(2),isc%os(4))
         f_kmq=E%f(isc%os(1),isc%os(2),isc%os(4))
         !
         do i_w=1,QP_dSc_steps
           !
           dp_dummy = 0.0_DP
           !
           !$omp parallel do default(shared), private(ig1,ig2,ctmp), reduction(+:dp_dummy), collapse(2)
           do ig2=1,X%ng
             do ig1=1,X%ng
               !
               ctmp = -4._DP/spin_occ*pi*isc%rhotw(ig1)*conjg(iscp%rhotw(ig2))*(-MPA_R_mat(ig1,ig2,npoles))*&
&                     ( (spin_occ-f_kmq+bose_f(MPA_E_mat(ig1,ig2,npoles)))/(W_(i_w)-E_kmq-MPA_E_mat(ig1,ig2,npoles))+&
&                       (f_kmq+bose_f(MPA_E_mat(ig1,ig2,npoles)))/(conjg(W_(i_w))-E_kmq+MPA_E_mat(ig1,ig2,npoles)))
               !
               dp_dummy=dp_dummy+ctmp
               !
             enddo
           enddo
           !$omp end parallel do
           !
           dc(i_w) = dp_dummy
           !
         enddo
         !
         QP_Sc(i_qp,:QP_dSc_steps)=QP_Sc(i_qp,:QP_dSc_steps)+dc(:QP_dSc_steps)
         !
       !
     enddo ! loop on scattering states
     !
   enddo QP_loop
   !
 enddo Q_loop 
 !
 call live_timing()
 !
 ! CLEAN
 !
 deallocate(dc)
 deallocate(W_)
 call X_alloc('X')
 !
 call timing('GW(mpa)',OPR='stop')
 !
 if (.not.l_sc_run) call WF_free(WF)
 !
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp)
 !
 ! ALL 2 ALL of QP_Sc
 !
 call timing('GW(REDUX)',OPR='start')
 call PP_redux_wait(QP_Sc)
 call timing('GW(REDUX)',OPR='stop')
 !
end subroutine QP_mpa
