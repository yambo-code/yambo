!
! License-Identifier: GPL
!
! Authors (see AUTHORS file for details): AM [AF,IM,MB]
!
! headers
!
#include<dev_defs.h>
#include<y_memory.h>
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
! Authors (see AUTHORS file for details): AM
!
! headers
!
#include<dev_defs.h>
#include<y_memory.h>
!
!
subroutine QP_ppa_cohsex(X,Xk,E,k,q,qp,Xw,GW_iter)
 !
 ! Plasmon Pole & COHSEX Correlation Self-Energy
 !
 use pars,          ONLY:DP,SP,pi,schlen,cZERO,cI,cONE
 use units,         ONLY:HA2EV
 use stderr,        ONLY:intc
 use frequency,     ONLY:w_samp,W_reset
 use electrons,     ONLY:levels,spin_occ,spin,n_met_bands,n_sp_pol
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use drivers,       ONLY:l_ppa,l_cohsex,l_sc_srpa,l_sc_sex,l_sc_coh,l_sc_run,&
&                        l_rt_carriers_in_use,l_rim_w
 use parallel_int,  ONLY:PP_wait,PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_index,&
&                        PARALLEL_WF_distribute
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,&
&                        PAR_IND_Q_ibz,PAR_IND_Q_ibz_ID,PAR_IND_QP_ID,&
&                        PAR_Q_ibz_index,n_WF_bands_to_load,HEAD_QP_cpu
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use functions,     ONLY:bose_f
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:manage_action,OP_RD_CL,REP,VERIFY,NONE,OP_RD,RD_CL_IF_END,&
&                        io_RESPONSE,deliver_IO_error_message
 use QP_m,          ONLY:QP_solver,QP_t,QP_n_G_bands,QP_dSc_steps,QP_Sc_steps,QP_table,l_QP_symmetrize,&
&                        QP_Sc,QP_n_states,QP_G_damp,QP_table,QP_dSc_delta,GW_terminator_E,&
&                        COHSEX_use_empties,l_GW_terminator,GW_terminator_Kind,QP_states_simmetrize
 use X_m,           ONLY:X_ALLOC_elemental,X_par,X_t
 use wave_func,     ONLY:WF
 use R_lattice,     ONLY:qindx_S,bz_samp,DEV_VAR(G_m_G),G_m_G_maxval,nqibz,RIM_W_E,&
&                        RIM_W_is_diagonal,RIM_W_ng,DEV_VAR(RIM_W)
 use D_lattice,     ONLY:nsym,i_time_rev,i_space_inv,mag_syms
 use wrapper,       ONLY:V_dot_V_gpu,devxlib_xGEMV_gpu
 use interfaces,    ONLY:QP_state_print,WF_load,WF_free,MATRIX_transpose
 use matrix,        ONLY:PAR_matrix,MATRIX_reset
 use timing_m,      ONLY:timing
 use parallel_m,    ONLY:PAR_COM_RL_A2A,PAR_COM_QP_A2A,PAR_COM_G_b_INDEX
#if defined _SC
 use SC,            ONLY:it_now,it_to_start
#endif
 use devxlib,       ONLY:devxlib_memcpy_h2d,devxlib_memcpy_d2h,&
&                        devxlib_memcpy_d2d,devxlib_memset_d,devxlib_conjg
 use gpu_m,         ONLY:have_gpu
 !
 use drivers,       ONLY:Finite_Tel
 use D_lattice,     ONLY:Bose_Temp
 use functions,     ONLY:bose_E_cut
 !
 USE_MEMORY
 !
 implicit none
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
 type(w_samp)     :: Sc_W(qp%n_states)
 integer          :: i_qp,i_w,iqbz,iqibz,ib,ig1,ig2,alloc_err,iqs,iscs_save(2,4),&
&                    i_qp_to_start,iq_to_start,is,iq_mem,io_err,ID,IO_ACT,timing_steps
 integer          :: iw
 integer          :: X_range1,X_range2
 integer          :: X_rows1,X_rows2,X_cols1,X_cols2,X_nrows,X_ncols
 !
 character(schlen):: ch,SECTION_name,W_name
 logical          :: X_is_TR_rotated,l_X_ALLOC_elemental,l_RIM_W_g
 real(SP)         :: eet_cutoff0_sp(n_sp_pol),eet_cutoff0,eet_cutoff1_sp(n_sp_pol),eet_cutoff1
 real(SP)         :: X_ppaE,bose_PPA_E,eet_energy
 real(SP)         :: E_kmq,f_kmq,PPA_E
 complex(SP)      :: PPA_R,pre_factor
 complex(SP)      :: W_i,lW,QP_ppa_EET
 real(DP)         :: dp_dummy_r,dp_dummy_i
 complex(DP)      :: dp_dummy,ctmp
 !
 complex(SP), allocatable          :: W_(:),dc(:)
 complex(SP), allocatable DEV_ATTR :: eet_factor(:,:)
 complex(SP), allocatable DEV_ATTR :: local_rhotw(:), conjg_iscp_rhotw(:)
 type(elemental_collision), target :: isc,iscp
 !
 complex(SP), pointer DEV_ATTR :: isc_rhotw_p(:)
 complex(SP), pointer DEV_ATTR :: iscp_rhotw_p(:)
 complex(SP), pointer DEV_ATTR :: isc_gamp_p(:,:)
 complex(SP), pointer DEV_ATTR :: X_blc_p(:,:,:)
 !
 integer,     external ::io_X
 complex(SP), external ::QP_ppa_EET_terminator
 !
 integer      :: PPcond_rate,TO_rate
 real(SP)     :: PP_err
 !
 ! Reset & checks
 !
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp)
 i_qp_to_start=1
 iq_to_start  =1
 QP_Sc        =cZERO
 !
 ! Sc Energy points (1 type each QP state !)
 !-------------------------------------------
 if (trim(QP_solver)=='n') then
   !
   do i_qp=1,qp%n_states
     Sc_W(i_qp)%n_freqs=QP_dSc_steps
     YAMBO_ALLOC(Sc_W(i_qp)%p,(Sc_W(i_qp)%n_freqs))
     forall (i_w=1:QP_dSc_steps) Sc_W(i_qp)%p(i_w)=qp%E_bare(i_qp)+(i_w-1)*QP_dSc_delta
   enddo
   !
 else if (trim(QP_solver)=='g') then
   !
   QP_dSc_steps=QP_Sc_steps
   do i_qp=1,qp%n_states
     call W_reset(Sc_W(i_qp))
     Sc_W(i_qp)%n_freqs =QP_Sc_steps
     call FREQUENCIES_Green_Function(i_qp,Sc_W(i_qp),E%E,.FALSE.)
     !
     if(Sc_W(i_qp)%grid_type=="ra") then
       Sc_W(i_qp)%p=real(Sc_W(i_qp)%p,SP)
     else
       QP_G_damp=0._SP
     endif
   enddo
   !
 endif
 !
 if (l_GW_terminator.and.trim(GW_terminator_Kind)/='BG'.and.&
&    trim(GW_terminator_Kind)/='BRS') &
&    call error(' [GW/PPA] unknown GW_terminator_Kind = '//trim(GW_terminator_Kind))
 !
 ! COHSEX: bands setup
 !
 if ((l_sc_srpa.or.l_cohsex.or.l_sc_coh.or.l_sc_sex  ).and.(.not.COHSEX_use_empties)) then
   QP_n_G_bands(2)=max(maxval(QP_table(:,:2)),maxval(n_met_bands))
 endif
 !
 ! Section
 !
 SECTION_name=''
#if defined _SC
 if (l_sc_coh) SECTION_name=trim(SECTION_name)//'COH'
 if (l_sc_sex) SECTION_name=trim(SECTION_name)//'SEX'
#endif
 if (.not.l_sc_run) then
   SECTION_name='G'//trim(intc(GW_iter))
   W_name      ='W0'
   if (                    l_cohsex) SECTION_name=trim(SECTION_name)//trim(W_name)//' (COHSEX)'
   if (     l_ppa                  ) SECTION_name=trim(SECTION_name)//trim(W_name)//' (W PPA)'
   if (.not.l_ppa.and..not.l_cohsex) SECTION_name=trim(SECTION_name)//trim(W_name)//' (W real-axis)'
 endif
 !
 if (GW_iter==0) then
   if(l_rim_w) then
     call section('-',trim(SECTION_name))
   else
     call section('+',trim(SECTION_name))
   end if
 end if
 if (GW_iter > 0) call section('=',trim(SECTION_name))
 !
 if (GW_iter==0) then
   call msg('r', '[  GW  ] Bands range     ',QP_n_G_bands)
   if (l_ppa) then
     call msg('r', '[GW/PPA] G damping       ',QP_G_damp*HA2EV,"[eV]")
     call msg('r','')
   endif
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
   if (PAR_IND_Q_ibz%element_1D(q%sstar(iqbz,1))) then
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
#if defined _SC
 if(l_sc_run) ch='-SC'
#endif
 !
 ! wf and collisions dimension
 !-----------------------------
 isc%ngrho=X%ng
 ! AF: isc%ngrho=maxval(G_m_G), but maxval() 
 !     is already precalculated in eval_G_minus_G
 if (((l_sc_srpa.or.l_cohsex.or.l_sc_coh).and..not.COHSEX_use_empties).or.&
&      l_GW_terminator) isc%ngrho=G_m_G_maxval
 !
 call WF_load(WF,isc%ngrho,maxval(qindx_S(:,:,2)),(/1,n_WF_bands_to_load/),(/1,k%nibz/),title=trim(ch))
 !
 call timing('GW(ppa)',OPR='start')
 !
 ! Plasmon-Pole/Static interaction DB I/O
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID) 
 io_err=io_X(X,Xw,ID)
 if (io_err<0.and.io_RESPONSE) call error('Incomplete and/or broken PPA/Static diel. fun. database')
 !
 ! Test the spatial Inversion
 !   
 call WF_spatial_inversion(E,Xk)
 !
 ! ALLOCATION
 !------------
 !
 ! deallocate X_mat
 call X_ALLOC_elemental("X")
 !
 if (.not.allocated(X_par).and.io_RESPONSE) then
   allocate(X_par(1))
 endif
 !
 if (l_sc_coh.or.l_sc_sex.or.l_cohsex.or.l_sc_srpa) then
   if (io_RESPONSE) call X_ALLOC_parallel(X_par(1),X%ng,1,"X")
   YAMBO_ALLOC(dc,(2))
 else
   if (io_RESPONSE) call X_ALLOC_parallel(X_par(1),X%ng,2,"X")
   YAMBO_ALLOC(W_,(QP_dSc_steps))
   YAMBO_ALLOC(dc,(QP_dSc_steps))
 endif
 YAMBO_ALLOC(local_rhotw,(X%ng))
 YAMBO_ALLOC(conjg_iscp_rhotw,(X%ng))
 !
 ! Elemental Collisions
 !====================== 
 isc%iqref=0
 call elemental_collision_alloc(isc,NG=isc%ngrho,TITLE="GW")
 call elemental_collision_alloc(iscp,NG=isc%ngrho,TITLE="GW")
 !
 isc_rhotw_p => DEV_VAR(isc%rhotw)
 iscp_rhotw_p => DEV_VAR(iscp%rhotw)
 !
 call PP_wait()
 !
 !
 timing_steps=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)*&
&             PAR_IND_Q_ibz%n_of_elements(PAR_IND_Q_ibz_ID+1)*&
&             count( PAR_IND_G_b%element_1D(QP_n_G_bands(1):QP_n_G_bands(2)) )
 !
 ch=trim(SECTION_name)
#if defined _SC
 if (l_sc_run) ch=trim(SECTION_name)//'@it'//trim(intc(it_now))
#endif
 call live_timing(trim(ch),timing_steps)
 !
 if (io_RESPONSE) then
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
   io_err=io_X(X,Xw,ID)
 endif
 !
 ! indexes to be used with X_par (i.e. also when using X_blc_p)
 ! X_mat_range(1:2) were used with X_mat, instead
 !
 X_range1=1
 X_range2=Xw%n_freqs
 X_ppaE  =X%ppaE
 !
 Q_loop: do iqbz=iq_to_start,q%nbz
   !
   if (.not.PAR_IND_Q_ibz%element_1D(q%sstar(iqbz,1))) cycle
   !
   isc%qs(2:)=(/q%sstar(iqbz,1),q%sstar(iqbz,2)/)
   iqibz=isc%qs(2)
   iqs  =isc%qs(3)
   !
   if (iqibz/=isc%iqref) then
     !
     iq_mem=PAR_Q_ibz_index(iqibz)
     if (io_RESPONSE) iq_mem=1
     !
     ! I/O
     !
     if (io_RESPONSE) then
       !
       IO_ACT=manage_action(RD_CL_IF_END,iqibz,iq_to_start,nqibz,PAR_INDEX=PAR_IND_Q_ibz)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
       io_err=io_X(X,Xw,ID)
       !
       call deliver_IO_error_message(io_err,'PP/Em1s',STOP_it=.TRUE.)
       !
     endif
     !
     ! Poles and Residuals
     !
     ! DV comment:
     ! When cutoff is used, here a careful check is needed.
     ! Because of qpg can be imaginary, also are some component of X_mat, I am not sure
     ! that the definition here below of X_mat(ig1,ig2,:) is correct.
     !
#if defined _SC
     if (l_sc_run .and. it_now==it_to_start.or.io_RESPONSE) then
#endif
       !
       PPcond_rate=0
       TO_rate=0
       PP_err=0.0_SP
       !
       if (l_ppa) then
         !
         !$omp     parallel do default(shared), private(ig1,ig2,l_RIM_W_g), &
         !$omp &   reduction(+:PPcond_rate,TO_rate,PP_err), collapse(2)
         do ig2=X_par(iq_mem)%cols(1),X_par(iq_mem)%cols(2)
         do ig1=X_par(iq_mem)%rows(1),X_par(iq_mem)%rows(2)
           !
           ! RIM W support
           !
           l_RIM_W_g=(l_RIM_W.and.ig1<=RIM_W_ng.and.ig2<=RIM_W_ng.and.iqibz==1)
           if (RIM_W_is_diagonal.and.l_RIM_W_g) l_RIM_W_g=(ig1==ig2)
           !
           if (l_RIM_W_g) then
             X_par(iq_mem)%blc(ig1,ig2,X_range2)=RIM_W_E(ig1,ig2)
             !
           else
             if (real(X_par(iq_mem)%blc(ig1,ig2,X_range1)/X_par(iq_mem)%blc(ig1,ig2,X_range2))<=1._SP) then
               X_par(iq_mem)%blc(ig1,ig2,X_range2)=X_ppaE
               !
               PP_err=PP_err+abs( X_par(iq_mem)%blc(ig1,ig2,X_range1)/(X_ppaE**2+1._SP)-X_par(iq_mem)%blc(ig1,ig2,X_range2) )/&
&                              maxval( abs( X_par(iq_mem)%blc(ig1,ig2,X_range1:X_range2) ) )
               PPcond_rate=PPcond_rate+1
             else
               X_par(iq_mem)%blc(ig1,ig2,X_range2)=sqrt(X_par(iq_mem)%blc(ig1,ig2,X_range1)/X_par(iq_mem)%blc(ig1,ig2,X_range2)-1)
               !
               if(real(1._SP/X_par(iq_mem)%blc(ig1,ig2,X_range2),SP)*aimag( 1._SP/X_par(iq_mem)%blc(ig1,ig2,X_range1) )>0._SP) then
                   TO_rate=TO_rate+1
               endif
             endif
             !
           endif
           !
         enddo
         enddo
         !
       else ! COHSEX
         !
         ! GAMP
         !
         ! update alloc
         call elemental_collision_alloc(isc,GAMP_ROWS=X_par(1)%rows,GAMP_COLS=X_par(1)%cols)
         !
         call scatter_Gamp_gpu(isc,'c')
         if (have_gpu) call devxlib_memcpy_d2h(isc%gamp,DEV_VAR(isc%gamp))
         !
         !$omp parallel do default(shared), private(ig1,ig2), collapse(2)
         do ig2=X_par(iq_mem)%cols(1),X_par(iq_mem)%cols(2)
         do ig1=X_par(iq_mem)%rows(1),X_par(iq_mem)%rows(2)
           X_par(iq_mem)%blc(ig1,ig2,X_range1)=X_par(iq_mem)%blc(ig1,ig2,X_range1)*isc%gamp(ig1,ig2)
         enddo
         enddo
         !
         YAMBO_FREE_GPU(DEV_VAR(isc%gamp))
         YAMBO_FREE(isc%gamp)
         !
       endif
       !
       call msg('r',' Current Q-pt index             ',iqibz)
       call msg('r',' :: PP condition fails/total    ',real(PPcond_rate,SP)/X%ng**2)
       call msg('r',' :: Time ordering fails/rest    ',real(TO_rate,SP)/(X%ng**2-PPcond_rate))
       call msg('r',' :: Mean rel dev of PP cond     ',PP_err/X%ng**2)
       !
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
     call MATRIX_transpose("T",M_out=X_par(iq_mem))
     X_is_TR_rotated=.true.
     !
   endif
   !
   ! PAR_matrix definitions
   !
   X_rows1=X_par(iq_mem)%rows(1)
   X_rows2=X_par(iq_mem)%rows(2)
   X_cols1=X_par(iq_mem)%cols(1)
   X_cols2=X_par(iq_mem)%cols(2)
   X_nrows=X_rows2-X_rows1+1
   X_ncols=X_cols2-X_cols1+1
   !
#ifdef _GPU
   if (have_gpu) then
     YAMBO_ALLOC_GPU_MOLD(DEV_VAR(X_par(iq_mem)%blc),X_par(iq_mem)%blc)
     call devxlib_memcpy_h2d(DEV_VAR(X_par(iq_mem)%blc),X_par(iq_mem)%blc)
   endif
#endif
   X_blc_p=> DEV_VAR(X_par(iq_mem)%blc)
   !
   ! GAMP
   !
   if(l_ppa) then
     ! update alloc
     call elemental_collision_alloc(isc,GAMP_ROWS=X_par(1)%rows,GAMP_COLS=X_par(1)%cols)
     call scatter_Gamp_gpu(isc,'c')
     isc_gamp_p => DEV_VAR(isc%gamp)
   endif
   !
   if(l_ppa.and.l_GW_terminator) then
     YAMBO_ALLOC_GPU(eet_factor,(X_rows1:X_rows2,X_cols1:X_cols2))
   endif

   !
   ! main QP loop
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
     if (((l_sc_srpa.or.l_sc_coh.or.l_cohsex).and..not.COHSEX_use_empties).or.l_GW_terminator) then
       !
       iscs_save(1,: )=isc%os
       iscs_save(2,:3)=isc%qs
       isc%os=(/QP_table(i_qp,2),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
       isc%qs=(/1,1,1/)
       !
       call scatter_Bamp_gpu(isc)
       !
       if (l_GW_terminator) then
         !
         do is=1,n_sp_pol
           eet_cutoff0_sp(is)=minval(E%E(E%nbf(is)+1,:,is))
           eet_cutoff1_sp(is)=minval(E%E(QP_n_G_bands(2),:,is))
         enddo
         eet_cutoff0=minval(eet_cutoff0_sp(:))
         eet_cutoff1=minval(eet_cutoff1_sp(:))
         !
         ! eet_factor=cZERO
         call devxlib_memset_d(eet_factor,cZERO)
         !
         ! MB: After the inclusion of G-parallelism, PAR_COM_G_b_INDEX can be different
         ! from HEAD_QP_cpu --> the right one to be used is PAR_COM_G_b_INDEX
         !
         if (PAR_COM_G_b_INDEX%CPU_id==0) then
           if (X_is_TR_rotated) then
             !
             !DEV_ACC_DEBUG data present(G_m_G,isc_rhotw_p,eet_factor)
             !DEV_ACC parallel loop collapse(2)
             !DEV_CUF kernel do(2)
             !DEV_OMPGPU target map(present,alloc:G_m_G,isc_rhotw_p,eet_factor)
             !DEV_OMPGPU teams loop collapse(2)
             !DEV_OMP parallel do default(shared), private(ig1,ig2), collapse(2)
             do ig2=X_cols1,X_cols2
             do ig1=X_rows1,X_rows2
               eet_factor(ig1,ig2)=isc_rhotw_p(DEV_VAR(G_m_G)(ig2,ig1))
             enddo
             enddo
             !DEV_OMPGPU end target
             !DEV_ACC_DEBUG end data
             !
           else
             !
             !DEV_ACC_DEBUG data present(G_m_G,isc_rhotw_p,eet_factor)
             !DEV_ACC parallel loop collapse(2)
             !DEV_CUF kernel do(2)
             !DEV_OMPGPU target map(present,alloc:G_m_G,isc_rhotw_p,eet_factor)
             !DEV_OMPGPU teams loop collapse(2)
             !DEV_OMP parallel do default(shared), private(ig1,ig2), collapse(2)
             do ig2=X_cols1,X_cols2
             do ig1=X_rows1,X_rows2
               eet_factor(ig1,ig2)=isc_rhotw_p(DEV_VAR(G_m_G)(ig1,ig2))
             enddo
             enddo
             !DEV_OMPGPU end target
             !DEV_ACC_DEBUG end data
             !
           endif
         endif
         !
       else
         !
         dp_dummy = 0.0_DP
         dp_dummy_r = 0.0_DP
         dp_dummy_i = 0.0_DP
         !
         if (X_is_TR_rotated) then
           !
           !DEV_ACC_DEBUG data present(G_m_G,isc_rhotw_p)
           !DEV_ACC parallel loop collapse(2) private(ctmp) reduction(+:dp_dummy_r,dp_dummy_i)
           !DEV_CUF kernel do(2)
           !DEV_OMPGPU target map(present,alloc:G_m_G,isc_rhotw_p) &
           !DEV_OMPGPU &      map(tofrom:dp_dummy_r,dp_dummy_i)
           !DEV_OMPGPU teams loop collapse(2) reduction(+:dp_dummy)
           !DEV_OMP parallel do default(shared), private(ig1,ig2,ctmp), &
           !DEV_OMP &           reduction(+:dp_dummy), collapse(2)
           !
           do ig2=X_cols1,X_cols2
           do ig1=X_rows1,X_rows2
             ctmp=cmplx(2._SP*pi*isc_rhotw_p(DEV_VAR(G_m_G)(ig2,ig1))* &
&                       X_blc_p(ig1,ig2,X_range1),kind=DP)
#ifdef _GPU
             dp_dummy_r=dp_dummy_r+real(ctmp,DP)
             dp_dummy_i=dp_dummy_i+imag(ctmp)
#else
             dp_dummy=dp_dummy+ctmp
#endif
           enddo
           enddo
           !DEV_OMPGPU end target
           !DEV_ACC_DEBUG end data
           !
         else
           !
           !DEV_ACC_DEBUG data present(G_m_G,isc_rhotw_p)
           !DEV_ACC parallel loop collapse(2) private(ctmp) reduction(+:dp_dummy_r,dp_dummy_i)
           !DEV_CUF kernel do(2)
           !DEV_OMPGPU target map(present,alloc:G_m_G,isc_rhotw_p) &
           !DEV_OMPGPU &      map(tofrom:dp_dummy_r,dp_dummy_i)
           !DEV_OMPGPU teams loop collapse(2) reduction(+:dp_dummy_r,dp_dummy_i)
           !DEV_OMP parallel do default(shared), private(ig1,ig2,ctmp), &
           !DEV_OMP &           reduction(+:dp_dummy), collapse(2)
           !
           do ig2=X_cols1,X_cols2
           do ig1=X_rows1,X_rows2
             ctmp=cmplx(2._SP*pi*isc_rhotw_p(DEV_VAR(G_m_G)(ig1,ig2))*&
&                       X_blc_p(ig1,ig2,X_range1),kind=DP)
#ifdef _GPU
             dp_dummy_r=dp_dummy_r+real(ctmp,DP)
             dp_dummy_i=dp_dummy_i+imag(ctmp)
#else
             dp_dummy=dp_dummy+ctmp
#endif
           enddo
           enddo
           !DEV_OMPGPU end target
           !DEV_ACC_DEBUG end data
           !
         endif
         !
         if (have_gpu) dp_dummy=cmplx(dp_dummy_r,dp_dummy_i,kind=DP)
         !
         dc(1) = cmplx(dp_dummy,kind=SP)
         !
         !
         ! DV: Bug spotted in April 2014. HEAD_QP_cpu is defined differently when
         ! the _SC flag is used. The point is that in SE calculations HEAD_QP_cpu is used
         ! in cases where no band loops are done (like here). In _SC instead 
         ! it is needed for a different purpose. This is why I use PAR_COM_QP_A2A%CPU_id in this case.
         !
         ! AF,MB: After the inclusion of G-parallelism, the communicator to use
         ! here (both SC and ordinary runs) has become   PAR_COM_G_b_INDEX
         !
         ! AF: for the time being, the SC case is treated as if g-parallelism is
         ! disabled
         !
#if defined _SC
         if (l_sc_run) then
           if (PAR_COM_QP_A2A%CPU_id==0)    QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+dc(1)
         else
#endif
!          if (HEAD_QP_cpu)                 QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+dc(1)
           if (PAR_COM_G_b_INDEX%CPU_id==0) QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+dc(1)
#if defined _SC
         endif
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
       call scatter_Bamp_gpu(isc)
       iscp%os=isc%os
       !
       if (any(isc%is/=iscp%is)) then
         call scatter_Bamp_gpu(iscp)
       else
         !iscp%rhotw=isc%rhotw
         call devxlib_memcpy_d2d(iscp_rhotw_p, isc_rhotw_p)
       endif
       !conjg_iscp_rhotw=conjg(iscp%rhotw)
       call devxlib_memcpy_d2d(conjg_iscp_rhotw,iscp_rhotw_p)
       call devxlib_conjg(conjg_iscp_rhotw)
       !
       dc=cZERO
       !
       if (l_ppa) then
         !
         if(l_GW_terminator) then
           !
           !DEV_ACC_DEBUG data present(eet_factor,isc_rhotw_p,conjg_iscp_rhotw)
           !DEV_ACC parallel loop collapse(2) 
           !DEV_CUF kernel do(2)
           !DEV_OMPGPU target map(present,alloc:eet_factor,isc_rhotw_p,conjg_iscp_rhotw)
           !DEV_OMPGPU teams loop collapse(2)
           !DEV_OMP parallel do default(shared), private(ig1,ig2), collapse(2)
           !
           do ig2=X_cols1,X_cols2
           do ig1=X_rows1,X_rows2
              eet_factor(ig1,ig2)=eet_factor(ig1,ig2)-isc_rhotw_p(ig1)*conjg_iscp_rhotw(ig2)
           enddo
           enddo
           !DEV_OMPGPU end target
           !DEV_ACC_DEBUG end data
           !
         endif
         !
         ! DALV: here the grid is center in E0
         forall (i_w=1:QP_dSc_steps) W_(i_w)=Sc_W(i_qp)%p(i_w)+cI*QP_G_damp
         !
         E_kmq=E%E(isc%os(1),isc%os(2),isc%os(4))
         f_kmq=E%f(isc%os(1),isc%os(2),isc%os(4))
         !
         !
         do i_w=1,QP_dSc_steps
           !
           W_i = W_(i_w)
           !
           ! NOTE: Split reduction done here to work around PGI bug with complex
           !       CUF reductions.
           dp_dummy_r = 0.0_DP
           dp_dummy_i = 0.0_DP
           dp_dummy = 0.0_DP
           !
           !DEV_ACC_DEBUG data present(X_blc_p,RIM_W,isc_gamp_p,isc_rhotw_p,iscp_rhotw_p)
           !DEV_ACC parallel  loop collapse(2) private(PPA_E,PPA_R,l_RIM_W_g,bose_PPA_E,ctmp) &
           !DEV_ACC                reduction(+:dp_dummy_r,dp_dummy_i)
           !DEV_CUF kernel do(2)
           !DEV_OMPGPU target map(present,alloc:X_blc_p,RIM_W,isc_gamp_p,isc_rhotw_p,iscp_rhotw_p) &
           !DEV_OMPGPU &      map(tofrom:dp_dummy_r,dp_dummy_i)
           !DEV_OMPGPU teams  loop collapse(2) private(PPA_E,PPA_R,l_rim_W_g,bose_PPA_E,ctmp) &
           !DEV_OMPGPU &           reduction(+:dp_dummy_r,dp_dummy_i)
           !DEV_OMP parallel do default(shared), private(ig1,ig2,PPA_E,PPA_R,ctmp,l_RIM_W_g), &
           !DEV_OMP &        reduction(+:dp_dummy), collapse(2)
           !
           do ig2=X_cols1,X_cols2
           do ig1=X_rows1,X_rows2
             !
             PPA_E= real(X_ppaE/X_blc_p(ig1,ig2,X_range2),kind=SP)
             !
             ! RIM W support
             !
             l_RIM_W_g=(l_RIM_W.and.ig1<=RIM_W_ng.and.ig2<=RIM_W_ng)
             if (RIM_W_is_diagonal.and.l_RIM_W_g) l_RIM_W_g=(ig1==ig2)
             !
             if (l_RIM_W_g) then
               PPA_R=-cmplx(real(DEV_VAR(RIM_W)(1,iqibz,ig1,ig2))/2._SP, &
&                           aimag(X_blc_p(ig1,ig2,X_range1))*real(isc_gamp_p(ig1,ig2)),kind=SP)/2._SP*PPA_E
             else
               PPA_R=-X_blc_p(ig1,ig2,X_range1)/2._SP*PPA_E*isc_gamp_p(ig1,ig2)
             endif
             !
             ! inline bose_f function
             bose_PPA_E=0.
             if (PPA_E<0.) bose_PPA_E=-spin_occ
             if (Finite_Tel) then
               if (abs(PPA_E)>epsilon(1.)) then
                 if (abs(PPA_E)<=bose_E_cut*Bose_Temp) bose_PPA_E=spin_occ*Bose_Temp/PPA_E
                 if (abs(PPA_E)> bose_E_cut*Bose_Temp) bose_PPA_E=spin_occ/(exp(PPA_E/Bose_Temp)-1.)
               else
                 bose_PPA_E=spin_occ*Bose_Temp/epsilon(1.)
               endif
             endif
             !
             ctmp = -4._SP/spin_occ*pi*isc_rhotw_p(ig1)*conjg_iscp_rhotw(ig2)*(-PPA_R)*&
&                   ( (spin_occ-f_kmq+bose_PPA_E)/(W_i-E_kmq-PPA_E)+&
&                     (f_kmq+bose_PPA_E)/(conjg(W_i)-E_kmq+PPA_E))
             !
#ifdef _GPU
             dp_dummy_r=dp_dummy_r+real(ctmp,DP)
             dp_dummy_i=dp_dummy_i+imag(ctmp)
#else
             dp_dummy=dp_dummy+ctmp
#endif
             !
           enddo
           enddo
           !DEV_OMPGPU end target
           !DEV_ACC_DEBUG end data
           !
           if (have_gpu) dp_dummy=cmplx(dp_dummy_r,dp_dummy_i,kind=DP)
           !
           dc(i_w) = cmplx(dp_dummy,kind=SP)
           !
         enddo
         !
         QP_Sc(i_qp,:QP_dSc_steps)=QP_Sc(i_qp,:QP_dSc_steps)+dc(:QP_dSc_steps)
         !
         !
       else if (l_sc_sex.or.l_cohsex.or.(l_sc_coh.and.COHSEX_use_empties)) then
         !
         call devxlib_xGEMV_gpu ('N',X_nrows,X_ncols, cONE, DEV_VAR(X_par(iq_mem)%blc)(:,:,X_range1),X_nrows,&
&                                conjg_iscp_rhotw(X_cols1:X_cols2),1,cZERO,local_rhotw(X_rows1:X_rows2),1)
         !
         pre_factor=V_dot_V_gpu(X_nrows,DEV_VAR(isc%rhotw)(X_rows1:X_rows2),local_rhotw(X_rows1:X_rows2))

         !
         ! SEX
         !
         if (l_sc_sex.or.l_cohsex) dc(1)=-4._SP/spin_occ*pi*pre_factor*e%f(isc%os(1),isc%os(2),isc%os(4))
         !
         ! COH (when no empties are used the COH part is indeed calculated above)
         !
         if (COHSEX_use_empties) then
           if (l_sc_coh.or.l_cohsex) dc(2)=2._SP*pi*pre_factor
         endif
         !
         QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+dc(1)+dc(2)
         !
       endif
       !
     enddo ! loop on scattering states
     !
     if(l_ppa.and.l_GW_terminator) then
       !
       ! DALV: here the grid is center in E0
       forall (i_w=1:QP_dSc_steps) W_(i_w)=Sc_W(i_qp)%p(i_w)+cI*QP_G_damp
       !
       do i_w=1,QP_dSc_steps
         !
         dp_dummy_r=0.0_DP
         dp_dummy_i=0.0_DP
         dp_dummy=0.0_DP
         !
         !DEV_ACC_DEBUG data present(X_blc_p,isc_gamp_p,eet_factor)
         !DEV_ACC parallel  loop collapse(2) private(PPA_E,PPA_R,eet_energy,lW,QP_ppa_EET,ctmp) &
         !DEV_ACC                reduction(+:dp_dummy_r,dp_dummy_i)
         !DEV_CUF kernel do(2)
         !DEV_OMPGPU target map(present,alloc:X_blc_p,isc_gamp_p,eet_factor) &
         !DEV_OMPGPU &      map(tofrom:dp_dummy_r,dp_dummy_i)
         !DEV_OMPGPU teams  loop collapse(2) private(PPA_E,PPA_R,eet_energy,lW,QP_ppa_EET,ctmp) &
         !DEV_OMPGPU &           reduction(+:dp_dummy_r,dp_dummy_i)
         !DEV_OMP parallel do default(shared), private(ig1,ig2,PPA_E,PPA_R,QP_ppa_EET,ctmp), &
         !DEV_OMP &        reduction(+:dp_dummy), collapse(2)
         !
         do ig2=X_cols1,X_cols2
         do ig1=X_rows1,X_rows2
           !
           PPA_E=X_ppaE/real(X_blc_p(ig1,ig2,X_range2))
           PPA_R=-X_blc_p(ig1,ig2,X_range1)/2._SP*PPA_E*isc_gamp_p(ig1,ig2)
           !
#ifdef _GPU
           eet_energy=eet_cutoff1+GW_terminator_E
           lW=W_i
           if (Finite_Tel) lW=conjg(W_i)
           QP_ppa_EET=real(spin_occ)/(lW-eet_energy-PPA_E)
#else
           QP_ppa_EET= QP_ppa_EET_terminator(W_i,E,isc%is,PPA_E,ig1,ig2,isc%qs(2),&
&                                            eet_cutoff0,eet_cutoff1)
#endif
           !
           ctmp = cmplx(4._SP/spin_occ*pi*PPA_R*eet_factor(ig1,ig2)*QP_ppa_EET, kind=DP)
           !
#ifdef _GPU
           dp_dummy_r=dp_dummy_r+real(ctmp,DP)
           dp_dummy_i=dp_dummy_i+imag(ctmp)
#else
           dp_dummy=dp_dummy+ctmp
#endif
           !
         enddo
         enddo
         !DEV_OMPGPU end target
         !DEV_ACC_DEBUG end data
         !
         if (have_gpu) dp_dummy=cmplx(dp_dummy_r,dp_dummy_i,kind=DP)
         !
         QP_Sc(i_qp,i_w)=QP_Sc(i_qp,i_w)+cmplx(dp_dummy,kind=SP)
         !
       enddo
       !
     endif
     !
   enddo QP_loop
   !
   YAMBO_FREE_GPU(eet_factor)
   YAMBO_FREE(eet_factor)
   YAMBO_FREE_GPU(DEV_VAR(isc%gamp))
   YAMBO_FREE(isc%gamp)
   if (have_gpu) then
     YAMBO_FREE_GPU(DEV_VAR(X_par(iq_mem)%blc))
   endif
   !
 enddo Q_loop 
 !
 call live_timing()
 !
 ! CLEAN
 !
 YAMBO_FREE(dc)
 YAMBO_FREE(W_)
 YAMBO_FREE_GPU(local_rhotw)
 YAMBO_FREE(local_rhotw)
 YAMBO_FREE_GPU(conjg_iscp_rhotw)
 YAMBO_FREE(conjg_iscp_rhotw)
 !
 l_X_ALLOC_elemental=.true.
#if defined _SC
 l_X_ALLOC_elemental=l_sc_run.and.io_RESPONSE
#endif
 if(l_X_ALLOC_elemental) then
   do iq_mem = 1,size(X_par)
     call MATRIX_reset(X_par(iq_mem))
   enddo
   if (allocated(X_par)) deallocate(X_par)
 endif
 call X_ALLOC_elemental('X')
 !
 call timing('GW(ppa)',OPR='stop')
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
 ! AM, Sept 2019. The COH potential seems to break (in some case of a large amount) the
 ! energy degenerations. 
 !
 if (.not.l_sc_run.and..not.l_rt_carriers_in_use.and.l_QP_symmetrize) then
   do i_w=1,QP_dSc_steps
     if (i_w==1) call QP_states_simmetrize(E,what="COHSEX Sc",V_complex=QP_Sc(:,1),warn_me=.TRUE.)
     if (i_w> 1) call QP_states_simmetrize(E,V_complex=QP_Sc(:,i_w))
   enddo
 endif
 !
end subroutine
