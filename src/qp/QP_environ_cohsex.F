!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine QP_environ_cohsex(X,Xk,E,k,q,qp)
 !
 ! Plasmon Pole & COHSEX Correlation Self-Energy
 !
 use pars,          ONLY:DP,SP,pi,schlen,cZERO,cI
 use units,         ONLY:HA2EV
 use stderr,        ONLY:intc
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin,n_met_bands,n_sp_pol
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use environ_m,     ONLY:v_env_mat
 use drivers,       ONLY:l_ppa,l_cohsex,l_sc_srpa,l_sc_sex,l_sc_coh,l_sc_run,&
&                        l_rt_carriers_in_use,l_RIM_W
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
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_table,l_QP_symmetrize,&
&                        QP_env_cohsex,QP_n_states,QP_G_damp,QP_table, QP_env_coh, QP_env_sex
 use X_m,           ONLY:X_ALLOC_elemental,X_mat,X_mat_d,X_t
 use wave_func,     ONLY:WF
 use R_lattice,     ONLY:qindx_S,bz_samp,G_m_G,nqibz,RIM_W_E,bare_qpg,bare_qpg_d,q_pt,&
&                        RIM_W_is_diagonal,RIM_W_ng,RIM_W,RIM_W_d
 use D_lattice,     ONLY:nsym,i_time_rev,i_space_inv,mag_syms
 use wrapper,       ONLY:M_by_V
 use interfaces,    ONLY:QP_state_print,WF_load,WF_free
 use matrix_operate,ONLY:mat_transpose
 use matrix,        ONLY:PAR_matrix,MATRIX_reset
 use timing_m,      ONLY:timing
#if defined _SC
 use parallel_m,    ONLY:PAR_COM_QP_A2A
 use SC,            ONLY:it_now,it_to_start
#endif
 use deviceXlib_m,  ONLY:dev_memcpy
 use cuda_m,        ONLY:have_cuda
#ifdef _CUDA
 use drivers,       ONLY:Finite_Tel
 use D_lattice,     ONLY:Bose_Temp
 use functions,     ONLY:bose_E_cut
#endif
 !
#include<dev_defs.h>
#include<memory.h>
 !
 type(levels) ::E
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 !
 ! External
 !
 integer, external :: io_ENVIRON
 !
 ! Work Space
 !
 integer          ::i_qp,i_w,iqbz,iqibz,ib,ig1,ig2,alloc_err,iqs,iscs_save(2,4),&
&                   i_qp_to_start,iq_to_start,is,io_err,ID,IO_ACT,timing_steps
 complex(SP), allocatable ::W_(:),dc(:),eet_factor(:,:)
 type(elemental_collision),target ::isc,iscp
 character(schlen)::ch,SECTION_name,W_name
 logical          ::X_is_TR_rotated,l_X_ALLOC_elemental,l_RIM_W_g
 real(SP)         ::eet_cutoff0(n_sp_pol),E_kmq,f_kmq,eet_cutoff1(n_sp_pol),PPA_E
 complex(SP)      ::PPA_R,local_rhotw(X%ng),pre_factor
 complex(DP)      ::dp_dummy,ctmp
 integer :: QP_n_G_bands_environ_cohsex(2)
 !
 !
 type(PAR_matrix) :: v_environ
 !
#ifdef _CUDA
 integer                      :: Xng, Xr1, Xr2
 real(SP)                     :: XppaE, bose_PPA_E
 real(DP)                     :: dp_dummy_r, dp_dummy_i
 complex(SP)                  :: W_i_w
 complex(SP), pointer, device :: isc_rhotw_d(:)
 complex(SP), pointer, device :: iscp_rhotw_d(:)
 complex(SP), pointer, device :: isc_gamp_d(:,:)
#endif
 !
 integer,     external ::io_X
 complex(SP), external ::QP_ppa_EET_terminator
 !
 call msg('r', '[  ENV ] Computing Environ COHSEX')
 !
 ! Reset & checks
 !
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp)
 i_qp_to_start=1
 iq_to_start  =1
 QP_env_cohsex =cZERO
 QP_env_coh =cZERO
 QP_env_sex =cZERO
 !
 ! COHSEX: bands setup
 !
 QP_n_G_bands_environ_cohsex = QP_n_G_bands
 QP_n_G_bands_environ_cohsex(2)=max(maxval(QP_table(:,:2)),maxval(n_met_bands))
 !
 call k_expand(k)
 !
 !
 ! WF distributed & load
 !=======================
 !
 if ( .not.l_sc_run ) call PARALLEL_global_indexes(E,k,q,"Self_Energy")
 !
 ! Redefine iq_to_start to be CPU-dependent
 !
 ! Environ SLK datatype
 !
 ! call MATRIX_init("SLK", v_environ , X%ng, 1) 
 ! Y AMBO_ALLOC(v_environ%blc,(v_environ%BLCrows(1):v_environ%BLCrows(2),v_environ%BLCcols(1):v_environ%BLCcols(2),1))
 ! v_environ%blc=cZERO
 !   
 ! if (have_cuda) then
 !    Y AMBO_ALLOC_SOURCE(v_environ%blc_d,v_environ%blc)
 ! endif
 do iqbz=1,q%nbz 
   if (PAR_IND_Q_ibz%element_1D(q%sstar(iqbz,1))) then
     iq_to_start=iqbz
     exit
   endif
 enddo
 !
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
 !
 isc%ngrho=maxval(G_m_G)
 !
 call WF_load(WF,isc%ngrho,maxval(qindx_S(:,:,2)),(/1,n_WF_bands_to_load/),(/1,k%nibz/),title=trim(ch))
 !
 ! Elemental Collisions
 !====================== 
 isc%iqref=0
 call elemental_collision_alloc(isc,NG=isc%ngrho,NG_GAMP=(/X%ng,X%ng/),TITLE="GW")
 call elemental_collision_alloc(iscp,NG=isc%ngrho,TITLE="GW")
 !
 call timing('QP_environ_cohsex',OPR='start')
 !
 ! Plasmon-Pole/Static interaction DB I/O
 !
 !  call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 !  io_err=io_X(X,Xw,ID)
 !  if (io_err<0.and.io_RESPONSE) call error('Incomplete and/or broken PPA/Static diel. fun. database')
 !
 ! Test the spatial Inversion
 !   
 call WF_spatial_inversion(E,Xk)
 !
 ! ALLOCATION
 !------------
 !


 call io_control(ACTION=OP_RD_CL, COM=REP, SEC=(/1,2/), MODE=VERIFY, ID=ID)
 io_err = io_ENVIRON(X, ID)
 if (io_RESPONSE) then
   if (io_err < 0) call error('Incomplete and/or broken PCM-screened interaction database')
   call X_ALLOC_elemental('V_ENV',(/X%ng, X%ng, 1/))
   call io_control(ACTION=OP_RD, COM=NONE, SEC=(/1/), ID=ID)
   io_err = io_ENVIRON(X, ID)
   call X_ALLOC_elemental('X', (/X%ng, X%ng, 1/))
 end if
 allocate(dc(2))
 !
 !
 call PP_wait()
 !
 timing_steps=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)*&
&             PAR_IND_Q_ibz%n_of_elements(PAR_IND_Q_ibz_ID+1)*&
&             count( PAR_IND_G_b%element_1D(QP_n_G_bands_environ_cohsex(1):QP_n_G_bands_environ_cohsex(2)) )
 !
 call live_timing(trim(ch),timing_steps)
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
     call DEV_SUB(scatter_Gamp)(isc,'c')
     if (have_cuda) call dev_memcpy(isc%gamp,isc%gamp_d)
     !
     if (io_RESPONSE) then
       !
       IO_ACT = manage_action(RD_CL_IF_END, iqibz, iq_to_start, nqibz, PAR_INDEX=PAR_IND_Q_ibz)
       call io_control(ACTION=IO_ACT, COM=NONE, SEC=(/2*iqibz, 2*iqibz+1/), ID=ID)
       io_err = io_ENVIRON(X, ID)
       !
       call deliver_IO_error_message(io_err,'environ',STOP_it=.TRUE.)
     end if
     !
     !$omp parallel do default(shared), private(ig1,ig2)
     do ig2=1,X%ng
       do ig1=1,X%ng
         ! 
         ! Here: X_mat is set to v_reac(G,G') * (q+G) * (q+G') * gamp(G,G')
         !
         X_mat(ig1,ig2,1)= v_env_mat(ig1,ig2,1) * &
&                                   bare_qpg(iqibz,ig1)*bare_qpg(iqibz,ig2)/(4.0_SP*pi)
         !
         ! subtract bare coulomb
         !
         if (ig1==ig2) X_mat(ig1,ig1,1)=X_mat(ig1,ig1,1)-1.0_SP
         !
         ! multiply by gamp (integration volume)
         !
         X_mat(ig1,ig2,1)=X_mat(ig1,ig2,1)*isc%gamp(ig1,ig2)
         !
       enddo
     enddo
     !$omp end parallel do
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
     call mat_transpose(X_mat(:,:,1))
     X_is_TR_rotated=.true.
   endif
   !
   if (have_cuda) call dev_memcpy(X_mat_d, X_mat)
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
     iscs_save(1,: )=isc%os
     iscs_save(2,:3)=isc%qs
     isc%os=(/QP_table(i_qp,2),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     isc%qs=(/1,1,1/)

     call DEV_SUB(scatter_Bamp)(isc)
     if (have_cuda) call dev_memcpy(isc%rhotw,isc%rhotw_d)
     !
     dp_dummy = 0.0_DP
     !
     if (X_is_TR_rotated) then
       !$omp parallel do default(shared), private(ig1,ig2), reduction(+:dp_dummy)
       do ig2=1,X%ng
         do ig1=1,X%ng
           dp_dummy=dp_dummy+cmplx(2._SP*pi*isc%rhotw(G_m_G(ig2,ig1))*X_mat(ig1,ig2,1),kind=DP)
         enddo
       enddo
       !$omp end parallel do
     else
       !$omp parallel do default(shared), private(ig1,ig2), reduction(+:dp_dummy)
       do ig2=1,X%ng
         do ig1=1,X%ng
           dp_dummy=dp_dummy+cmplx(2._SP*pi*isc%rhotw(G_m_G(ig1,ig2))*X_mat(ig1,ig2,1),kind=DP)
         enddo
       enddo
       !$omp end parallel do
     endif
     !
     dc(1) = cmplx(dp_dummy,kind=SP)
     !
     ! Bug spotted by D.V. (April 2014). HEAD_QP_cpu is defined differently when
     ! the _SC flag is used. The point is that in SE calculations HEAD_QP_cpu is used
     ! in cases where no band loops are done (like here). In _SC instead 
     ! it is needed for a different purpose. This is why I use PAR_COM_QP_A2A%CPU_id in this case.
     !
     if (HEAD_QP_cpu)  QP_env_cohsex(i_qp)=QP_env_cohsex(i_qp)+dc(1)
     if (HEAD_QP_cpu)  QP_env_coh(i_qp)=QP_env_coh(i_qp)+dc(1)
     !
     dc=cZERO
     !
     isc%os=iscs_save(1,: )
     isc%qs=iscs_save(2,:3)
     !
     !
     do ib=QP_n_G_bands_environ_cohsex(1), QP_n_G_bands_environ_cohsex(2)
       !
       if (.not.PAR_IND_G_b%element_1D(ib)) cycle
       !
       if (q%sstar(iqbz,2)==1) call live_timing(steps=1)
       !
       isc%os(1)=ib
       !
       call DEV_SUB(scatter_Bamp)(isc)
       if (have_cuda) call dev_memcpy(isc%rhotw, isc%rhotw_d)
       iscp%os=isc%os
       !
       if (any(isc%is/=iscp%is)) then
         call DEV_SUB(scatter_Bamp)(iscp)
         if (have_cuda) call dev_memcpy(iscp%rhotw, iscp%rhotw_d)
       else
         iscp%rhotw=isc%rhotw
         if (have_cuda) call dev_memcpy(iscp%rhotw_d, iscp%rhotw)
       endif
       !
       dc=cZERO
       !
       ! SEX
       !
       !
       call M_by_V('N', X%ng, X_mat(:,:,1), conjg(iscp%rhotw), local_rhotw)
       !
       pre_factor=0.0_SP
       !$omp parallel do default(shared), private(ig1), reduction(+:pre_factor)
       do ig1=1,X%ng
         pre_factor=pre_factor+isc%rhotw(ig1)*local_rhotw(ig1)
       enddo
       !$omp end parallel do
       !
       dc(1)=-4._SP/spin_occ*pi*pre_factor*e%f(isc%os(1),isc%os(2),isc%os(4))
       !
       QP_env_cohsex(i_qp)=QP_env_cohsex(i_qp)+dc(1)
       QP_env_sex(i_qp)=QP_env_sex(i_qp)+dc(1)
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
 if (io_RESPONSE) then
   call X_ALLOC_elemental('X')
   call X_ALLOC_elemental('V_ENV')
 end if
 !
 deallocate(dc)
 !
 if (.not.l_sc_run) call WF_free(WF)
 !
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp)
 !
 ! ALL 2 ALL of QP_env_cohsex
 call PP_redux_wait(QP_env_cohsex)
 call PP_redux_wait(QP_env_sex)
 !
 call timing('QP_environ_cohsex',OPR='stop')
 !
end subroutine QP_environ_cohsex

