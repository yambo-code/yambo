!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DALV, AF, AM
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
subroutine QP_mpa(X,Xk,E,k,q,qp,Xw,W,GW_iter)
 !
 ! Multipole approximation to the Correlation Self-Energy
 !
 use pars,          ONLY:SP,DP,pi,schlen,cZERO,cI
 use units,         ONLY:HA2EV
 use stderr,        ONLY:intc
 use frequency,     ONLY:w_samp 
 use electrons,     ONLY:levels,spin_occ,spin,n_met_bands,n_sp_pol
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use drivers,       ONLY:l_sc_run
 use parallel_int,  ONLY:PP_wait,PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_index,&
&                        PARALLEL_WF_distribute
 use parser_m,      ONLY:parser
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,PAR_COM_Q_A2A,&
&                        PAR_IND_Q_ibz,PAR_IND_Q_ibz_ID,PAR_IND_QP_ID,PAR_COM_RL_INDEX,&
&                        PAR_Q_ibz_index,n_WF_bands_to_load,HEAD_QP_cpu,master_cpu,myid
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use functions,     ONLY:bose_f
 use IO_int,        ONLY:io_control,IO_and_Messaging_switch
 use IO_m,          ONLY:manage_action,OP_RD_CL,OP_WR_CL,OP_APP_CL,REP,VERIFY,NONE,RD_CL,OP_RD,RD_CL_IF_END,&
 &                       io_RESPONSE,io_MULTIPOLE,deliver_IO_error_message
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_dSc_steps,&
&                        QP_Sc,QP_n_states,QP_G_damp,QP_table,QP_dSc_delta
 use X_m,           ONLY:X_ALLOC_elemental,X_mat,X_t
 use wave_func,     ONLY:WF
 use R_lattice,     ONLY:qindx_S,bz_samp,G_m_G
 use D_lattice,     ONLY:nsym,i_time_rev,i_space_inv,mag_syms
 use wrapper,       ONLY:M_by_V
 use interfaces,    ONLY:QP_state_print,WF_load,WF_free
 use matrix_operate,ONLY:mat_transpose
 use timing_m,      ONLY:timing
 use mpa_m,         ONLY:mpa_RE_solver,err_func_X,MPA_E_mat,MPA_R_mat,MPA_freqs,&
&                        MPA_alloc,MPA_free
 use drivers,       ONLY:Finite_Tel
 use D_lattice,     ONLY:Bose_Temp
 use functions,     ONLY:bose_E_cut
 !
#include<memory.h>
 !
 type(levels)        ::E
 type(bz_samp)       ::Xk,k,q
 type(X_t)           ::X
 type(QP_t)          ::qp
 type(w_samp)        ::Xw
 real(SP),intent(in) ::W(QP_dSc_steps) ! DALV: this is the frequency grid for Sc now set in the solvers('n','s','g')
 integer             ::GW_iter
 !
 ! Work Space
 !
 integer          :: i_qp,i_w,iqbz,iqibz,ib,ig1,ig2,alloc_err,iqs,iscs_save(2,4),&
&                    i_qp_to_start,iq_to_start,is,iq_mem,X_range(2),io_err,ID,IO_ACT,timing_steps,&
&                    cond_g, cond_b
 !
 complex(SP), allocatable :: W_(:),dc(:),eet_factor(:,:)
 type(elemental_collision):: isc,iscp
 character(schlen):: ch,SECTION_name
 !
 logical          :: X_is_TR_rotated,l_X_alloc
 real(SP)         :: eet_cutoff0(n_sp_pol),E_kmq,f_kmq,eet_cutoff1(n_sp_pol)
 complex(SP)      :: local_rhotw(X%ng),pre_factor
 complex(DP)      :: dp_dummy,ctmp
 !
 ! MPA variables
 !
 integer          :: i_np,ID2
 logical          :: MPred
 real(SP)         :: bose_PPA_E,MPred_rate,PPcond_rate,PPcond_Qrate,MP_err,MP_Qerr,cond_num(2),cond_numQ(2) 
 complex(SP), allocatable :: MPA_Xo(:)
 character(len=99):: str
 !
 integer, external::io_X,io_MPA,QP_state_extract
 !
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
 ! MPA dynamic X DB I/O
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
 if (io_RESPONSE) call X_ALLOC_elemental('X',(/X%ng,X%ng,2*X%mpa_npoles/))
 allocate(W_(QP_dSc_steps))
 allocate(dc(QP_dSc_steps),stat=alloc_err)
 !
 ! MPA workspace
 !
 if (io_MULTIPOLE) call MPA_alloc((/X%ng,X%ng,X%mpa_npoles,2*X%mpa_npoles/)) 
 call parser('mpERdb',io_MULTIPOLE)
 !
 call PP_wait()
 !
 timing_steps=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)*&
&             PAR_IND_Q_ibz%n_of_elements(PAR_IND_Q_ibz_ID+1)*&
&             count( PAR_IND_G_b%element_1D(QP_n_G_bands(1):QP_n_G_bands(2)) )
 !
 ch=trim(SECTION_name)
 call live_timing(trim(ch),timing_steps)
 !
 call msg('r','  MPA interpolation solver:      ',Xw%mpa_solver)
 !
 if (io_RESPONSE) then
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
   io_err=io_X(X,Xw,ID)
 endif
 !
 if(io_MULTIPOLE) then
   !call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID2,COMM=PAR_COM_NULL,DO_IT=master_cpu)
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID2)
   io_err=io_MPA(X,Xw,ID2)
   call IO_and_Messaging_switch("SAVE")
   call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_Q_A2A%CPU_id==0)
 endif
 !
 call PP_wait()
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
     X_range=(/1,Xw%n_freqs/)
     if (.not.io_RESPONSE) X_range=(/Xw%n_freqs*(iq_mem-1)+1,iq_mem*Xw%n_freqs/)
     !
     call scatter_Gamp(isc,'c')
     !
     ! I/O
     !
     if (io_RESPONSE) then
       !
       IO_ACT=manage_action(RD_CL_IF_END,iqibz,iq_to_start,q%nibz,PAR_INDEX=PAR_IND_Q_ibz)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
       io_err=io_X(X,Xw,ID)
       !
       call deliver_IO_error_message(io_err,'MPA/Xm',STOP_it=.TRUE.)
       !
       ! Sampling the frequency domain
       !
       MPA_freqs=Xw%p
       !
     endif
     !
     ! Poles and Residues
     !
     MPred_rate=0._SP
     PPcond_Qrate=0._SP
     MP_Qerr=0._SP
     cond_numQ(:)=0._SP
     !
     !$omp parallel default(shared),private(ig1,ig2,MPred,PPcond_rate,MP_err,cond_num,i_np,MPA_Xo), &
     !$omp & reduction(+:MPred_rate),reduction(+:PPcond_Qrate),reduction(+:MP_Qerr),reduction(+:cond_numQ)
     !
     YAMBO_ALLOC(MPA_Xo,(Xw%n_freqs))
     !
     !$omp do collapse(2)
     do ig2=1,X%ng
       do ig1=1,X%ng
         !
         MPA_Xo(1:Xw%n_freqs)=X_mat(ig1,ig2,1:Xw%n_freqs)! DALV: Differently from PPA the factor isc%gamp(ig1,ig2) is included later
         MPred=.false.
         PPcond_rate=0._SP
         MP_err=0._SP
         cond_num(:)=0._SP
         !
         call mpa_RE_solver(X%mpa_npoles,MPA_freqs,MPA_Xo,MPA_R_mat(ig1,ig2,:),MPA_E_mat(ig1,ig2,:),&
&                                                  Xw%mpa_solver,MPred,PPcond_rate,MP_err,cond_num)
         if(MPred) MPred_rate=MPred_rate+1._SP
         PPcond_Qrate=PPcond_Qrate+PPcond_rate
         MP_Qerr=MP_Qerr+MP_err
         cond_numQ(:)=cond_numQ(:)+cond_num(:)
         !
       enddo
     enddo
     !$omp end do
     !     
     YAMBO_FREE(MPA_Xo)
     !
     !$omp end parallel
     !
     X_is_TR_rotated=.false.
     !
     MPred_rate=MPred_rate/X%ng**2
     PPcond_Qrate=PPcond_Qrate/X%ng**2
     MP_Qerr=MP_Qerr/X%ng**2
     cond_numQ(:)=cond_numQ(:)/X%ng**2
     !
     ! I/O MPA
     !
     if (io_MULTIPOLE) then
       !
! #if defined _PAR_IO
!       call io_control(ACTION=OP_APP_CL,COM=REP,SEC=(/2*iqibz,2*iqibz+1/),ID=ID2, &
!       &               COMM=PAR_COM_RL_INDEX,DO_IT=PAR_COM_RL_A2A%CPU_id==0 )
! #else
!       call io_control(ACTION=OP_APP_CL,COM=REP,SEC=(/2*iqibz,2*iqibz+1/),ID=ID2, &
!       &               COMM=PAR_COM_NULL,DO_IT=PAR_COM_X_WORLD%CPU_id==0 )
! #endif
       call io_control(ACTION=OP_APP_CL,COM=REP,SEC=(/2*iqibz,2*iqibz+1/),ID=ID2)
       io_err=io_MPA(X,Xw,ID2)
       call msg('r',' :: PP cond fix/tot      ',PPcond_Qrate)
       call msg('r',' :: Mean np reduction    ',MPred_rate)
       call msg('r',' :: Mean Xm rel dev      ',MP_Qerr)
       if(Xw%mpa_solver=='LA') call msg('r',' :: Condition numbers    ',cond_numQ)
       !
     endif
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
     do i_np=1,X%mpa_npoles
       call mat_transpose(MPA_E_mat(:,:,i_np))
       call mat_transpose(MPA_R_mat(:,:,i_np))
     enddo
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
     ! DALV: here the grid is center in E0
     forall (i_w=1:QP_dSc_steps) W_(i_w)=W(i_w)+qp%E_bare(i_qp)+cI*QP_G_damp
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
       E_kmq=E%E(isc%os(1),isc%os(2),isc%os(4))
       f_kmq=E%f(isc%os(1),isc%os(2),isc%os(4))
       !
       do i_w=1,QP_dSc_steps
         !
         dp_dummy = 0.0_SP
         !
         !$omp parallel do default(shared), private(i_np,ig1,ig2,bose_PPA_E,ctmp), &
         !$omp &           reduction(+:dp_dummy), collapse(3)
         !
         do i_np=1,X%mpa_npoles
           do ig2=1,X%ng
             do ig1=1,X%ng
               !
               ! DALV: inline bose_f function
               bose_PPA_E=0._SP
               if (real(MPA_E_mat(ig1,ig2,i_np),SP)<0._SP) bose_PPA_E=-spin_occ
               if (Finite_Tel) then
                 if (real(MPA_E_mat(ig1,ig2,i_np),SP)>epsilon(1._SP)) then
                   if (real(MPA_E_mat(ig1,ig2,i_np),SP)<=bose_E_cut*Bose_Temp) then
                     bose_PPA_E=spin_occ*Bose_Temp/real(MPA_E_mat(ig1,ig2,i_np),SP)
                   endif 
                   if (real(MPA_E_mat(ig1,ig2,i_np),SP)> bose_E_cut*Bose_Temp) then
                     bose_PPA_E=spin_occ/(exp(real(MPA_E_mat(ig1,ig2,i_np),SP)/Bose_Temp)-1._SP)
                   endif
                 else
                   bose_PPA_E=spin_occ*Bose_Temp/epsilon(1._SP)
                 endif
               endif
               !
               ! DALV: the factor isc%gamp(ig1,ig2) is included here
               ctmp = -4._DP/spin_occ*pi*isc%rhotw(ig1)*conjg(iscp%rhotw(ig2)) *isc%gamp(ig1,ig2)  *&
&                                 (sign(1._SP,aimag(MPA_E_mat(ig1,ig2,i_np))) *MPA_R_mat(ig1,ig2,i_np))*&
&                     ( (spin_occ-f_kmq+bose_PPA_E)/(W_(i_w)-E_kmq &
&                                             +sign(1._SP,aimag(MPA_E_mat(ig1,ig2,i_np)))*MPA_E_mat(ig1,ig2,i_np))+&
&                       (f_kmq+bose_PPA_E)/(conjg(W_(i_w))-E_kmq   &
&                                             -sign(1._SP,aimag(MPA_E_mat(ig1,ig2,i_np)))*MPA_E_mat(ig1,ig2,i_np)) )
               !
               dp_dummy=dp_dummy+ctmp
               !
             enddo
           enddo
         enddo
         !$omp end parallel do
         !
         dc(i_w) = cmplx(dp_dummy,KIND=SP)
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
 if(io_MULTIPOLE) call IO_and_Messaging_switch("RESTORE")
 !
 call live_timing()
 !
 ! CLEAN
 !
 deallocate(dc)
 deallocate(W_)
 call X_ALLOC_elemental('X')
 call MPA_free()
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
