!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine COLLISIONS_NEQ_GW_static(X,Xk,E,k,q,Xw)
 !
 ! NEQ GW collisions 
 !
 use pars,          ONLY:SP,pi,schlen,cZERO,rZERO
 use memory_m,      ONLY:mem_est
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:error
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_QP,PAR_Plasma_index,PAR_QP_index,&
&                        PAR_IND_Q,PAR_Q_index,&
&                        PAR_n_Bp_mat_elements,PAR_COM_QP_A2A,PAR_IND_Plasma,&
&                        PAR_IND_Bp_mat,PAR_nQP,PAR_nQ,PAR_COM_G_b_A2A,PAR_COM_Q_index
 use collision,     ONLY:ggwinfo,collision_reset,COLLISIONS_parallel_SC_structure,&
&                        COLLISIONS_element,nGW_NEQ_collisions,GW_NEQ_collisions,&
&                        COLLISIONS_IO_status,scatterBamp_alloc
 use plasma,        ONLY:EH_gas
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,Io_and_Messaging_switch,io_COLLs
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_nb,QP_n_states,QP_table
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use interfaces,    ONLY:WF_load
 use wave_func,     ONLY:WF
 use R_lattice,     ONLY:qindx_S,qindx_C,bz_samp
 use D_lattice,     ONLY:DL_vol
 use wrapper,       ONLY:M_by_V
 use SC,            ONLY:SC_bands,B_mat_index
 !
 implicit none
 !
 !
 type(levels)             ::E
 type(bz_samp)            ::k,q,Xk
 type(X_t)                ::X
 type(w_samp)             ::Xw
 !
 ! Work Space
 !
 integer                  ::i_qp1,i_qp2,i1,i2,iqbz,iqibz,alloc_err,iqs,&
&                           i_n,i_k,i_k_s,i_k_sp,i_J,i_J_mem,PAR_nQP_local,&
&                           i_np,i_kmq,i_kmq_bz,i_kmq_s,i_kmq_sp,i_kmq_G0,&
&                           i_m,i_p,i_p_bz,i_p_s,i_p_sp,i_p_star,n_bands,&
&                           i_mp,i_pmq,i_pmq_s,i_pmq_sp,i_pmq_G0,i_pmq_bz,i_q_mem,i_qp1_mem
 complex(SP), allocatable ::EM1s(:,:)
 type(ggwinfo)            ::isc,iscp
 integer          ::io_err,ID
 integer, external::io_X
 character(schlen)::ch
 complex(SP)      ::local_rhotw(X%ng),pre_factor
 !
 ! Reset...
 !
 alloc_err=0
 !
 call collision_reset(isc)
 call collision_reset(iscp)
 !
 ! Setup...
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 call k_expand(k)
 !
 ! Dimensions...
 !
 QP_n_G_bands=SC_bands
 !
 isc%iqref =0
 isc%ngrho =X%ng
 iscp%ngrho=isc%ngrho
 !
 ! WFs...
 !
 n_bands=QP_nb
 !
 call WF_load(WF,isc%ngrho,maxval(qindx_S(:,:,2)),(/1,n_bands/),(/1,k%nibz/),title='-SC')
 !
 ! Plasmon-Pole/Static interaction DB I/O
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=io_X(X,Xw,ID)
 !
 if (io_err<0) call error('Incomplete and/or broken PPA/Static diel. fun. database')
 !
#if defined _TIMING
 call timing('NEQ GW Collisions',OPR='start')
#endif
 !
 ! Test the spatial Inversion
 !   
 call WF_spatial_invertion(E,Xk)
 !
 ! ALLOCATION
 !------------
 !
 call X_alloc('X',(/X%ng,X%ng,1/))
 allocate(EM1s(X%ng,X%ng),stat=alloc_err)
 call mem_est("EM1s",(/size(EM1s)/),errors=(/alloc_err/))
 !
 call scatterBamp_alloc(isc)
 call scatterBamp_alloc(iscp)
 allocate(isc%gamp(X%ng,X%ng),isc%rhotw(isc%ngrho),iscp%rhotw(isc%ngrho),stat=alloc_err)
 call mem_est("ISC-GAMP",(/X%ng**2+2*isc%ngrho/),errors=(/alloc_err/))
 !
 ! Parallel Indexes & LIVE-TIMING STEPS
 !
 PAR_nQP_local=PAR_nQP
 if (allocated(COLLISIONS_IO_status)) then
   PAR_nQP_local=count(COLLISIONS_IO_status==-1)
 endif
 call live_timing("GW collisions (e-e scattering)",PAR_nQP_local*PAR_nQ)
 !
 do i_qp1=1,QP_n_states
   !
   if (.not.PAR_IND_QP%element_1D(i_qp1)) cycle
   !
   i_qp1_mem=PAR_QP_index(i_qp1)
   !
   if (io_COLLs) then
     if (COLLISIONS_IO_status(i_qp1)/=-1) cycle
     GW_NEQ_collisions(1)%v3_r=rZERO
   endif
   !
   do iqbz=1,q%nbz 
     !
     iqibz    =q%sstar(iqbz,1)
     iqs      =q%sstar(iqbz,2)
     !
     isc%qs   =(/0,iqibz,iqs/)
     iscp%qs  =(/0,iqibz,iqs/)
     !
     if (iqibz/=isc%iqref) then
       !
       call scatterGamp(isc,'c')
       !
       call Io_and_Messaging_switch("-fragments_IO_log")
       !
       ! I/O
       !
       if (q%nbz==1) then
         call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/1,2,3/),ID=ID)
       else
         if (iqbz ==1) call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/1,2,2*iqibz+1/),ID=ID)
         if (iqbz>  1) call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
       endif
       io_err=io_X(X,Xw,ID)
       !
       call Io_and_Messaging_switch("+fragments_IO_log")
       !
       ! Poles and Residuals
       !
       forall(i1=1:X%ng) X_mat(i1,i1,1)=X_mat(i1,i1,1)+1.
       !
!$omp parallel do default(shared), private(i1,i2)
       do i1=1,X%ng
         do i2=1,X%ng
           ! Davide: which is the reason of this if ? It seems useless ...
           if (iqibz==1) then
             EM1s(i1,i2)=X_mat(i1,i2,1)*isc%gamp(i1,i2)
           else
             EM1s(i1,i2)=X_mat(i1,i2,1)*isc%gamp(i1,i2)
           endif
         enddo
       enddo
!$omp end parallel do
       if (iqibz==1) EM1s(1,1)=X_mat(1,1,1)*sqrt(isc%gamp(1,1)/DL_vol/q%nbz)
       !
     endif
     !
     if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
     i_q_mem=PAR_Q_index(iqbz)
     !
     ! Scattering geometry (I)
     !=========================
     i_n     = QP_table(i_qp1,1)
     i_m     = QP_table(i_qp1,2)
     i_k     = QP_table(i_qp1,3)
     !
     i_k_s   = 1
     !i_kbz  = i_k
     !
     i_kmq_bz =qindx_S(i_k,iqbz,1)
     i_kmq_G0 =qindx_S(i_k,iqbz,2)
     !
     i_kmq   = k%sstar(i_kmq_bz,1)
     i_kmq_s = k%sstar(i_kmq_bz,2)
     !
     ! Spins
     !
     i_k_sp  = spin(QP_table(i_qp1,:))
     i_p_sp  = i_k_sp
     i_kmq_sp= i_k_sp
     i_pmq_sp= i_k_sp
     !
     isc%is  =(/i_n,i_k,i_k_s,i_k_sp/)
     isc%os  =(/0  ,i_kmq,i_kmq_s,i_kmq_sp/)
     !
     isc%qs(1)=i_kmq_G0
     !
     !
     i_np     = QP_table(i_qp1,2)
     isc%os(1)= i_np
     !
     call scatterBamp(isc)
     !
     do i_qp2=1,QP_n_states
       !
       i_m     = QP_table(i_qp2,1)
       i_mp    = QP_table(i_qp2,2)
       i_p     = QP_table(i_qp2,3)
       !
       do i_p_star=1,k%nstar(i_p)
         !
         ! Scattering geometry (II)
         !==========================
         !
         i_p_s =k%star(i_p,i_p_star)
         i_p_bz=k%k_table(i_p,i_p_s)
         !
         i_pmq_bz = qindx_C(i_p_bz,iqbz,1)
         i_pmq_G0 = qindx_C(i_p_bz,iqbz,2)
         !
         i_pmq   = k%sstar(i_pmq_bz,1)
         i_pmq_s = k%sstar(i_pmq_bz,2)
         !
         ! (i_m,i_p,i_p_s)--<--.--<--(i_mp,i_pmq,i_pmq_s)
         !                     |
         !                    /|\ W(i_pmq_G0,iqibz,iqs)
         !                     |
         !     (i_n,i_k,1)-->--.-->--(i_np,i_kmq,i_kmq_s)
         !
         ! W_{nn' k}{mm' p}^{q} = [W_{n'n k+q}{m'm p+q}^{-q}]^{*} = W_{mm' p}{nn' k}^{q}
         !
         i_J=EH_gas%tab_m1(i_qp2,i_p_s,i_q_mem)
         !
         if (.not.PAR_IND_Plasma%element_1D(i_J)) cycle
         i_J_mem=PAR_Plasma_index(i_J)
         !
         iscp%is =(/i_m,i_p,i_p_s,i_p_sp/)
         iscp%os =(/i_mp,i_pmq,i_pmq_s,i_pmq_sp/)
         !
         iscp%qs(1)=i_pmq_G0
         !
         call scatterBamp(iscp)
         !
         call M_by_V('N', X%ng, EM1s, conjg(iscp%rhotw), local_rhotw)
         !
         pre_factor=0.0_SP
!$omp parallel do default(shared), private(i1), reduction(+:pre_factor)
         do i1=1,X%ng
           pre_factor=pre_factor+isc%rhotw(i1)*local_rhotw(i1)
         enddo
!$omp end parallel do
         !
         if (io_COLLs) then
           !
           GW_NEQ_collisions(1)%v3_r(i_J,1,iqbz)=&
           &  GW_NEQ_collisions(1)%v3_r(i_J,1,iqbz)+abs(pre_factor)**2._SP
           !
         else
           !
           GW_NEQ_collisions(i_qp1_mem)%v3_r(i_J_mem,1,i_q_mem)=&
           &  GW_NEQ_collisions(i_qp1_mem)%v3_r(i_J_mem,1,i_q_mem)+abs(pre_factor)**2._SP
           !
         endif
         !
       enddo
       !
     enddo
     !
     call live_timing(steps=1)
     !
   enddo 
   !
   if (io_COLLs) then
     if (COLLISIONS_parallel_SC_structure) then
       call PP_redux_wait(GW_NEQ_collisions(1)%v3_r,COMM=PAR_COM_QP_A2A%COMM)
       call PP_redux_wait(GW_NEQ_collisions(1)%v3_r,COMM=PAR_COM_Q_index%COMM)
     else
       call PP_redux_wait(GW_NEQ_collisions(1)%v3_r,COMM=PAR_COM_G_b_A2A%COMM)
     endif
   endif
   !
   if (io_COLLs) call COLLISIONS_linearize_and_IO("GW_NEQ",i_qp1,GW_NEQ_collisions,nGW_NEQ_collisions)
   !
 enddo
 !
 call live_timing()
 !
#if defined _TIMING
 call timing('GW NEQ Collisions',OPR='stop')
#endif
 !
 ! CLEAN
 !
 deallocate(EM1s)
 call mem_est("EM1s")
 call X_alloc('X')
 call collision_reset(isc)
 call collision_reset(iscp)
 !
end subroutine
