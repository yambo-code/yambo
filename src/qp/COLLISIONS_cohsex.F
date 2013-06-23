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
subroutine COLLISIONS_cohsex(X,Xk,E,k,q,Xw)
 !
 ! COHSEX collisions 
 !
 use pars,          ONLY:SP,pi,schlen,cZERO
 use memory_m,      ONLY:mem_est
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin
 use timing,        ONLY:live_timing
 use com,           ONLY:error
 use real_time,     ONLY:B_mat_index
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_QP,&
&                        PAR_IND_Q,PAR_n_B_mat_elements,PAR_COM_QP_A2A,PAR_COM_G_b_A2A,PAR_QP_index,&
&                        PAR_IND_B_mat,PAR_Q_index,PAR_IND_QPp,PAR_QPp_index,PAR_nQ,PAR_nQP,PAR_nQPp
 use collision,     ONLY:ggwinfo,collision_reset,COLLISIONS
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,IO_and_Messaging_switch
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_nb,QP_n_states,QP_table
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use wave_func,     ONLY:WF_load,WF_free
 use R_lattice,     ONLY:qindx_S,bz_samp
 use D_lattice,     ONLY:DL_vol,nsym
 use wrapper,       ONLY:M_by_V
 use collisions_IO, ONLY:TEMP_COLLISIONS_v,l_STORE_COLLISIONS
 use drivers,       ONLY:l_carrier_dynamics
 use SC,            ONLY:SC_bands
 use fragments,     ONLY:fragment_status
 !
 implicit none
 type(levels) ::E
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(w_samp) ::Xw
 !
 ! Work Space
 !
 integer                  ::i_qp1,i_qp2,i1,i2,iqbz,iqibz,alloc_err,iqs,&
&                           i_n,i_k,i_k_s,i_k_sp,&
&                           i_np,i_kmq,i_kmq_s,i_kmq_sp,i_kmq_G0,&
&                           i_m,i_p,i_p_s,i_p_sp,i_p_star,&
&                           i_mp,i_pmq,i_pmq_s,i_pmq_sp,i_pmq_G0,i_pmq_bz,i_q_mem,i_qp2_mem,i_qp1_mem
 complex(SP), allocatable ::EM1s(:,:)
 type(ggwinfo)            ::isc,iscp
 integer          ::io_err,ID
 integer, external::ioX
 character(schlen)::ch
 complex(SP)      ::local_rhotw(X%ng),pre_factor
 !
 ! Reset...
 !
 !
 call collision_reset(isc)
 call collision_reset(iscp)
 !
 ! Setup...
 !
 call k_sym2sym(k,'k')
 call k_sym2sym(q,'k')
 call k_expand(k)
 !
 ! Dimensions...
 !
 QP_n_G_bands(1)=SC_bands(1)
 !
 isc%iqref =0
 isc%ngrho =X%ng
 iscp%ngrho=isc%ngrho
 !
 ! WFs...
 !
 call WF_load(isc%ngrho,maxval(qindx_S(:,:,2)),(/1,max(QP_n_G_bands(2),QP_nb)/),(/1,k%nibz/),title='-SC')
 !
 ! Plasmon-Pole/Static interaction DB I/O
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioX(X,Xw,ID)
 !
 ! Test the spatial Inversion
 !   
 call WF_spatial_invertion(E,Xk)
 !
 ! ALLOCATION
 !------------
 !
 call X_alloc('X',(/X%ng,X%ng,1/))
 allocate(EM1s(X%ng,X%ng))
 call mem_est("EM1s",(/size(EM1s)/),errors=(/alloc_err/))
 !
 allocate(isc%gamp(X%ng,X%ng),isc%rhotw(isc%ngrho),iscp%rhotw(isc%ngrho),stat=alloc_err)
 call mem_est("ISC-GAMP",(/X%ng**2+2*isc%ngrho/),errors=(/alloc_err/))
 !
 ! Parallel Indexes & LIVE-TIMING STEPS
 !
 if (l_carrier_dynamics) then
   call live_timing("COHSEX collisions",PAR_nQP*PAR_nQ*PAR_nQPp)
 else
   call live_timing("COHSEX collisions",PAR_nQP*PAR_nQ*PAR_n_B_mat_elements)
 endif
 if (io_err/=0) call error('Incomplete and/or broken PPA/Static diel. fun. database')
 !
 QP_loop: do i_qp1=1,QP_n_states
   !
   if (.not.PAR_IND_QP%element_1D(i_qp1)) cycle
   !
   i_qp1_mem=PAR_QP_index(i_qp1)
   !
   if (l_STORE_COLLISIONS) then
     if (fragment_status(i_qp1)/=-1) then
       call live_timing(steps=PAR_nQ*PAR_n_B_mat_elements)
       cycle
     endif
   else
     allocate(COLLISIONS(i_qp1_mem)%v3(PAR_nQPp,nsym,PAR_nQ))
   endif
   !
   TEMP_COLLISIONS_v=cZERO
   !
   Q_loop: do iqbz=1,q%nbz 
     !
     i_kmq_G0 =qindx_S(QP_table(i_qp1,3),iqbz,2)
     iqibz    =q%sstar(iqbz,1)
     iqs      =q%sstar(iqbz,2)
     isc%qs  =(/i_kmq_G0,iqibz,iqs/)
     !
     if (iqibz/=isc%iqref) then
       !
       call scatterGamp(isc,'c')
       !
       call IO_and_Messaging_switch("-fragments_IO_log")
       !
       ! I/O
       !
       if (q%nbz==1) call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/1,2,3/),ID=ID)
       if (iqbz ==1) call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/1,2,2*iqibz+1/),ID=ID)
       if (iqbz>  1) call io_control(ACTION=OP_RD_CL,COM=NONE, SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
       io_err=ioX(X,Xw,ID)
       !
       call IO_and_Messaging_switch("+fragments_IO_log")
       !
       ! Poles and Residuals
       !
       forall(i1=1:X%ng) X_mat(i1,i1,1)=X_mat(i1,i1,1)+1.
       !
       if (iqibz==1) then
         forall(i1=1:X%ng,i2=1:X%ng) EM1s(i1,i2)=X_mat(i1,i2,1)*isc%gamp(i1,i2)
         EM1s(1,1)=X_mat(1,1,1)*sqrt(isc%gamp(1,1)/DL_vol/q%nbz)
       else
         forall(i1=1:X%ng,i2=1:X%ng) EM1s(i1,i2)=X_mat(i1,i2,1)*isc%gamp(i1,i2)
       endif
       !
     endif
     !
     if (l_carrier_dynamics) then
       if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
       i_q_mem=PAR_Q_index(iqbz)
     else
       if (.not.PAR_IND_Q%element_1D(q%sstar(iqbz,1))) cycle
     endif
     !
     ! Scattering geometry (I)
     !=========================
     i_n     = QP_table(i_qp1,1)
     i_m     = QP_table(i_qp1,2)
     i_k     = QP_table(i_qp1,3)
     i_k_s   = 1
     i_kmq   = k%sstar(qindx_S(i_k,iqbz,1),1)
     i_kmq_s = k%sstar(qindx_S(i_k,iqbz,1),2)
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
     iscp%is =(/i_m,i_k,i_k_s,i_k_sp/)
     iscp%os =(/0  ,i_kmq,i_kmq_s,i_kmq_sp/)
     iscp%qs =isc%qs
     !
     if (l_carrier_dynamics) then
       !
       i_np     = QP_table(i_qp1,2)
       isc%os(1)= i_np
       !
       call scatterBamp(isc)
       !
       do i_qp2=1,QP_n_states
         !
         if (.not.PAR_IND_QPp%element_1D(i_qp2)) cycle
         i_qp2_mem=PAR_QPp_index(i_qp2)
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
           !
           call RT_EE_scattering_kinematics(k,q,i_p,i_p_s,iqibz,iqs,i_pmq_bz,i_pmq_G0)
           !
           i_pmq   = k%sstar(i_pmq_bz,1)
           i_pmq_s = k%sstar(i_pmq_bz,2)
           !
           iscp%is =(/i_m,i_p,i_p_s,i_p_sp/)
           iscp%os =(/i_mp,i_pmq,i_pmq_s,i_pmq_sp/)
           iscp%qs =(/i_pmq_G0,iqibz,1/)
           !
           call scatterBamp(iscp)
           !
           call M_by_V('N', X%ng, EM1s, conjg(iscp%rhotw), local_rhotw)
           !
           if (l_STORE_COLLISIONS) then
             !
             TEMP_COLLISIONS_v(i_qp2,i_p_s,iqbz)=sum(isc%rhotw(1:X%ng)*local_rhotw(1:X%ng))
             !
           else
             !
             COLLISIONS(i_qp1_mem)%v3(i_qp2_mem,i_p_s,i_q_mem)=sum(isc%rhotw(1:X%ng)*local_rhotw(1:X%ng))
             !
           endif
           !
         enddo
         !
         call live_timing(steps=1)
         !
       enddo
       !
     else
       !
       do i_np=QP_n_G_bands(1),QP_n_G_bands(2)
         !
         isc%os(1)=i_np
         !
         isc_loop: do i_mp=QP_n_G_bands(1),QP_n_G_bands(2)
           if ( PAR_IND_B_mat%element_1D( B_mat_index(i_np,i_mp) ) ) then
             call scatterBamp(isc)
             exit isc_loop
           endif
         enddo isc_loop
         !
         do i_mp=QP_n_G_bands(1),QP_n_G_bands(2)
           !
           if ( .not. PAR_IND_B_mat%element_1D( B_mat_index(i_np,i_mp) ) ) cycle
           !
           iscp%os(1)=i_mp
           !
           if (any(isc%is/=iscp%is).or.any(isc%os/=iscp%os)) then
             call scatterBamp(iscp)
           else
             iscp%rhotw=isc%rhotw
           endif
           !
           call M_by_V('N', X%ng, EM1s, conjg(iscp%rhotw), local_rhotw)
           !
           pre_factor=sum(isc%rhotw(1:X%ng)*local_rhotw(1:X%ng))
           !
           TEMP_COLLISIONS_v(i_np,i_mp,isc%os(2))=TEMP_COLLISIONS_v(i_np,i_mp,isc%os(2))-4._SP/spin_occ*pi*pre_factor
           !
           if (q%sstar(iqbz,2)==1) call live_timing(steps=1)
           !
         enddo ! loop on the band i_np
         !
       enddo ! loop on the band i_m
       !
     endif
   enddo Q_loop 
   !
   if (l_carrier_dynamics) then
     if (l_STORE_COLLISIONS) call PP_redux_wait(TEMP_COLLISIONS_v,COMM=PAR_COM_G_b_A2A%COMM)
   else
     call PP_redux_wait(TEMP_COLLISIONS_v,COMM=PAR_COM_QP_A2A%COMM)
   endif
   !
   if (l_STORE_COLLISIONS) call COLLISIONS_store(i_qp1)
   !
 enddo QP_loop
 !
 call live_timing()
 !
 ! CLEAN
 !
 deallocate(EM1s)
 call mem_est("EM1s")
 call X_alloc('X')
 if (l_STORE_COLLISIONS) call WF_free()
 call collision_reset(isc)
 call collision_reset(iscp)
 !
end subroutine
