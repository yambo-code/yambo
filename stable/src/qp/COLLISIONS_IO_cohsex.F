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
subroutine COLLISIONS_IO_cohsex(X,Xk,E,k,q,Xw)
 !
 ! COHSEX collisions 
 !
 use pars,          ONLY:SP,pi,schlen,cZERO
 use memory_m,      ONLY:mem_est
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin
 use timing,        ONLY:live_timing
 use com,           ONLY:error
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,    ONLY:PARALLEL_index
 use collision,     ONLY:ggwinfo,collision_reset
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,RD_CL,OP_RD,RD_CL_IF_END
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_nb,QP_n_states,QP_table
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use wave_func,     ONLY:WF_load,WF_free
 use R_lattice,     ONLY:qindx_S,bz_samp,qindx_X
 use D_lattice,     ONLY:DL_vol
 use wrapper,       ONLY:M_by_V
 use collisions_IO, ONLY:TEMP_COLLISIONS_v
 use WF_distribute, ONLY:WF_states_setup
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
&                           i_m,i_p,i_p_s,i_p_sp,i_p_star,i_p_bz,&
&                           i_mp,i_pmq,i_pmq_s,i_pmq_sp,i_pmq_G0
 complex(SP), allocatable ::EM1s(:,:)
 type(ggwinfo)            ::isc,iscp
 type(PP_indexes)         ::px
 integer          ::io_err,ID
 integer, external::ioX
 character(schlen)::ch
 complex(SP)      ::local_rhotw(X%ng),pre_factor
 !
 ! Reset...
 !
 call collision_reset(isc)
 call collision_reset(iscp)
 call PP_indexes_reset(px)
 !
 ! Setup...
 !
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
 ! Quasi-particle list
 ! (all processors have all qp-states)
 call WF_states_setup(qp_to_load=(/1,QP_n_states/),par_loop=.FALSE.)
 !
 ! Scattering states are distributed
 call WF_states_setup(b_to_load=QP_n_G_bands,qp_to_load=(/1,QP_n_states/),q_to_load=(/1,q%nbz/),k=k)
 !
 ! Report
 call WF_states_setup(b_to_load=QP_n_G_bands,report=.TRUE.)
 !
 ! WFs
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
 call X_alloc('X',(/X%ng,X%ng,1/))
 allocate(EM1s(X%ng,X%ng),stat=alloc_err)
 call mem_est("EM1s",(/size(EM1s)/),errors=(/alloc_err/))
 !
 allocate(isc%gamp(X%ng,X%ng),isc%rhotw(isc%ngrho),iscp%rhotw(isc%ngrho),stat=alloc_err)
 call mem_est("ISC-GAMP",(/X%ng**2+2*isc%ngrho/),errors=(/alloc_err/))
 !
 ! Parallel Indexes & LIVE-TIMING STEPS
 !
 call PARALLEL_index(px,(/QP_n_states/))
 !
 call PP_redux_wait
 !
 call live_timing("COHSEX collisions",px%n_of_elements(myid+1)) 
 !
 QP_loop: do i_qp1=1,QP_n_states
   !
   if (.not.px%element_1D(i_qp1)) cycle
   !
   if (fragment_status(i_qp1)/=-1) then
     call live_timing(steps=1)
     cycle
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
       ! I/O
       !
       if (iqbz ==1) call io_control(ACTION=OP_RD,COM=NONE,       SEC=(/1,2,2*iqibz+1/),ID=ID)
       if (q%nbz==1) call io_control(ACTION=OP_RD_CL,COM=NONE,    SEC=(/1,2,3/),ID=ID)
       if (iqbz>  1) call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
       io_err=ioX(X,Xw,ID)
       !
       ! Poles and Residuals
       !
       if(l_carrier_dynamics) forall(i1=1:X%ng) X_mat(i1,i1,1)=X_mat(i1,i1,1)+1._SP
       !
       if (iqibz==1) then
         forall(i1=1:X%ng,i2=1:X%ng) EM1s(i1,i2)=X_mat(i1,i2,1)*isc%gamp(i1,i2)
         if(l_carrier_dynamics) EM1s(1,1)=X_mat(1,1,1)*sqrt(isc%gamp(1,1)/DL_vol/q%nbz)
       else
         forall(i1=1:X%ng,i2=1:X%ng) EM1s(i1,i2)=X_mat(i1,i2,1)*isc%gamp(i1,i2)
       endif
       !
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
           i_p_bz=i_p_star
           if (i_p>1) i_p_bz=sum(k%nstar(:i_p-1))+i_p_star
           !
           i_pmq   = k%sstar(qindx_X(iqibz,i_p_bz,1),1)
           i_pmq_s = k%sstar(qindx_X(iqibz,i_p_bz,1),2)
           i_pmq_G0= qindx_X(iqibz,i_p_bz,2)
           !
           iscp%is =(/i_m,i_p,i_p_s,i_p_sp/)
           iscp%os =(/i_mp,i_pmq,i_pmq_s,i_pmq_sp/)
           iscp%qs =(/i_pmq_G0,iqibz,1/)
           !
           call scatterBamp(iscp)
           !
           call M_by_V('N', X%ng, EM1s, conjg(iscp%rhotw), local_rhotw)
           !
           TEMP_COLLISIONS_v(i_qp2,i_p_s,iqbz)=sum(isc%rhotw(1:X%ng)*local_rhotw(1:X%ng))
           !
         enddo
         !
       enddo
       !
     else
       !
       do i_np=QP_n_G_bands(1),QP_n_G_bands(2)
         !
         isc%os(1)=i_np
         !
         call scatterBamp(isc)
         !
         do i_mp=QP_n_G_bands(1),QP_n_G_bands(2)
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
         enddo ! loop on the band i_np
         !
       enddo ! loop on the band i_m
       !
     endif
   enddo Q_loop 
   !
   call COLLISIONS_IO_store(i_qp1)
   !
   call live_timing(steps=1)
   !
 enddo QP_loop
 !
 call live_timing()
 !
 ! CLEAN
 !
 deallocate(isc%gamp,isc%rhotw,iscp%rhotw,EM1s)
 call mem_est("ISC-GAMP GW-E EM1s")
 call X_alloc('X')
 call WF_free()
 call collision_reset(isc)
 call collision_reset(iscp)
 call PP_indexes_reset(px)
 !
end subroutine
