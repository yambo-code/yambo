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
subroutine COLLISIONS_sex(X,Xk,E,k,q,Xw)
 !
 ! SEX collisions 
 !
 use pars,          ONLY:SP,pi,cZERO
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,spin
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:error
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_QP,PAR_IND_Q,&
&                        PAR_n_Bp_mat_elements,PAR_COM_QP_A2A,&
&                        PAR_IND_Bp_mat,PAR_nQP,PAR_nQ,PAR_COM_G_b_A2A,PAR_COM_Q_index
 use collision,     ONLY:ggwinfo,collision_reset,COLLISIONS_parallel_SC_structure,&
&                        SEX_collisions,nSEX_collisions,&
&                        COLLISIONS_IO_status,scatterBamp_alloc
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,Io_and_Messaging_switch,io_COLLs
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_nb,QP_n_states,QP_table
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use interfaces,    ONLY:WF_load
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use R_lattice,     ONLY:qindx_S,bz_samp
 use SC,            ONLY:SC_bands,B_mat_index
 !
 implicit none
 !
 type(levels)             ::E
 type(bz_samp)            ::k,q,Xk
 type(X_t)                ::X
 type(w_samp)             ::Xw
 !
 ! Work Space
 !
 integer                 :: i_qp,ig1,ig2,iqbz,iqibz,alloc_err,iqs,n_bands_to_load,&
&                           i_n,i_k,i_k_s,i_sp,i_m,i_mp,n_bands(2),N_g_vecs,&
&                           i_np,i_kmq,i_kmq_bz,i_kmq_s,i_kmq_G0,PAR_nQP_local
 !
 type(ggwinfo)           :: isc,iscp
 complex(SP)             :: pre_factor
 !
 complex(SP), allocatable:: EM1s(:,:)
 integer                 :: io_err,ID
 integer, external       :: io_X
 !
#if defined _TIMING
 call timing('SEX Collisions',OPR='start')
#endif
 !
 ! Reset
 !
 alloc_err=0
 !
 call collision_reset(isc)
 call collision_reset(iscp)
 !
 ! Setup
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 call k_expand(k)
 !
 ! Dimensions
 !
 QP_n_G_bands=SC_bands
 !
 n_bands   =QP_n_G_bands
 N_g_vecs  =X%ng
 !
 n_bands_to_load=max(QP_n_G_bands(2),QP_nb)
 !
 call msg('r', '[SEX] Plane waves :',N_g_vecs)
 !
 ! WFs
 !
 if (WF_buffered_IO) then
   !
   call WF_load(WF_buffer,0,1,SC_bands,(/1,k%nibz/),space='B',title='-BUFFER')
   !
 endif
 !
 call WF_load(WF,N_g_vecs,maxval(qindx_S(:,:,2)),(/1,n_bands_to_load/),(/1,k%nibz/),title='-SEX Collisions')
 !
 isc%iqref =0
 isc%ngrho =N_g_vecs
 iscp%ngrho=N_g_vecs
 !
 ! Plasmon-Pole/Static interaction DB I/O
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=io_X(X,Xw,ID)
 if (io_err<0) call error('Incomplete and/or broken PPA/Static diel. fun. database')
 !
 ! Test the spatial Inversion
 !   
 call WF_spatial_invertion(E,Xk)
 !
 ! Allocation
 !
 call X_alloc('X',(/N_g_vecs,N_g_vecs,1/))
 allocate(EM1s(N_g_vecs,N_g_vecs),stat=alloc_err)
 call mem_est("EM1s",(/size(EM1s)/),errors=(/alloc_err/))
 !
 call scatterBamp_alloc(isc)
 call scatterBamp_alloc(iscp)
 allocate(isc%gamp(N_g_vecs,N_g_vecs),isc%rhotw(N_g_vecs),iscp%rhotw(N_g_vecs),stat=alloc_err)
 call mem_est("ISC-GAMP",(/N_g_vecs**2+2*N_g_vecs/),errors=(/alloc_err/))
 !
 ! Parallel Indexes & LIVE-TIMING STEPS
 !
 PAR_nQP_local=PAR_nQP
 if (allocated(COLLISIONS_IO_status)) then
   PAR_nQP_local=count(COLLISIONS_IO_status==-1)
 endif
 call live_timing("SEX collisions (e-e correlation)",PAR_nQP_local*PAR_nQ*PAR_n_Bp_mat_elements)
 !
 do i_qp=1,QP_n_states
   !
   if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
   !
   if (io_COLLs) then
     if (COLLISIONS_IO_status(i_qp)/=-1) cycle
   endif
   !
   i_n     = QP_table(i_qp,1)
   i_m     = QP_table(i_qp,2)
   i_k     = QP_table(i_qp,3)
   i_sp    = spin(QP_table(i_qp,:))
   !
   SEX_collisions(1)%v3_c=cZERO
   !
   do iqbz=1,q%nbz
     !
     if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
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
!$omp parallel do default(shared), private(ig1,ig2)
       do ig1=1,X%ng
         do ig2=1,X%ng
           EM1s(ig1,ig2)=X_mat(ig1,ig2,1)*isc%gamp(ig1,ig2)
         enddo
       enddo
!$omp end parallel do
       !
     endif
     !
     ! Scattering geometry
     !=====================
     !
     !                      W(i_kmq_G0,iqibz,iqs)                    
     !                        _ _ _ _ \ _ _ _ _
     !                       /        /        \
     !                      /                   \
     !                     /     G_{np,mp}       \
     !     (i_n,i_k,1)-->--.---------->----------.---->----(i_m,i_k,1)
     !                          (i_kmq,i_kmq_s)
     !
     i_k_s   = 1
     !
     i_kmq_bz =qindx_S(i_k,iqbz,1)
     i_kmq_G0 =qindx_S(i_k,iqbz,2)
     !
     i_kmq   = k%sstar(i_kmq_bz,1)
     i_kmq_s = k%sstar(i_kmq_bz,2)
     !
     isc%is  =(/i_n,i_k,  i_k_s,  i_sp/)
     isc%os  =(/0  ,i_kmq,i_kmq_s,i_sp/)
     !
     iscp%is =(/i_m,i_k,  i_k_s,  i_sp/)
     iscp%os =(/0,  i_kmq,i_kmq_s,i_sp/)
     !
     isc%qs(1) =i_kmq_G0
     iscp%qs(1)=i_kmq_G0
     !
     do i_mp=n_bands(1),n_bands(2)
       !
       iscp%os(1)=i_mp
       !
       call scatterBamp(iscp)
       !
       do i_np=n_bands(1),n_bands(2)
         !
         ! COLLISIONS(n,n',qp_n_states) = \sum_{G,G',q} ( rho(m,n,G,q,k) x EM1s(G ,G') x rho*(m',n',G',q,k) /|q+G|/|q+G'| )
         !
         if ( .not. PAR_IND_Bp_mat%element_1D( B_mat_index(i_np,i_mp) ) ) cycle
         !
         isc%os(1)=i_np
         !
         if (any(isc%is/=iscp%is).or.any(isc%os/=iscp%os)) then
           call scatterBamp(isc)
         else
           isc%rhotw=iscp%rhotw
         endif
         !
         pre_factor=cZERO
         !
!$omp parallel do default(shared), private(ig1), reduction(+:pre_factor)
         do ig1=1,X%ng
           do ig2=1,X%ng
             pre_factor=pre_factor+isc%rhotw(ig2)*EM1s(ig2,ig1)*conjg(iscp%rhotw(ig1))
           enddo
         enddo
!$omp end parallel do
         !
         SEX_collisions(1)%v3_c(i_np,i_mp,i_kmq)=SEX_collisions(1)%v3_c(i_np,i_mp,i_kmq)-4._SP/spin_occ*pi*pre_factor
         !
         call live_timing(steps=1)
         !
       enddo ! loop on the band i_np
       !
     enddo ! loop on the band i_mp
     !
   enddo ! loop on iqbz
   !
   if (COLLISIONS_parallel_SC_structure) then
     call PP_redux_wait(SEX_collisions(1)%v3_c,COMM=PAR_COM_QP_A2A%COMM)
     call PP_redux_wait(SEX_collisions(1)%v3_c,COMM=PAR_COM_Q_index%COMM)
   else
     call PP_redux_wait(SEX_collisions(1)%v3_c,COMM=PAR_COM_G_b_A2A%COMM)
   endif
   !
   call COLLISIONS_linearize_and_IO("SEX",i_qp,SEX_collisions,nSEX_collisions)
   !
 enddo ! loop on i_qp
 !
 call live_timing()
 !
 ! CLEAN
 !
 deallocate(EM1s)
 call mem_est("EM1s")
 call X_alloc('X')
 call collision_reset(isc)
 call collision_reset(iscp)
 !
#if defined _TIMING
 call timing('SEX Collisions',OPR='stop')
#endif
 !
end subroutine
