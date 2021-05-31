!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine QP_ELEL_Lifetimes(X,Xen,Xk,en,k,q,qp,Xw,Dip)
 !
 ! This routine calculates the electronic Lifetimes
 !
 use pars,          ONLY:SP,schlen,pi,cI
 use units,         ONLY:HA2EV
 use drivers,       ONLY:l_life
 use electrons,     ONLY:levels,spin
 use frequency,     ONLY:w_samp,coarse_grid_index,coarse_grid_N,W_reset
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,&
&                        PAR_IND_Q_ibz,PAR_QP_index,PAR_Q_bz_index,PAR_G_bands_index,&
&                        n_WF_bands_to_load,PAR_Q_ibz_index,PAR_nQP
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_index,&
&                        PARALLEL_WF_distribute
 use interfaces,    ONLY:QP_state_print,WF_load,WF_free
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use wave_func,     ONLY:WF
 use DIPOLES,       ONLY:DIPOLE_t
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:DUMP,NONE,io_RESPONSE,io_SCREEN,OP_RD,OP_RD_CL,RD,RD_CL
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_n_states,QP_G_damp,QP_table,&
&                        QP_n_W_freqs,QP_n_W_freqs_redux,e2et,h2ht
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use functions,     ONLY:bose_decay
 use R_lattice,     ONLY:qindx_S,bz_samp
 use D_lattice,     ONLY:nsym,i_space_inv,i_time_rev,mag_syms
 use wrapper,       ONLY:Vstar_dot_V,V_dot_V
 use wrapper_omp,   ONLY:Vstar_dot_V_omp,V_dot_V_omp
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 type(levels)   ::en,Xen
 type(bz_samp)  ::Xk,k,q
 type(X_t)      ::X
 type(QP_t)     ::qp
 type(w_samp)   ::Xw
 type(DIPOLE_t) ::Dip
 !
 ! WorkSpace
 !
 type(elemental_collision) ::scattering_main,scattering_prime
 type(w_samp)     ::X_life_W(q%nibz)
 integer          ::i1,i2,i3,i_or,iqbz,iqibz,ib,i_err,i_q_to_start,iqs,&
&                   io_err,X_id,timing_steps,i_q_mem,X_range(2),i_qp,i_qp_mem,i_q_W_mem,i_b_mem
 complex(SP)      :: lrhotw(X%ng),X_mat_ws(X%ng,X%ng)
 real(SP)            ::life_Fe,life_Fh
 integer, allocatable::life_W_table(:,:)
 integer, external   ::io_X,X_dielectric_matrix,QP_life_transitions
 logical             ::X_is_TR_rotated
 !
 call timing('GW Lifetimes',OPR='start')
 !
 scattering_main%iqref=0
 !
 ! Main Loop
 !===========
 !
 call PP_redux_wait()
 !
 do iqbz=i_q_to_start,q%nbz
   !
   scattering_main%qs(2:)=(/q%sstar(iqbz,1),q%sstar(iqbz,2)/)
   iqibz=scattering_main%qs(2)
   iqs  =scattering_main%qs(3)
   !
   i_q_W_mem=PAR_Q_bz_index(iqbz)
   !
   if (.not.PAR_IND_Q_ibz%element_1D(iqibz)) cycle
   !
   if (iqibz/=scattering_main%iqref) then
     !
     i_q_mem=PAR_Q_ibz_index(iqibz)
     X_range=(/1,QP_n_W_freqs_redux/)
     !
     if (.not.io_RESPONSE) X_range=(/QP_n_W_freqs_redux*(i_q_mem-1)+1,QP_n_W_freqs_redux*(i_q_mem-1)+X_life_W(iqibz)%n_freqs/)
     !
     call scatter_Gamp(scattering_main,'c')
     !
     ! I/O
     !
     if (io_RESPONSE) then
       !
       call io_control(ACTION=RD,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=X_id)
       !
       io_err=io_X(X,X_life_W(iqibz),X_id)
       !
     endif
     !
     call X_delta_part(X%ng,X_life_W(iqibz)%n_freqs,X_mat(:,:,X_range(1):X_range(2)),scattering_main%gamp)
     !
     QP_n_W_freqs=0
     !
     X_is_TR_rotated=.false.
     !
   endif
   !
   if (iqs>nsym/(i_time_rev+1) .and. (i_space_inv==0.or.mag_syms) .and..not.X_is_TR_rotated) then
     X_is_TR_rotated=.true.
     do i3=X_range(1),X_range(2)
       forall(i1=1:X%ng,i2=1:X%ng) X_mat_ws(i2,i1)=X_mat(i1,i2,i3)
       X_mat(:,:,i3)=X_mat_ws(:,:)
     enddo
   endif
   !
   qp_loop: do i_qp=1,QP_n_states
     !
     i_qp_mem=PAR_QP_index(i_qp)
     !
     scattering_main%is=(/QP_table(i_qp,1),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     scattering_main%os(2:)=(/k%sstar(qindx_S(scattering_main%is(2),iqbz,1),:),spin(QP_table(i_qp,:))/)
     scattering_prime%is=(/QP_table(i_qp,2),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     !
     scattering_main%qs(1)=qindx_S(QP_table(i_qp,3),iqbz,2)
     scattering_prime%qs=scattering_main%qs
     !
     if (.not.PAR_IND_QP%element_1D(i_qp)) then
       !
       ! Bug spotted by AM (Feb 2015). The QP_n_W_freqs was erronously accumulated
       ! in the loop when the code was run in parallel. These lines are a short work-around
       ! that should be replaced by a full parallelization also of the frequencies tabulation
       !
       do ib=QP_n_G_bands(1),QP_n_G_bands(2)
         scattering_main%os(1)=ib
         i_or=IOR(e2et((/scattering_main%is(:2),scattering_main%is(4)/),            &
&                      (/scattering_main%os(:2),scattering_main%os(4)/),en,life_Fe),&
&                 h2ht((/scattering_main%is(:2),scattering_main%is(4)/),            &
&                      (/scattering_main%os(:2),scattering_main%os(4)/),en,life_Fh))
         QP_n_W_freqs=QP_n_W_freqs+i_or
       enddo
       cycle qp_loop
     endif
     !
     bands_loop: do ib=QP_n_G_bands(1),QP_n_G_bands(2)
       !
       scattering_main%os(1)=ib
       !
       i_b_mem=PAR_G_bands_index(ib)
       !
       i_or=IOR(e2et((/scattering_main%is(:2),scattering_main%is(4)/),            &
&                    (/scattering_main%os(:2),scattering_main%os(4)/),en,life_Fe),&
&               h2ht((/scattering_main%is(:2),scattering_main%is(4)/),            &
&                    (/scattering_main%os(:2),scattering_main%os(4)/),en,life_Fh))
       QP_n_W_freqs=QP_n_W_freqs+i_or
       !
       ! The Parallel cycle has to be here in order to have a proper accumulation of QP_n_W_freqs
       !
       if (.not.PAR_IND_G_b%element_1D(ib))  cycle bands_loop
       !
       scattering_prime%os=scattering_main%os
       !
       call live_timing(steps=1)
       !
       if (i_or==0) cycle
       !
       call scatter_Bamp(scattering_main)
       scattering_prime%rhotw=scattering_main%rhotw
       !
       if (any(scattering_main%is/=scattering_prime%is)) call scatter_Bamp(scattering_prime)
       !
       i2=life_W_table(iqibz,QP_n_W_freqs)
       !
       do i3=1,X%ng
         lrhotw(i3)=V_dot_V_omp(X%ng,scattering_main%rhotw,X_mat(:,i3,i2+X_range(1)-1))
       enddo
       !
       ! To compensate the Tel/w divergence of the Bose function at finite
       ! Tel I multiply the X_mat function by [w/(Tel*Bose_E_cut)]^2
       !
       ! Note that the same procedure is applied in QP_W2Sc when S_c is
       ! calculated
       ! 
       qp%E(i_qp)=qp%E(i_qp)-2._SP*cI*pi*&
&                 (life_Fe+life_Fh)*bose_decay( real(X_life_W(iqibz)%p(i2)) )*&
&                 Vstar_dot_V_omp(X%ng,scattering_prime%rhotw,lrhotw)
       !
       !
     enddo bands_loop
   enddo qp_loop
   !
 enddo 
 !
 if (io_RESPONSE) then
   call io_control(ACTION=RD_CL,COM=NONE,SEC=(/1/),ID=X_id)
   io_err=io_X(X,Xw,X_id)
 endif
 !
 call live_timing()
 call PP_redux_wait()
 !
 ! CLEAN
 !
 YAMBO_FREE(life_W_table)
 call PP_redux_wait(qp%E)
 do iqibz=1,q%nibz
   call W_reset(X_life_W(iqibz))
 enddo
 !
 call X_alloc('X')
 call WF_free(WF)
 call elemental_collision_free(scattering_main)
 call elemental_collision_free(scattering_prime)
 call PP_redux_wait()
 !
 call timing('GW Lifetimes',OPR='stop')
 !
end subroutine
