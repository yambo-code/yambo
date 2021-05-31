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
subroutine QP_ELEL_real_axis_driver(X,Xen,Xk,en,k,q,qp,Xw,Dip,GW_iter)
 !
 ! This routine drives the calculations of 
 ! 
 ! [1] Real axis GW S.E.
 ! [2] Electronic Lifetimes
 !
 use pars,          ONLY:SP,schlen,pi,cZERO,cI
 use units,         ONLY:HA2EV
 use drivers,       ONLY:l_life
 use electrons,     ONLY:levels,spin,spin_occ
 use frequency,     ONLY:w_samp,coarse_grid_index,coarse_grid_N,W_reset
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,&
&                        PAR_IND_Q_ibz,PAR_QP_index,PAR_Q_bz_index,PAR_G_bands_index,&
&                        n_WF_bands_to_load,PAR_Q_ibz_index,PAR_nQP,&
&                        PAR_nG_bands,PAR_nQ_bz
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_index,&
&                        PARALLEL_WF_distribute
 use interfaces,    ONLY:QP_state_print,WF_load,WF_free
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use wave_func,     ONLY:WF
 use DIPOLES,       ONLY:DIPOLE_t
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:manage_action,REP,VERIFY,DUMP,NONE,io_RESPONSE,io_SCREEN,&
&                        OP_WR_CL,OP_APP_CL,RD_CL_IF_END,OP_RD,OP_RD_CL,RD,RD_CL
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_dSc_steps,QP_Sc,QP_n_states,QP_G_damp,QP_table,QP_dSc_delta,&
&                        QP_W,QP_solver,QP_W_er,QP_W_dr,QP_n_W_freqs,QP_Sc_steps,&
&                        QP_n_W_freqs_redux,e2et,h2ht
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use functions,     ONLY:bose_decay
 use R_lattice,     ONLY:qindx_S,bz_samp
 use D_lattice,     ONLY:nsym,i_space_inv,i_time_rev,mag_syms
 use stderr,        ONLY:intc
 use wrapper,       ONLY:Vstar_dot_V,V_dot_V
 use wrapper_omp,   ONLY:Vstar_dot_V_omp,V_dot_V_omp
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 type(levels) ::en,Xen
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 type(w_samp) ::Xw
 type(DIPOLE_t)       :: Dip
 integer      ::GW_iter
 !
 ! WorkSpace
 !
 type(elemental_collision) ::scattering_main,scattering_prime
 integer          ::i1,i2,i3,i4,i_or,iqbz,iqibz,ib,i_err,i_q_to_start,iqs,&
&                   io_err,X_id,WID,IO_ACT,timing_steps,i_q_mem,X_range(2),i_qp,i_qp_mem,i_q_W_mem,i_b_mem
 complex(SP)      :: lrhotw(X%ng),X_mat_ws(X%ng,X%ng),W_dummy
 real(SP)            ::life_Fe,life_Fh
 integer, allocatable::life_W_table(:,:)
 integer, external   ::io_X,io_QP_and_GF,X_dielectric_matrix,QP_life_transitions
 character(schlen)   ::ch,SEC_name
 logical             ::X_is_TR_rotated
 !
 ! New ...
 integer                   ::nW,iW
 type(w_samp), allocatable ::W(:)
 !
 ! Resets
 !========
 !...scatterings
 call elemental_collision_free(scattering_main)
 call elemental_collision_free(scattering_prime)
 !
 !...W's
 if (     l_life) nW=q%nibz
 if (.not.l_life) nW=qp%n_states
 allocate(W(nW))
 do iW=1,nW
  call W_reset(W(iW))
  if (l_life)  W(iW)%dr=Xw%dr
 enddo
 !
 !... k -> BZ
 call k_expand(k)
 !
 ! ALLOCATION 
 !============
 ! W energy and damping ranges
 !-----------------------------
 if (.not.l_life) then
   YAMBO_ALLOC(QP_W_er,(q%nbz,2))
   YAMBO_ALLOC(QP_W_dr,(q%nbz,2))
 endif
 !
 ! Section 
 !=========
 if (l_life     ) SEC_name='E-E Lifetimes'
 if (.not.l_life) SEC_name='G'//trim(intc(GW_iter))//"W0 on the real axis"
 !
 if (GW_iter==0) call section('+',trim(SEC_name))
 if (GW_iter> 0) call section('=',trim(SEC_name))
 !
 if (.not.l_life)  call msg('r', '[GW] Bands range     ',QP_n_G_bands)
 call                   msg('r', '[GW] G damping       ',QP_G_damp*HA2EV,"[eV]")
 !
 call QP_state_print( )
 !
 ! W Energy points
 !=================
 call section('+','Screened interaction transition points selector')
 call QP_ELEL_W_energy_points( nW, W , en, k, q, qp)
 !
 ! Dynamical interaction
 !=======================
 !
 ! Eval
 if (l_life) then
  do iqibz=1,q%nibz
     X%iq=iqibz
     i_err=X_dielectric_matrix(Xen,Xk,q,X,W(iqibz),Dip,iqibz>1)
   enddo
   call PP_redux_wait()
 endif
 !
 ! DB I/O read to dump the first section variables.
 if (io_RESPONSE) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=X_id)
   if (l_life)      i_err=io_X(X,W(1),X_id)
   if (.not.l_life) i_err=io_X(X,Xw,X_id)
 endif
 !
 ! Self_Energy PARALLEL environment
 !==================================
 call PARALLEL_global_indexes(Xen,Xk,q,"Self_Energy")
 call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_G_b,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
 call PARALLEL_WF_index( )
 !
 ! Redefine i_q_to_start to be CPU-dependent
 !==========================================
 do iqbz=1,q%nbz
   if (PAR_IND_Q_ibz%element_1D(q%sstar(iqbz,1))) then
     i_q_to_start=iqbz
     exit
   endif
 enddo
 !
 ! WFs
 !=====
 call WF_load(WF,X%ng,maxval(qindx_S(:,:,2)),(/QP_n_G_bands(1),n_WF_bands_to_load/),(/1,k%nibz/),title='-Sc')
 !
 if (.not.l_life) then
   ! W
   !===
   QP_n_W_freqs=Xw%n_freqs
   QP_n_W_freqs_redux=Xw%n_freqs
   YAMBO_ALLOC(QP_W,(PAR_nQP,PAR_nQ_bz,PAR_nG_bands,QP_n_W_freqs))
   QP_W=cZERO
 endif
 !
 ! Test the spatial Inversion
 !---------------------------
 call WF_spatial_inversion(en,Xk)
 !
 ! Timing
 !========
 timing_steps=0
 do iqbz=i_q_to_start,q%nbz
   if (.not.PAR_IND_Q_ibz%element_1D( q%sstar(iqbz,1) )) cycle
   timing_steps=timing_steps+1
 enddo
 !
 timing_steps=timing_steps*PAR_nQP*count( PAR_IND_G_b%element_1D(QP_n_G_bands(1):QP_n_G_bands(2)) )
 !
 if (.not.l_life) call live_timing('G'//trim(intc(GW_iter)),timing_steps)
 if (l_life)      call live_timing('Lifetimes',timing_steps)
 !
 if (io_RESPONSE) then
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=X_id)
   io_err=io_X(X,Xw,X_id)
 endif
 !
 ! ALLOCATIONs
 !=============
 call X_alloc('X',(/X%ng,X%ng,QP_n_W_freqs_redux/))
 call elemental_collision_alloc(scattering_main,NG=X%ng,NG_GAMP=(/X%ng,X%ng/),TITLE="GW")
 call elemental_collision_alloc(scattering_prime,NG=X%ng,TITLE="GW")
 !
 ! Lifetimes
 !===========
 !
 ! Clean
 !=======
 deallocate(W)
 call elemental_collision_free(scattering_main)
 call elemental_collision_free(scattering_prime)
 !
end subroutine
