!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine COLLISIONS_momentum(E,k,q)
 !
 ! Momentum matrix elements
 !
 use pars,          ONLY:SP,schlen,pi,cI,cZERO,rZERO
 use IO_m,          ONLY:io_COLLs
 use com,           ONLY:msg,error
 use LIVE_t,        ONLY:live_timing
 use electrons,     ONLY:levels,spin,n_spinor
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_QP,PAR_IND_Q,PAR_Q_index,PAR_QP_index,&
&                        PAR_COM_QP_A2A,PAR_nQP,PAR_nQ,PAR_COM_G_b_A2A,PAR_COM_Q_index
 use collision,     ONLY:elemental_collision,elemental_collision_free,nP_collisions,&
&                        COLLISIONS_IO_status,P_collisions,elemental_collision_alloc,&
&                        elemental_collision_copy,nP_collisions,&
&                        COLLISIONS_parallel_SC_structure
 use QP_m,          ONLY:QP_n_states,QP_QED_ng,QP_n_states
 use R_lattice,     ONLY:qindx_S,bz_samp
 use SC,            ONLY:SC_bands
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use interfaces,    ONLY:WF_load,SCATTERING_GW_kinematics
 !
 implicit none
 type(levels)             ::E       
 type(bz_samp)            ::k,q
 !
 !Work Space
 !
 type(elemental_collision)::I,I_grad_k,I_grad_p
 integer                  ::i_qp,PAR_nQP_,i_q_bz,i_q,i_q_mem,i_g,i_g_mem,i_c,i_cp,i_qp_mem,i_m,i_G0
 complex(SP)              ::P(3)
 real(SP)                 ::ACC,T_matrix(3,3),T_vec(3),photon_E
 character(schlen)        ::ch
 !
 if (n_spinor==2) call error('Momentum collisions not implemented in the spinorial case')
 !
 ! Resets & basis setup
 !
 call elemental_collision_free(I)
 call elemental_collision_free(I_grad_k) 
 call elemental_collision_free(I_grad_p) 
 !
 call k_ibz2bz(q,'c',.TRUE.)
 !
 ! COLLISIONS 
 !
 call msg('r', '[Momentum] Plane waves :',QP_QED_ng)
 !
 if (WF_buffered_IO) then
   !
   call WF_load(WF_buffer,0,1,SC_bands,(/1,k%nibz/),space='B',title='-BUFFER')
   !
 endif
 !
 call WF_load(WF,QP_QED_ng,maxval(qindx_S(:,:,2)),SC_bands,(/1,k%nibz/),space='R',title='-Momentum Collisions')
 !
#if defined _TIMING
 call timing('Momentum Collisions',OPR='start')
#endif
 !
 ! Allocation again
 !
 call elemental_collision_alloc(I,NG=QP_QED_ng,TITLE="COLL_P")
 call elemental_collision_alloc(I_grad_k,NG=QP_QED_ng,GRADIENT=.TRUE.,TITLE="COLL_P")
 call elemental_collision_alloc(I_grad_p,NG=QP_QED_ng,GRADIENT=.TRUE.,TITLE="COLL_P")
 !
 PAR_nQP_=PAR_nQP
 if (allocated(COLLISIONS_IO_status)) then
   PAR_nQP_=count(COLLISIONS_IO_status==-1)
 endif
 !
 call live_timing("Momentum collisions",PAR_nQP_*PAR_nQ)
 !
 QP_loop: do i_qp=1,QP_n_states
   !
   if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
   !
   if (io_COLLs) then
     if (COLLISIONS_IO_status(i_qp)/=-1) cycle
   endif
   !
   i_qp_mem=PAR_QP_index(i_qp)
   !
   P_collisions(1)%v3_r=rZERO
   !
   Q_loop: do i_q_bz=1,q%nbz 
     !
     if (.not.PAR_IND_Q%element_1D(i_q_bz)) cycle
     !
     i_q_mem=PAR_Q_index(i_q_bz)
     !
     B_loop: do i_m=SC_bands(1),SC_bands(2) 
       !
       call SCATTERING_GW_kinematics(i_qp,i_q_bz,i_m,I,k,q,G0=i_G0)
       !
       call elemental_collision_copy(I,I_grad_k)
       call elemental_collision_copy(I,I_grad_p)
       !
       call scatter_Bamp(I)
       call scatter_Bamp_using_the_gradient(I_grad_k,"i")
       call scatter_Bamp_using_the_gradient(I_grad_p,"o")
       !
       do i_g=1,QP_QED_ng
         !
         i_g_mem=i_g
         !
         call SCATTERING_QED_transverse_matrix(i_q_bz,i_g,i_G0,q,T_matrix,T_vec,photon_E)
         !
         do i_c=1,3
           P(i_c)=I_grad_k%rhotw(QP_QED_ng*(i_c-1)+i_g)-I_grad_p%rhotw(QP_QED_ng*(i_c-1)+i_g)-cI*T_vec(i_c)*I%rhotw(i_g)
         enddo
         !
         ACC=cZERO
         !
         do i_cp=1,3
           do i_c=1,3
             ACC=ACC+conjg(P(i_c))*T_matrix(i_c,i_cp)*P(i_cp)
           enddo
         enddo
         !
         P_collisions(1)%v3_r(i_g_mem,i_m,i_q_bz)=ACC
         !
       enddo
       !
     enddo B_loop
     !
     call live_timing(steps=1)
     !
   enddo Q_loop
   !
   if (COLLISIONS_parallel_SC_structure) then
     call PP_redux_wait(P_collisions(1)%v3_r,COMM=PAR_COM_QP_A2A%COMM)
     call PP_redux_wait(P_collisions(1)%v3_r,COMM=PAR_COM_Q_index%COMM)
   else
     call PP_redux_wait(P_collisions(1)%v3_r,COMM=PAR_COM_G_b_A2A%COMM)
   endif
   !
   call COLLISIONS_linearize_and_IO("Momentum",i_qp,P_collisions,nP_collisions)
   !
 enddo QP_loop
 !
 call live_timing()
 !
 call elemental_collision_free(I)
 call elemental_collision_free(I_grad_k) 
 call elemental_collision_free(I_grad_p) 
 !
#if defined _TIMING
 call timing('Momentum Collisions',OPR='stop')
#endif
 !
end subroutine
