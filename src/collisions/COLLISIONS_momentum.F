!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use pars,          ONLY:SP,schlen,cI,cZERO,rZERO
 use IO_m,          ONLY:io_COLLs
 use com,           ONLY:msg
 use LIVE_t,        ONLY:live_timing
 use electrons,     ONLY:levels
 use parallel_m,    ONLY:PAR_IND_Q_bz,PAR_COM_QP_A2A,PAR_nQ_bz,PAR_COM_Q_INDEX,PAR_COM_G_b_INDEX
 use parallel_int,  ONLY:PP_redux_wait
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc,&
&                        elemental_collision_copy
 use collision_ext, ONLY:P_COLL_element,COLL_bands,P_collisions
 use R_lattice,     ONLY:qindx_S,bz_samp
 use QP_m,          ONLY:QP_QED_ng
 use timing_m,      ONLY:timing
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use interfaces,    ONLY:WF_load
 use COLL_interfaces, ONLY:SCATTERING_GW_kinematics
 !
 implicit none
 type(levels)             ::E       
 type(bz_samp)            ::k,q
 !
 !Work Space
 !
 type(elemental_collision)::I,I_grad_k,I_grad_p
 integer                  ::i_coll,PAR_NC_local,i_q_bz,i_q,i_g,i_g_mem,i_c,i_cp,i_m,i_G0
 complex(SP)              ::P(3)
 real(SP)                 ::ACC,T_matrix(3,3),T_vec(3),photon_E
 character(schlen)        ::ch
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
 call msg('r', '[Momentum] Plane waves ',QP_QED_ng)
 !
 if (WF_buffered_IO) then
   !
   call WF_load(WF_buffer,0,1,COLL_bands,(/1,k%nibz/),space='B',title='-BUFFER')
   !
 endif
 !
 call WF_load(WF,QP_QED_ng,maxval(qindx_S(:,:,2)),COLL_bands,(/1,k%nibz/),space='R',title='-Momentum Collisions')
 !
 call timing('Momentum Collisions',OPR='start')
 !
 ! Allocation again
 !
 call elemental_collision_alloc(I,NG=QP_QED_ng,TITLE="COLL_P")
 call elemental_collision_alloc(I_grad_k,NG=QP_QED_ng,GRADIENT=.TRUE.,TITLE="COLL_P")
 call elemental_collision_alloc(I_grad_p,NG=QP_QED_ng,GRADIENT=.TRUE.,TITLE="COLL_P")
 !
 PAR_NC_local=P_collisions%PAR_N
 if (allocated(P_collisions%IO_status)) then
   PAR_NC_local=0
   do i_coll=1,P_collisions%N
     if (.not.P_collisions%PAR_IND%element_1D(i_coll)) cycle
     if (P_collisions%IO_status(i_coll)/=-1) cycle
     PAR_NC_local=PAR_NC_local+1
   enddo
 endif
 !
 ! Write Database Header 
 !
 call COLLISIONS_linearize_and_IO("Momentum",0,P_collisions,P_COLL_element)
 !
 call live_timing("Collisions",PAR_NC_local*PAR_nQ_bz)
 !
 QP_loop: do i_coll=1,P_collisions%N
   !
   if (.not.P_collisions%PAR_IND%element_1D(i_coll)) cycle
   !
   if (io_COLLs) then
     if (P_collisions%IO_status(i_coll)/=-1) cycle
   endif
   !
   P_COLL_element(1)%v3_r=rZERO
   !
   Q_loop: do i_q_bz=1,q%nbz 
     !
     if (.not.PAR_IND_Q_bz%element_1D(i_q_bz)) cycle
     !
     B_loop: do i_m=COLL_bands(1),COLL_bands(2) 
       !
       call SCATTERING_GW_kinematics(i_q_bz,i_m,I,k,q,P_collisions%state(i_coll,:),G0=i_G0)
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
         call SCATTERING_QED_transverse_matrix(I%is(2),i_q_bz,i_g,i_G0,k,q,T_matrix,T_vec,photon_E)
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
         P_COLL_element(1)%v3_r(i_g_mem,i_m,i_q_bz)=ACC/PAR_COM_G_b_INDEX%n_CPU
         !
       enddo
       !
     enddo B_loop
     !
     call live_timing(steps=1)
     !
   enddo Q_loop
   !
   call PP_redux_wait(P_COLL_element(1)%v3_r,COMM=PAR_COM_QP_A2A%COMM)
   call PP_redux_wait(P_COLL_element(1)%v3_r,COMM=PAR_COM_Q_INDEX%COMM)
   !
   call COLLISIONS_linearize_and_IO("Momentum",i_coll,P_collisions,P_COLL_element)
   !
 enddo QP_loop
 !
 call live_timing()
 !
 ! Clean-up
 !
 call elemental_collision_free(I)
 call elemental_collision_free(I_grad_k) 
 call elemental_collision_free(I_grad_p) 
 !
 call timing('Momentum Collisions',OPR='stop')
 !
end subroutine
