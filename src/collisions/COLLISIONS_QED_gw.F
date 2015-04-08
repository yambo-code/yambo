!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine COLLISIONS_QED_gw(E,k,q)
 !
 ! Hartree-Fock
 !
 use pars,          ONLY:SP,schlen,pi,cI
 use IO_m,          ONLY:io_COLLs
 use com,           ONLY:msg,error
 use LIVE_t,        ONLY:live_timing
 use electrons,     ONLY:levels,spin,n_spinor
 use parallel_m,    ONLY:PAR_IND_QP,PAR_IND_Q,PAR_Q_index,PAR_RL_index,&
&                        PAR_nQP,PAR_nQ,PAR_QP_index,PAR_G_bands_index,PAR_IND_G_b
 use collision,     ONLY:ggwinfo,collision_reset,nGW_QED_collisions,&
&                        COLLISIONS_IO_status,GW_QED_collisions,scatterBamp_alloc
 use QP_m,          ONLY:QP_table,QP_n_states,QP_QED_ng,QP_n_G_bands,QP_nb
 use R_lattice,     ONLY:qindx_S,bz_samp
 use SC,            ONLY:SC_bands
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use interfaces,    ONLY:WF_load
 !
 implicit none
 type(levels)             ::E       
 type(bz_samp)            ::k,q
 !
 !Work Space
 !
 type(ggwinfo)           ::isc,isc_grad
 integer                 ::i_qp,ig,PAR_nQP_,i_q_bz,i_q,i_q_mem,i_g,i_g_mem,i_c,i_qp_mem,i_m,i_m_mem
 real(SP)                ::p_cc(3)
 character(schlen)       ::ch
 !
 if (n_spinor==2) call error('QED collisions not implemented in the spinorial case')
 !
 ! Resets & basis setup
 !
 call collision_reset(isc)
 call collision_reset(isc_grad) 
 !
 call k_ibz2bz(k,'c',.TRUE.)
 !
 ! COLLISIONS 
 !
 call msg('r', '[QED] Plane waves :',QP_QED_ng)
 !
 if (WF_buffered_IO) then
   !
   call WF_load(WF_buffer,0,1,SC_bands,(/1,k%nibz/),space='B',title='-BUFFER')
   !
 endif
 !
 call WF_load(WF,QP_QED_ng,maxval(qindx_S(:,:,2)),SC_bands,(/1,k%nibz/),space='R',title='-QED Collisions')
 !
#if defined _TIMING
 call timing('QED GW Collisions',OPR='start')
#endif
 !
 ! Allocation again
 !
 call scatterBamp_alloc(isc)
 call scatterBamp_alloc(isc_grad)
 allocate(isc%rhotw(QP_QED_ng),isc_grad%rhotw(3*QP_QED_ng))
 isc%ngrho =QP_QED_ng
 isc_grad%ngrho=QP_QED_ng
 !
 PAR_nQP_=PAR_nQP
 if (allocated(COLLISIONS_IO_status)) then
   PAR_nQP_=count(COLLISIONS_IO_status==-1)
 endif
 !
 call live_timing("QED GW collisions",PAR_nQP_*PAR_nQ)
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
   Q_loop: do i_q_bz=1,q%nbz 
     !
     if (.not.PAR_IND_Q%element_1D(i_q_bz)) cycle
     !
     i_q_mem=PAR_Q_index(i_q_bz)
     !
     p_cc=k%ptbz(qindx_S(QP_table(i_qp,3),i_q_bz,1),:)
     !
     !
     ! n',k-q   n,k
     ! --<--.-<--
     !      | 
     !      |
     !     \|/ q,G,\alpha
     !      |
     !      
     ! n   =QP_table(i_qp,1)
     ! n'  =QP_table(i_qp,2)
     ! k   =QP_table(i_qp,3)
     !
     isc%is=(/QP_table(i_qp,1),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     isc%os=(/QP_table(i_qp,2),k%sstar(qindx_S(isc%is(2),i_q_bz,1),:),spin(QP_table(i_qp,:))/)
     isc%qs=(/qindx_S(QP_table(i_qp,3),i_q_bz,2),q%sstar(i_q_bz,1),q%sstar(i_q_bz,2)/)
     !
     isc_grad%is=isc%is
     isc_grad%os=isc%os
     isc_grad%qs=isc%qs
     !
     call scatterBamp(isc)
     call scatterBamp_using_the_gradient(isc_grad)
     !
     do i_g=1,QP_QED_ng
       !
       i_g_mem=PAR_RL_index(i_g)
       !
       do i_c=1,3
         !
         if (io_COLLs) then
           GW_QED_collisions(1)%v3_c(i_q_bz,i_g,i_c)=-cI*(p_cc(i_c)*isc%rhotw(i_g)+isc_grad%rhotw(QP_QED_ng*(i_c-1)+ig))
         else
           GW_QED_collisions(i_qp_mem)%v3_c(i_q_mem,i_g_mem,i_c)=-cI*(p_cc(i_c)*isc%rhotw(i_g)+isc_grad%rhotw(QP_QED_ng*(i_c-1)+ig))
         endif
         !
       enddo
       !
     enddo
     !
     call live_timing(steps=1)
     !
   enddo Q_loop
   !
   !if (io_COLLs) call COLLISIONS_linearize_and_IO("GW_QED",i_qp,GW_QED_collisions,nGW_QED_collisions)
   !
 enddo QP_loop
 !
 call live_timing()
 !
 deallocate(isc%rhotw,isc_grad%rhotw)
 !
 call collision_reset(isc)
 call collision_reset(isc_grad) 
 !
#if defined _TIMING
 call timing('QED GW Collisions',OPR='stop')
#endif
 !
end subroutine
