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
subroutine QP_W2Sc(iqbz,k,E,Xw,Sc_W)
 !
 ! Performs the complex Hilbert transform corresponding to
 !
 ! \int dw' G(w-w')W(w')
 !
 use pars,          ONLY:SP,cZERO,cI
 use QP_m,          ONLY:QP_t,QP_W,QP_Sc,QP_n_states,QP_G_damp,QP_n_G_bands,&
&                        QP_solver_state,QP_n_W_freqs,QP_table
 use drivers,       ONLY:Finite_Tel
 use frequency,     ONLY:w_samp
 use functions,     ONLY:bose_f,bose_decay
 use electrons,     ONLY:levels,spin_occ,spin
 use R_lattice,     ONLY:qindx_S,bz_samp
 use parallel_m,    ONLY:PAR_IND_QP,PAR_IND_G_b,PAR_QP_index,PAR_Q_bz_index,PAR_G_bands_index
 !
 implicit none
 type(bz_samp)::k
 type(levels) ::E
 type(w_samp) ::Xw,Sc_W(QP_n_states)
 integer      ::iqbz
 !
 ! WorkSpace
 !
 integer         :: i_qp,i_w,ib,is(2),os(3),i_qp_mem,i_q_mem,i_b_mem
 real(SP)        :: tsign
 complex(SP)     :: QP_W_here(QP_n_W_freqs)
 complex(SP), allocatable :: dSc(:)
 !
 tsign=-1._SP
 if (Finite_Tel) tsign=1._SP
 !
 i_q_mem=PAR_Q_bz_index(iqbz)
 !
 do i_qp=1,QP_n_states
   !
   if (allocated(QP_solver_state)) then
     if(QP_solver_state(i_qp)<=0) cycle
   endif
   !
   is=(/QP_table(i_qp,1),QP_table(i_qp,3)/)   ! (nk) QP
   os(2)=k%sstar(qindx_S(is(2),iqbz,1),1)     ! (nk) intermediate state
   os(3)=spin(QP_table(i_qp,:))
   !
   if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
   !
   i_qp_mem=PAR_QP_index(i_qp)
   !
   allocate(dSc(Sc_W(i_qp)%n_freqs))
   !
   dSc=cZERO
   !
   do ib=QP_n_G_bands(1),QP_n_G_bands(2)
     !
     if (.not.PAR_IND_G_b%element_1D(ib)) cycle
     !
     i_b_mem=PAR_G_bands_index(ib)
     !
     os(1)=ib
     !
     ! 1st term: (spin_occ-f_os+fbose)
     !
     forall(i_w=1:Xw%n_freqs) QP_W_here(i_w)=QP_W(i_qp_mem,i_q_mem,i_b_mem,i_w)*    &
&                        (spin_occ-E%f(os(1),os(2),os(3))+bose_f(real(Xw%p(i_w))))* &
&                         bose_decay(real(Xw%p(i_w)))
     !
     call Kramers_Kronig(QP_W_here,real(Xw%p(:)),QP_n_W_freqs,dSc,          &
&            real(Sc_W(i_qp)%p(:))-tsign*cI*aimag(Sc_W(i_qp)%p(:)),         &
&            Sc_W(i_qp)%n_freqs, E%E(os(1),os(2),os(3))+tsign*cI*QP_G_damp)
     !
     ! 2nd term: (f_os+fbose)
     !
     forall(i_w=1:Xw%n_freqs) QP_W_here(i_w)=QP_W(i_qp_mem,i_q_mem,i_b_mem,i_w)* &
&                         (E%f(os(1),os(2),os(3))+bose_f(real(Xw%p(i_w))))*      &
&                         bose_decay(real(Xw%p(i_w)))
     !
     call Kramers_Kronig(-QP_W_here,real(Xw%p(:)),QP_n_W_freqs,dSc,         &
&            -real(Sc_W(i_qp)%p(:))+cI*aimag(Sc_W(i_qp)%p(:)),              &
&             Sc_W(i_qp)%n_freqs,-E%E(os(1),os(2),os(3))-cI*QP_G_damp)
     !
   enddo
   !
   forall(i_w=1:Sc_W(i_qp)%n_freqs) QP_Sc(i_qp,i_w)=QP_Sc(i_qp,i_w)+dSc(i_w)
   !
   deallocate(dSc)
   !
 enddo
 !
end subroutine
