!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team 
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
subroutine QP_W2Sc(iqbz,k,E,Xw,Sc_W)
 !
 ! Performs the complex Hilber transofrmation corresponding to
 !
 ! \int dw' G(w-w')W(w')
 !
 use pars,          ONLY:SP
 use QP_m,          ONLY:QP_t,QP_W,QP_Sc,QP_n_states,QP_G_damp,QP_n_G_bands,&
&                        QP_solver_state,QP_n_W_freqs,QP_solver,QP_table
 use drivers,       ONLY:Finite_Tel
 use frequency,     ONLY:w_samp
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use functions,     ONLY:bose_f,bose_decay
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:qindx_S,bz_samp
 !
 implicit none
 type(bz_samp)::k
 type(levels) ::E
 type(w_samp) ::Xw,Sc_W(QP_n_states)
 integer      ::iqbz
 !
 ! WorkSpace
 !
 integer         :: i1,i2,ib,is(2),os(2)
 type(pp_indexes):: px
 real(SP)        :: tsign
 complex(SP)     :: QP_W_here(QP_n_W_freqs)
 complex(SP), allocatable :: dSc(:)
 !
 tsign=-1.
 if (Finite_Tel) tsign=1.
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/QP_n_G_bands(2)/),low_range=(/QP_n_G_bands(1)/))
 !
 do i1=1,QP_n_states
   !
   if (allocated(QP_solver_state)) then
     if(QP_solver_state(i1)<=0) cycle
   endif
   !
   allocate(dSc(Sc_W(i1)%n(1)))
   !
   dSc=(0._SP,0._SP)
   !
   is=(/QP_table(i1,1),QP_table(i1,3)/)   ! (nk) QP
   os(2)=k%sstar(qindx_S(is(2),iqbz,1),1) ! (nk) intermediate state
   !
   do ib=QP_n_G_bands(1),QP_n_G_bands(2)
     if (.not.px%i1p(ib)) cycle
     !
     os(1)=ib
     !
     ! 1st term: (2-f_os+fbose)
     !
     forall(i2=1:Xw%n(1)) QP_W_here(i2)=QP_W(i1,ib,i2)*&
&                         (2.-E%f(os(1),os(2),1)+bose_f(real(Xw%p(i2))))*&
&                         bose_decay(real(Xw%p(i2)))
     !
     call Kramers_Kronig(QP_W_here,real(Xw%p(:)),QP_n_W_freqs,dSc,&
&            real(Sc_W(i1)%p(:))-cmplx(0.,tsign,SP)*aimag(Sc_W(i1)%p(:)),&
&            Sc_W(i1)%n(1),&
&            E%E(os(1),os(2),1)+tsign*cmplx(0.,QP_G_damp,SP))
     !
     ! 2nd term: (f_os+fbose)
     !
     forall(i2=1:Xw%n(1)) QP_W_here(i2)=QP_W(i1,ib,i2)*&
&                         (E%f(os(1),os(2),1)+bose_f(real(Xw%p(i2))))*&
&                         bose_decay(real(Xw%p(i2)))
     !
     call Kramers_Kronig(-QP_W_here,real(Xw%p(:)),QP_n_W_freqs,dSc,&
&            -real(Sc_W(i1)%p(:))+cmplx(0.,1.,SP)*aimag(Sc_W(i1)%p(:)),Sc_W(i1)%n(1),&
&            -E%E(os(1),os(2),1)-cmplx(0.,QP_G_damp,SP))
     !
   enddo
   !
   call pp_redux_wait(dSc)
   forall(i2=1:Sc_W(i1)%n(1)) QP_Sc(i1,i2)=QP_Sc(i1,i2)+dSc(i2)
   !
   deallocate(dSc)
   !
 enddo
 !
end subroutine
