!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
&                        QP_solver_state,QP_table,QP_n_W_freqs,QP_solver,QP_G,&
&                        QP_Vxc,QP_Sx
 use X_m,           ONLY:X_t
 use drivers,       ONLY:Finite_Tel
 use frequency,     ONLY:w_samp
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use functions,     ONLY:bose_f,bose_decay
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:qindx_S,bz_samp,nqbz
 !
 implicit none
 type(bz_samp)::k
 type(levels) ::E
 type(w_samp) ::Xw,Sc_W(QP_n_states)
 integer      ::iqbz
 !
 ! WorkSpace -- GPL_END --
 !
 integer         :: i1,i2,ib,is(2),os(2)
 type(pp_indexes):: px
 real(SP)        :: tsign,E_bare
 complex(SP)     :: QP_W_here(QP_n_W_freqs),QP_Go
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
     call kk(QP_W_here,real(Xw%p(:)),QP_n_W_freqs,dSc,&
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
     call kk(-QP_W_here,real(Xw%p(:)),QP_n_W_freqs,dSc,&
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
 ! The Green`s functions
 !
 if (trim(QP_solver)=="g".and.iqbz==nqbz) then
   !
   do i1=1,QP_n_states
     !
     is=(/QP_table(i1,1),QP_table(i1,3)/) ! (nk) QP
     !
     E_bare=E%E(is(1),is(2),1) 
     if (associated(E%Eo)) E_bare=E%Eo(is(1),is(2),1)
     !
     do i2=1,Sc_W(i1)%n(1) 
       !
       QP_Go=0.5*E%f(is(1),is(2),1)/(Sc_W(i1)%p(i2)-E_bare)
       if (Finite_Tel) then
         QP_Go=QP_Go+0.5*&
&              (2.-E%f(is(1),is(2),1))/(Sc_W(i1)%p(i2)-E_bare)
       else
         QP_Go=QP_Go+0.5*&
&              (2.-E%f(is(1),is(2),1))/(conjg(Sc_W(i1)%p(i2))-E_bare)
       endif
       !
       QP_G(i1,i2)=QP_Go/(1.-QP_Go*(QP_Sc(i1,i2)+QP_Sx(i1)-QP_Vxc(i1)))
       !
     enddo
   enddo
 endif
 !
 ! -- GPL_START --
 !
 end subroutine
