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
subroutine QP_secant_driver(X,Xen,Xk,en,k,q,qp,Xw,Dip)
 !
 !Procedure for non-perturbative solution of Dyson equation.
 !Here it`s solved real part of Dyson Eq. with a fixed
 !shift from real axis
 !
 ! E1 = Eo + Sx - Vxc + Re[Sc(E1)] (1)
 ! E1 is REAL
 !
 ! Re(E) = E1 + Im[Sc(E1)] + Re[Sc(E)]
 ! Im(E) = Im[Sc(E)]
 !
 !After convergence is reached Sc(w) is analtically extendend
 !outside real axe using first order Taylor expansion around E1.
 !In this way it`s found the solution of Complex Equation
 !
 ! E = E1 + 1/(1-dSc(E1)) Im[Sc(E1)]
 !
 !The procedure for finding root of (1) is divided into two steps:
 !
 ! 1. BRACKETING: starting for an initial guess of the
 !    QP-energy [Eqp] (given by E_lda) the routine gives a range
 !    of energy where the solution is expected to be
 !
 ! 2. SECANT SOLUTION: see "Numerical Recipes (Fortran version)", Pag. 248
 !
 use pars,          ONLY:SP
 use units,         ONLY:HA2EV
 use LIVE_t,        ONLY:live_timing
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use frequency,     ONLY:w_samp,W_reset
 use DIPOLES,       ONLY:DIPOLE_t
 use X_m,           ONLY:X_t
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Q_ibz
 use QP_m,          ONLY:QP_t,QP_Vxc,QP_Vnl_xc,QP_Sc,QP_n_W_freqs,QP_W,QP_n_states,&
&                        QP_solver_state,QP_dSc_steps,QP_dSc,&
&                        QP_W_dr,QP_W_er,QP_dSc_delta
 use IO_m,          ONLY:io_control,NONE,RD,RD_CL,OP_RD,OP_RD_CL,io_SCREEN
 use stderr,        ONLY:intc
 !
#include<memory.h>
 type(levels)::en,Xen
 type(bz_samp)::Xk,k,q
 type(X_t):: X
 type(QP_t):: qp
 type(DIPOLE_t) :: Dip
 type(w_samp)::Xw
 !
 ! Work Space 
 !
 integer      :: i1,i2,ID,io_err,iqbz,nstep,nconv
 type(w_samp) :: Sc_W(qp%n_states)
 real(SP)     :: Eqp_raxis(qp%n_states,2),dQP_raxis(qp%n_states,2)
 complex(SP)  :: Z(QP_dSc_steps-1),Eqp(QP_dSc_steps-1)
 real(SP),parameter :: root_acc=0.001/HA2EV
 !
 integer, external  :: io_QP_and_GF,QP_secant,DYSON_bracket
 !
 !I need to store on disk the <W> m. elemnts
 !
 call QP_real_axis(X,Xen,Xk,en,k,q,qp,Xw,Dip,0)
 !
 !I prepare the Sc energy type
 !
 do i1=1,qp%n_states
   call W_reset(Sc_W(i1))
   Sc_W(i1)%n_freqs=1
   YAMBO_ALLOC(Sc_W(i1)%p,(1))
 enddo
 !
 !SOLVER STATE
 !=============
 !
 ! 1 : updating Eqp_raxis(?,1)
 ! 2 : updating Eqp_raxis(?,2)
 !
 ! <0: Eqp_raxis(?,iabs(SOLVER_STATE))  converged
 !
 YAMBO_ALLOC(QP_solver_state,(qp%n_states))
 !
 !Brackets
 !
 forall(i1=1:qp%n_states) Eqp_raxis(i1,1)=qp%E_bare(i1)
 forall(i1=1:qp%n_states) Eqp_raxis(i1,2)=Eqp_raxis(i1,1)+1./HA2EV
 !
 QP_solver_state=1
 call W2Sc_local_call()
 !
 QP_solver_state=2
 call W2Sc_local_call()
 !
 call live_timing('[SECANT] Brackets',QP_n_states)
 !
 do while(any(QP_solver_state/=0))
   do i1=1,qp%n_states
     i2=QP_solver_state(i1)
     QP_solver_state(i1)=DYSON_bracket(Eqp_raxis(i1,1),dQP_raxis(i1,1),&
&                                Eqp_raxis(i1,2),dQP_raxis(i1,2))
     if (i2/=0.and.QP_solver_state(i1)==0) call live_timing(steps=1)
   enddo
   call W2Sc_local_call()
 enddo
 call live_timing()
 call PP_redux_wait()
 !
 ! Root
 !
 nstep=0
 nconv=0
 QP_solver_state=1
 call live_timing('[SECANT]  Root(s)',QP_n_states)
 do while(any(QP_solver_state>=0))
   nstep=nstep+1
   do i1=1,qp%n_states
     i2=QP_solver_state(i1)
     QP_solver_state(i1)=QP_secant(Eqp_raxis(i1,1),dQP_raxis(i1,1),&
&                               Eqp_raxis(i1,2),dQP_raxis(i1,2),root_acc,nstep)
     if (i2>0.and.QP_solver_state(i1)<0) then
       nconv=nconv+1
       call live_timing(steps=1)
     endif
   enddo
   if (nstep==500) then
     call warning(' Step 500. '//trim(intc(QP_n_states-nconv))//' state(s) non converged. Max error= '//&
&                 trim(intc(int(maxval(abs(dQP_raxis))*10E+4)))//"E-4 eV. At step 1500 I'll exit Root(s) loop.")
   else if (nstep==1500) then
     call warning(' Step 1500. '//trim(intc(QP_n_states-nconv))//&
&                 ' state(s) non converged. Secan loop interrupted.')
     do i1=1,qp%n_states
       if(QP_solver_state(i1)<0) cycle
       call warning(' State '//trim(intc(i1))//' not converged. Accuracy obtained'//&
&                    trim(intc(int(maxval(abs(dQP_raxis(i1,:)))*10E+4)))//'E-4 eV.')
       QP_solver_state(i1)=-1
     enddo
     call live_timing(steps=(QP_n_states-nconv))
   endif
   call W2Sc_local_call()
 enddo
 call live_timing()
 call PP_redux_wait()
 !
 ! Analytic continuation 
 !
 call live_timing('[SECANT] Analytic continuation',q%nbz)
 do i1=1,qp%n_states
   Eqp_raxis(i1,1)=Eqp_raxis(i1,iabs(QP_solver_state(i1)))
   call W_reset(Sc_W(i1))
   Sc_W(i1)%n_freqs=QP_dSc_steps
   YAMBO_ALLOC(Sc_W(i1)%p,(QP_dSc_steps))
   forall (i2=1:QP_dSc_steps) Sc_W(i1)%p(i2)=Eqp_raxis(i1,1)+(i2-1)*QP_dSc_delta
 enddo
 YAMBO_FREE(QP_solver_state)
 !
 call W2Sc_local_call()
 !
 do i1=1,QP_n_states
   do i2=1,QP_dSc_steps-1
     QP_dSc(i1,i2)=(QP_Sc(i1,i2+1)-QP_Sc(i1,i2))/QP_dSc_delta
     Z(i2)=1./(1.-QP_dSc(i1,i2))
     Eqp(i2)=Eqp_raxis(i1,1)+cmplx(0.,real(Z(i2))*aimag(QP_Sc(i1,1)),SP)
   enddo
   qp%E(i1)=Eqp(1)
   qp%Z(i1)=Z(1)
 enddo
 call live_timing()
 !
 !CLEAN
 !
 do i1=1,QP_n_states
   call W_reset(Sc_W(i1))
 enddo
 YAMBO_FREE(QP_W)
 YAMBO_FREE(QP_W_dr)
 YAMBO_FREE(QP_W_er)
 call PP_redux_wait()
 !
 contains 
   !
   subroutine W2Sc_local_call()
     !
     ! Here I call the QP_W2Sc to routine to calculate the Sc SE
     ! corresponding to Eqp_raxis(i1,QP_solver_state(i1)) energies
     !
     ! The driver QP_solver_state tells if I am updating the 1st or the 2nd
     ! approx to the final QP energy.
     !
     if (allocated(QP_solver_state)) then
       do i1=1,QP_n_states
         if (QP_solver_state(i1)>0) Sc_W(i1)%p(1)=Eqp_raxis(i1,QP_solver_state(i1))
       enddo
     endif
     QP_Sc=(0._SP,0._SP)
     do iqbz=1,q%nbz
       !
       if (io_SCREEN) then
         !
         if (iqbz==1.and.iqbz<q%nbz)  call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1,2,3/),ID=ID)
         if (iqbz> 1.and.iqbz<q%nbz)  call io_control(ACTION=RD,COM=NONE,SEC=(/2+iqbz/),ID=ID)
         if (iqbz> 1.and.iqbz==q%nbz) call io_control(ACTION=RD_CL,COM=NONE,SEC=(/2+iqbz/),ID=ID)
         if (iqbz==1.and.iqbz==q%nbz) call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3/),ID=ID)
         io_err=io_QP_and_GF('W',qp,ID)
         !
       endif
       !
       if (.not.PAR_IND_Q_ibz%element_1D( q%sstar(iqbz,1) )) cycle
       !
       Xw%er=QP_W_er(iqbz,:)
       Xw%dr=QP_W_dr(iqbz,:)
       Xw%n_freqs=QP_n_W_freqs
       call FREQUENCIES_setup(Xw)
       !
       call QP_W2Sc(iqbz,k,en,Xw,Sc_W)
       !
       if (.not.allocated(QP_solver_state)) call live_timing(steps=1)
       !
     enddo
     !
     call PP_redux_wait(QP_Sc)
     !
     if (allocated(QP_solver_state)) then
       do i1=1,QP_n_states
         !
         ! If the Qp energy is not converged, yet, I store in 
         ! dQP_raxis = Eo+Re[Sc_tot]-Eqp_raxis
         !
         ! The convergence criteria is dQP_raxis= 0.
         !
         if (QP_solver_state(i1)>0) dQP_raxis(i1,QP_solver_state(i1))=&
&            ( qp%E_bare(i1)+real(QP_Sc(i1,1)+QP_Vnl_xc(i1)-QP_Vxc(i1)) )-&
&            Eqp_raxis(i1,QP_solver_state(i1))
       enddo
     endif
     !
   end subroutine
   !
end subroutine
