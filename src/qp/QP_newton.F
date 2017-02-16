!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine QP_newton(X,Xen,Xk,en,k,q,qp,Xw)
 !
 use pars,          ONLY:SP,HARTREE
 use drivers,       ONLY:l_ppa,l_el_corr,l_ph_corr,l_chosex
 use com,           ONLY:msg
 use X_m,           ONLY:X_t
 use SC,            ONLY:SC_E_threshold
 use QP_m,          ONLY:QP_t,QP_dSc_steps,QP_Sx,QP_Vxc,QP_Sc,&
&                        QP_n_states,QP_dSc,QP_dSc_delta,QP_n_G_bands,&
&                        GWo_SC_done,GWo_iterations
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_apply
 implicit none
 type(levels) ::en,Xen
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 type(w_samp) ::Xw
 !
 ! Work Space
 !
 integer     :: i1,i2,iter
 real(SP)    :: SC_corr_prev,SC_corr_now
 complex(SP) :: Z(QP_dSc_steps-1),Eqp(QP_dSc_steps-1),Eqp_prev(QP_n_states)
 !
 ! Dyson equation: Newton solver 
 !
 ! ... no energy dependence in GW CHOSEX
 !
   !
   call msg('r', '[Newton] Sc step   [ev]:',QP_dSc_delta*HARTREE)
   call msg('r', '[Newton] Sc steps      :',QP_dSc_steps)
   !
 !
 call msg('rn','[Newton] SC iterations :',GWo_iterations)
 !
 iter=0
 SC_corr_prev=1.
 Eqp_prev(:)=qp%E_bare(:)
 !
   !
   if (l_el_corr) then
     if (l_ppa.or.l_chosex) then
       call QP_ppa_chosex(X,Xk,en,k,q,qp,Xw,iter)
     else
       !
       !
     endif
   else
     QP_Sc=cmplx(0.,0.,SP)
   endif
   !
   !
   do i1=1,QP_n_states
     !
     do i2=1,QP_dSc_steps-1
       QP_dSc(i1,i2)=(QP_Sc(i1,i2+1)-QP_Sc(i1,i2))/QP_dSc_delta
       Z(i2)=1./(1.-QP_dSc(i1,i2))
       !
       if (.not.l_el_corr) Z(i2)=1.
       !
       Eqp(i2)=qp%E_bare(i1)+Z(i2)*QP_Sc(i1,1)
       if (l_el_corr) Eqp(i2)=Eqp(i2)+Z(i2)*(QP_Sx(i1)-QP_Vxc(i1))
       !
     enddo
     !
     ! CHOSEX: no energy dependence
     !
     if (l_chosex) then
       Eqp(1)=qp%E_bare(i1)+(QP_Sc(i1,1)+QP_Sx(i1)-QP_Vxc(i1))
       Z(1)=1.
     endif
     !
     qp%E(i1)=Eqp(1)
     qp%Z(i1)=Z(1)
     !
   enddo
   !
   SC_corr_now= maxval(real(qp%E(:)-Eqp_prev(:)))
   GWo_SC_done=abs( SC_corr_prev-SC_corr_now )<SC_E_threshold.or.iter==GWo_iterations
   Eqp_prev(:)=qp%E(:)
   !
 !
 ! Update GWo_iterations
 !
 if (GWo_iterations<0) GWo_iterations=iter
 !
end subroutine
