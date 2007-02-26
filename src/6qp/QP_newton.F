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
 subroutine QP_newton(X,Xen,Xk,en,k,q,qp,Xw)
 !
 use pars,          ONLY:SP,HARTREE
 use memory_m,      ONLY:mem_est
 use drivers,       ONLY:l_ppa,l_el_corr,l_ph_corr
 use com,           ONLY:msg
 use X_m,           ONLY:X_t
 use parser_m,      ONLY:parser
 use QP_m,          ONLY:QP_t,QP_dSc_steps,QP_Sx,QP_Vxc,QP_Sc,&
&                        QP_n_states,QP_dSc,QP_dSc_delta,QP_n_G_bands,&
&                        GWo_SC_done,GWo_iterations,QP_table
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use QP_CTL,        ONLY:QP_apply
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
 real(SP), parameter :: SC_tresh=0.01/HARTREE
 !
 ! Dyson equation: Newton solver 
 !
 call msg('r', '[Newton] Sc step   [ev]:',QP_dSc_delta*HARTREE)
 call msg('r', '[Newton] Sc steps      :',QP_dSc_steps)
 call msg('rn','[Newton] SC iterations :',GWo_iterations)
 !
 iter=0
 SC_corr_prev=1.
 Eqp_prev(:)=qp%E_bare(:)
 !
 do while((iter<=GWo_iterations.or.GWo_iterations<0).and..not.GWo_SC_done) 
   !
   if (l_el_corr) then
     if (.not.l_ppa) call QP_raxis(X,Xen,Xk,en,k,q,qp,Xw,iter)
     if (l_ppa)      call   QP_ppa(X,Xk,en,k,q,qp,Xw,iter)
#if defined PJ_SC
     return
#endif
   else
     QP_Sc=cmplx(0.,0.,SP)
   endif
#if defined PJ_PH
   if (l_ph_corr)    call QP_elph(en,k,q,qp)
#endif
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
     qp%E(i1)=Eqp(1)
     qp%Z(i1)=Z(1)
     !
   enddo
   !
   SC_corr_now= maxval(real(qp%E(:)-Eqp_prev(:)))
   GWo_SC_done=abs( SC_corr_prev-SC_corr_now )<SC_tresh.or.iter==GWo_iterations
   Eqp_prev(:)=qp%E(:)
   !
   if (iter>0) call msg('rs','[SC] Max corrections at prev/current cycle [ev]',&
&                      (/SC_corr_prev,SC_corr_now/)*HARTREE)
   SC_corr_prev=SC_corr_now
   !
   if (.not.GWo_SC_done.and.GWo_iterations/=0) then
     !
     ! Now I need to transfer the calculated QP corrections to the "en" type
     ! and to the qp%Eo array
     !
     if (associated(en%qp_done)) en%qp_done=0
     call QP_apply(QP_n_G_bands,en,k,3,msg_fmt='rs',qp_impose=qp)
     !
   endif
   !
   ! Increase the counter
   !
   iter=iter+1
   !
 enddo
 !
 ! Update GWo_iterations
 !
 if (GWo_iterations<0) GWo_iterations=iter
 !
 end subroutine
