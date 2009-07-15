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
subroutine QP_Green_Function(qp,E,QPdb_read_err)
 !
 use drivers,       ONLY:l_ph_corr,Finite_Tel,l_el_corr
 use pars,          ONLY:SP
 use units,         ONLY:HARTREE
 use QP_m,          ONLY:QP_G,QP_t,QP_Sc,QP_Sx,QP_Vxc,QP_table,&
&                        QP_n_states,use_GreenF_to_eval_QP,QP_Sc_steps,QP_G_dr,&
&                        QP_dSc_delta,Force_Newton_QP
 use electrons,     ONLY:spin,levels,spin_occ
 use frequency,     ONLY:w_samp,W_reset
 implicit none
 !
 type(levels)     ::E
 type(QP_t)       ::qp
 integer          ::QPdb_read_err
 !
 ! Work Space
 !
 type(w_samp)     ::W
 integer          ::iqp,ib,ik,i_spin,iw,n_poles,ip,ip_larger_R,nw_dSc
 integer,parameter::n_max_poles=100
 complex(SP)      ::QP_Go,Eqp(n_max_poles),R(n_max_poles),beta(n_max_poles),G_m1(QP_Sc_steps)
 real(SP)         ::Eo
 !
 ! The Green's function
 !
 call W_reset(W)
 W%n =QP_Sc_steps
 W%dr=QP_G_dr
 !
 do iqp=1,QP_n_states
   !
   ! Frequency range
   !
   call FREQUENCIES_Green_Function(iqp,W,E%E)
   QP_Sc_steps=W%n(1)
   !
   ! Bare energy
   !
   ib    =QP_table(iqp,1)
   ik    =QP_table(iqp,3)
   i_spin=spin(QP_table(iqp,:))
   !
   Eo=E%E(ib,ik,i_spin) 
   if (associated(E%Eo)) Eo=E%Eo(ib,ik,i_spin)
   !
   do iw=1,W%n(1) 
     !
     !  Davide: 15/04/2009 moved conjg from the second term of the Green to
     !          the first according to the QP expression for the T-ordered Green
     !          G=\frac{1}{\omega-\epsilon_i+i\eta sign(\epsilon-\mu)}         
     !
     if (Finite_Tel.or.l_ph_corr) then
       QP_Go=E%f(ib,ik,i_spin)/(W%p(iw)-Eo)/spin_occ
     else
       QP_Go=E%f(ib,ik,i_spin)/(conjg(W%p(iw))-Eo)/spin_occ
     endif
     !
     QP_Go=QP_Go+(spin_occ-E%f(ib,ik,i_spin))/(W%p(iw)-Eo)/spin_occ                    
     !
     if (QPdb_read_err<0) then
       if ( l_el_corr.and.allocated(QP_Sx) ) then
         qp%S_total(iqp,:)=QP_Sc(iqp,:)+QP_Sx(iqp)-QP_Vxc(iqp)
       else
         qp%S_total(iqp,:)=QP_Sc(iqp,:)
       endif
     endif
     !
     QP_G(iqp,iw)=QP_Go/(1.-QP_Go*qp%S_total(iqp,iw))
     !
   enddo
   !
   if (.not.use_GreenF_to_eval_QP) cycle
   !
   nw_dSc=int(QP_dSc_delta/real(W%p(2)-W%p(1))) 
   !
   ! Extraction of the QP properties directly from the Green's function
   !
   G_m1(:)=real(W%p(:))-Eo-qp%S_total(iqp,:)
   if (Force_Newton_QP) then
     n_poles=1
     do iw=1,W%n(1)-1
       if (real(W%p(iw))<Eo.and.real(W%p(iw+1))>Eo) then
         beta(1)=(qp%S_total(iqp,min(iw+nw_Dsc,W%n(1)))-qp%S_total(iqp,iw))/&
&                real(W%p(min(iw+nw_Dsc,W%n(1)))-W%p(iw))
         Eqp(1) =cmplx(real(W%p(iw)),aimag(qp%S_total(iqp,iw)))+1./(1.-beta(1))*qp%S_total(iqp,iw)
       endif
     enddo
   else
     n_poles=0
     do iw=1,W%n(1)-1
       if (real(G_m1(iw))*real(G_m1(iw+1))<0.) then
         n_poles=n_poles+1
         Eqp(n_poles)=cmplx(real(W%p(iw)),aimag(qp%S_total(iqp,iw)))
         R(n_poles)   =QP_G(iqp,iw)
         beta(n_poles)=(qp%S_total(iqp,min(iw+nw_Dsc,W%n(1)))-qp%S_total(iqp,iw))/&
&                      (real(W%p(min(iw+nw_Dsc,W%n(1)))-W%p(iw)))
       endif
       if (n_poles==n_max_poles) exit
     enddo
   endif
   ip_larger_R=1
   if (n_poles>1) then
     do ip=2,n_poles
       if (-real(R(ip))>-real(R(ip_larger_R))) ip_larger_R=ip
     enddo
   endif
   qp%E(iqp)=Eqp(ip_larger_R)
   qp%Z(iqp)=1./(1.-beta(ip_larger_R))
 enddo
 !
end subroutine

