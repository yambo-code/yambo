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
 use drivers,       ONLY:l_ph_corr,Finite_Tel
 use pars,          ONLY:SP
 use QP_m,          ONLY:QP_G,QP_t,QP_Sc,QP_Sx,QP_Vxc,QP_table,Gf_energy_range,&
&                        QP_n_states,use_GreenF_to_eval_QP,QP_Sc_steps,QP_G_dr
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
 integer          ::iqp,ib,ik,i_spin,iw
 complex(SP)      ::QP_Go
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
   call GF_energy_range(iqp,W,E%E)
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
       if (allocated(QP_Sx)) then
         qp%S_total(iqp,:)=QP_Sc(iqp,:)+QP_Sx(iqp)-QP_Vxc(iqp)
       else
         qp%S_total(iqp,:)=QP_Sc(iqp,:)
       endif
     endif
     !
     QP_G(iqp,iw)=QP_Go/(1.-QP_Go*qp%S_total(iqp,iw))
     !
   enddo
 enddo
 !
 if (.not.use_GreenF_to_eval_QP) return
 !
 ! Extraction of the QP properties directly from the Green's function
 !
 !
end subroutine

