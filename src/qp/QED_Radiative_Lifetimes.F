!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine QED_Radiative_Lifetimes(E,k,qp,X)
 !
 use units,               ONLY:HA2EV,SPEED_OF_LIGHT
 use pars,                ONLY:SP,rZERO,cI
 use QP_m,                ONLY:QP_time_order_sign,QP_table,QP_t,QP_n_G_bands,QP_Sc
 use IO_m,                ONLY:io_DIP
 use functions,           ONLY:theta_function
 use electrons,           ONLY:levels,spin,spin_occ
 use R_lattice,           ONLY:bz_samp
 use parallel_m,          ONLY:PAR_IND_QP,PAR_IND_G_b
 use X_m,                 ONLY:DIP_P,X_t
 use wave_func,           ONLY:wf_ng
 !
 implicit none
 !
 type(levels)  ::E
 type(bz_samp) ::k
 type(QP_t)    ::qp
 type(X_t)     ::X
 !
 ! Work Space
 !
 integer     :: i_k,i_n,i_spin,i_m,i_qp
 real(SP)    :: E_diff,f_k_m_q,Sc_im,P_square
 complex(SP) :: P(3)
 !
 X%ib  =QP_n_G_bands
 X%ng  =wf_ng
 io_DIP=.FALSE.
 !
 call DIPOLE_driver(E,k,X,X%q0)
 !
 QP_loop: do i_qp=1,qp%n_states
   !
   Sc_im=rZERO
   !
   if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
   !
   i_n    = QP_table(i_qp,1)
   i_k    = QP_table(i_qp,3)
   i_spin = spin(QP_table(i_qp,:))
   !
   B_loop: do i_m=QP_n_G_bands(1),QP_n_G_bands(2)
     !
     if (.not.PAR_IND_G_b%element_1D(i_m)) cycle
     !
     E_diff =E%E(i_n,i_k,i_spin)-E%E(i_m,i_k,i_spin)
     f_k_m_q=E%f(i_m,i_k,i_spin)/spin_occ
     !
     P=DIP_P(:,i_n,i_k,i_k,i_spin)
     !
     P_square=P_square+abs(P(1))**2+abs(P(2))**2+abs(P(3))**2
     !
     Sc_im=Sc_im+P_square/3._SP/SPEED_OF_LIGHT**2*abs(E_diff)*&
&                ( QP_time_order_sign*(1._SP-f_k_m_q)*theta_function( E_diff,0.01/HA2EV,0) +&
&                                            f_k_m_q *theta_function(-E_diff,0.01/HA2EV,0) )
     !
   enddo B_loop
   !
   QP_Sc(i_qp,1)=cI*abs(Sc_im)
   !
 enddo QP_loop
 !
end subroutine QED_Radiative_Lifetimes

