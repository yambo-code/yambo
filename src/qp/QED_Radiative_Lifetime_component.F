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
subroutine QED_Radiative_Lifetime_component(E,RADIATIVE_pre_factor,i_qp,i_m)
 !
 use units,               ONLY:HA2EV
 use pars,                ONLY:SP
 use QP_m,                ONLY:QP_time_order_sign,QP_table
 use functions,           ONLY:theta_function
 use electrons,           ONLY:levels,spin,spin_occ
 !
 implicit none
 !
 type(levels) :: E
 real(SP)     :: RADIATIVE_pre_factor
 integer      :: i_qp,i_m
 !
 ! Work Space
 !
 integer   :: i_k,i_n,i_spin
 real(SP)  :: E_diff,f_m
 !
 i_n    = QP_table(i_qp,1)
 i_k    = QP_table(i_qp,3)
 i_spin = spin(QP_table(i_qp,:))
 !
 E_diff =E%E(i_n,i_k,i_spin)-E%E(i_m,i_k,i_spin)
 f_m    =E%f(i_m,i_k,i_spin)/spin_occ
 !
 RADIATIVE_pre_factor=abs(E_diff)*( QP_time_order_sign*(1._SP-f_m)*theta_function( E_diff,0.01/HA2EV,0) +&
&                                                             f_m *theta_function(-E_diff,0.01/HA2EV,0) )
 !
end subroutine QED_Radiative_Lifetime_component

