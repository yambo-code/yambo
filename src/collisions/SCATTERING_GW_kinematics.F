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
subroutine SCATTERING_GW_kinematics(i_qp,i_q_bz,i_m,I,k,q,E,N,G0,Ek,Ekmq,E_diff)
 !
 ! m,k-q   n,k
 ! --<--.-<--
 !      | 
 !      |
 !     \|/ q,G,\alpha,...
 !      |
 !      
 ! n   =QP_table(i_qp,1)
 ! k   =QP_table(i_qp,3)
 !
 use pars,                ONLY:SP
 use electrons,           ONLY:spin,levels
 use R_lattice,           ONLY:qindx_S,bz_samp
 use QP_m,                ONLY:QP_table
 use collision,           ONLY:elemental_collision
 !
 implicit none
 !
 integer,       intent(in) ::i_qp,i_q_bz,i_m
 type(elemental_collision) ::I
 type(bz_samp)             ::k,q
 type(levels),  OPTIONAL   ::E
 integer,       OPTIONAL   ::N,G0
 real(SP),      OPTIONAL   ::Ek,Ekmq,E_diff
 !
 ! WorkSpace
 !
 integer  ::i_n,i_k,i_G0,i_k_m_q,i_k_m_q_bz,i_spin,i_k_m_q_s,i_q,i_q_s
 !
 i_n    = QP_table(i_qp,1)
 i_k    = QP_table(i_qp,3)
 i_G0       = qindx_S(i_k,i_q_bz,2)
 i_k_m_q_bz = qindx_S(i_k,i_q_bz,1) 
 i_k_m_q    = k%sstar(i_k_m_q_bz,1)
 i_k_m_q_s  = k%sstar(i_k_m_q_bz,2)
 i_spin     = spin(QP_table(i_qp,:))
 i_q        = q%sstar(i_q_bz,1)
 i_q_s      = q%sstar(i_q_bz,2)
 !
 I%is=(/i_n,i_k,1,i_spin/)
 I%os=(/i_m,i_k_m_q,i_k_m_q_s,i_spin/)
 I%qs=(/i_G0,i_q,i_q_s/)
 !
 if (present(N)) then
   N=i_n
 endif
 if (present(G0)) then
   G0=i_G0
 endif
 if (present(Ek)) then
   Ek=E%E(i_n,i_k,i_spin)
 endif
 if (present(Ekmq)) then
   Ekmq=E%E(i_m,i_k_m_q,i_spin)
 endif
 if (present(E_diff)) then
   E_diff=E%E(i_n,i_k,i_spin)-E%E(i_m,i_k_m_q,i_spin)
 endif
 !
end subroutine SCATTERING_GW_kinematics
