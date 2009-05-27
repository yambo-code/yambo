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
subroutine V_qp_basis_to_H(ik,V,H)
 !
 use electrons,      ONLY:n_sp_pol,spin
 use pars,           ONLY:SP
 use QP_m,           ONLY:QP_nb,QP_table,QP_n_states
 implicit none
 !
 integer     :: ik
 complex(SP) :: V(QP_n_states)
 complex(SP) :: H(QP_nb,QP_nb,n_sp_pol)
 !
 ! Work Space
 !
 integer    :: i1,ib,ibp,i_spin
 !
 ! Fill ...
 !
 do i1=1,QP_n_states
   !
   if (QP_table(i1,3)/=ik) cycle
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   i_spin = spin(QP_table(i1,:))
   H(ib,ibp,i_spin) = H(ib,ibp,i_spin) + V(i1)
   !
   ! Force to be Hermitian
   !
   if (ib/=ibp) H(ibp,ib,i_spin) = conjg(H(ib,ibp,i_spin))
   !
 end do
 !
end subroutine 
