!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV
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
subroutine exciton_spin(Xk,Xen,BS_R_right,BS_E,BS_E_degs,lambda_s,n_lambda)
 !
 ! Excitonc Spin
 !
 use pars,          ONLY:SP
 use BS,            ONLY:BS_H_dim
 use BS_solvers,    ONLY:BSS_n_eig
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_sp_pol,n_spinor
 use YPP,           ONLY:excitons_sort_and_report,BSiq
 !
 implicit none
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 complex(SP)   :: BS_R_right(BS_H_dim),BS_E(BS_H_dim)
 integer       :: n_lambda,BS_E_degs(BSS_n_eig),lambda_s(n_lambda)
 !
 real(SP)      :: S_z(BS_H_dim),S_sq(BS_H_dim)
 !
 call section('=','Reporting sorted Energies, Strengths and Spins')
 !
 if (n_sp_pol==2) call excitons_collinear(BS_E_degs,S_z,S_sq)
 !
 if (n_spinor==2) call excitons_non_collinear(Xk,lambda_s,n_lambda,S_z,S_sq)
 !
 call excitons_sort_and_report(BSiq,Xen,BS_R_right,BS_E,S_z=S_z,S_sq=S_sq)
 !
end subroutine exciton_spin   
