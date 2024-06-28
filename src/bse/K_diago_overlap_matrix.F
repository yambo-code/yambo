!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, MG, DS
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
subroutine K_diago_overlap_matrix(BS_V_left,BS_V_right,BS_overlap)
 !
 use pars,           ONLY:SP,cZERO,cONE
 use wrapper_omp,    ONLY:M_by_M_omp
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver 
 use LIVE_t,         ONLY:live_timing
 use linear_algebra, ONLY:INV
 use BS,             ONLY:BS_H_dim
 use BS_solvers,     ONLY:BSS_n_eig
 !
 implicit none
 !
 complex(SP),intent(in)  :: BS_V_left(BS_H_dim,BSS_n_eig),BS_V_right(BS_H_dim,BSS_n_eig)
 complex(SP),intent(out) :: BS_overlap(BSS_n_eig,BSS_n_eig)
 !
 ! BS_overlap(i,j)=sum_k conjg(BS_V_left(k,i))*BS_V_right(k,j)
 !
 BS_overlap=cZERO
 !
 call live_timing('BSK overlap mat',1)
 !
 call M_by_M_omp('c','n',BSS_n_eig,BSS_n_eig,BS_H_dim,cONE,BS_V_left,BS_H_dim,BS_V_right,BS_H_dim,cZERO,BS_overlap,BSS_n_eig)
 !
 call LINEAR_ALGEBRA_driver(INV,M=BS_overlap)
 call live_timing(steps=1)
 call live_timing()
 !
end subroutine K_diago_overlap_matrix
