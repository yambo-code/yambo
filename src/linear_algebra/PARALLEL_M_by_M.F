!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
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
subroutine PARALLEL_M_by_M(ORTHO,POOL,A_slk,B_slk,C_slk,transa,transb,m,n,k,C)
 !
 !  Perform the parallel matrix multiplication
 !
 !          C = A B 
 !
 !  where A(m,k), B(k,n), C(m,n)
 !
 use pars,           ONLY:SP,cONE,cZERO
 use SLK_m,          ONLY:POOL_group,ORTHO_group,SLK_matrix
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 implicit none
 !
 integer           :: n,m,k
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 type(SLK_matrix)  :: A_slk, B_slk, C_slk
 character         :: transa,transb
 complex(SP)       :: C(n,m)
 !
 character(64) :: subname="PARALLEL_M_by_M"
 !
 C=0.0_SP
 !
 if (ORTHO%coordinate(1)==-1) return
 !
#if defined _TIMING
 call timing('PARALLEL_M_by_M',OPR='start')
#endif
 !
#if defined _DOUBLE
 CALL PZGEMM( transa, transb, m, n, k, cONE, A_slk%blc, 1, 1, A_slk%desc, &
              B_slk%blc, 1, 1, B_slk%desc, cZERO, C_slk%blc, 1, 1, C_slk%desc)
#else
 CALL PCGEMM( transa, transb, m, n, k, cONE, A_slk%blc, 1, 1, A_slk%desc, &
              B_slk%blc, 1, 1, B_slk%desc, cZERO, C_slk%blc, 1, 1, C_slk%desc)
#endif
 !
#if defined _TIMING
 call timing('PARALLEL_M_by_M',OPR='stop')
#endif
 !
end subroutine
