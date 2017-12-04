!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, AM
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
subroutine PARALLEL_M_by_M(A,B,C,transa,transb,m,n,k)
 !
 !  Perform the parallel matrix multiplication
 !
 !          A x B = C
 !
 !  where A(m,k), B(k,n), C(m,n)
 !
 use pars,           ONLY:cONE,cZERO
 use SLK_m,          ONLY:SLK_ORTHO
 use matrix,         ONLY:PAR_matrix
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 implicit none
 !
 integer           :: n,m,k
 type(PAR_matrix)  :: A, B, C
 character         :: transa,transb
 !
 character(64) :: subname="PARALLEL_M_by_M"
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
#if defined _TIMING
 call timing('PARALLEL_M_by_M',OPR='start')
#endif
 !
#if defined _DOUBLE
 CALL PZGEMM&
#else
 CALL PCGEMM&
#endif
& ( transa, transb, m, n, k, cONE, A%blc(:,:,A%I), 1, 1, A%desc, &
    B%blc(:,:,B%I), 1, 1, B%desc, cZERO, C%blc(:,:,C%I), 1, 1, C%desc)
 !
#if defined _TIMING
 call timing('PARALLEL_M_by_M',OPR='stop')
#endif
 !
end subroutine
