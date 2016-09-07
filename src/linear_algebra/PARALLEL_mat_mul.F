!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine PARALLEL_mat_mul(ORTHO,POOL,A_slk,B_slk,C_slk,transa,transb,m,n,k,C)
 !
 !  Perform the parallel matrix multiplication
 !                   C = A B 
 !  where A(mk), B(kn), C(mn)
 !
 use pars,           ONLY:SP,cONE,cZERO
 use parallel_m,     ONLY:PP_redux_wait
 use SLK_m,          ONLY:POOL_group,ORTHO_group,SLK_matrix
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,WS,LINEAR_ALGEBRA_WS_clean_up
 !
 implicit none
 !
 integer           :: n,m,k
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 type(SLK_matrix)  :: A_slk, B_slk, C_slk
 character         :: transa,transb
 complex(SP)       :: C(n,m)
 !
 character(64) :: subname="PARALLEL_mat_mul"
 logical :: do_SLK
 !
 C=0.0_SP
 !
 do_SLK=.TRUE.
 if (ORTHO%coordinate(1)==-1) do_SLK=.FALSE.
 !
 if ( do_SLK ) then
   !
#if defined _DOUBLE
   CALL PZGEMM( transa, transb, m, n, k, cONE, A_slk%blc, 1, 1, A_slk%desc, &
                B_slk%blc, 1, 1, B_slk%desc, cZERO, C_slk%blc, 1, 1, C_slk%desc)
#else
   CALL PCGEMM( transa, transb, m, n, k, cONE, A_slk%blc, 1, 1, A_slk%desc, &
                B_slk%blc, 1, 1, B_slk%desc, cZERO, C_slk%blc, 1, 1, C_slk%desc)
#endif
   !
   ! gather the solution
   !
   C(C_slk%rows(1):C_slk%rows(2),C_slk%cols(1):C_slk%cols(2))=C_slk%blc(:C_slk%nrows_used,:C_slk%ncols_used)
   !
 endif
 !
 call PP_redux_wait(C,COMM=POOL%INTRA_comm)
 !
 call LINEAR_ALGEBRA_WS_clean_up(WS)
 ! 
end subroutine
