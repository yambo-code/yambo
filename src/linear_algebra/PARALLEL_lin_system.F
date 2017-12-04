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
subroutine PARALLEL_lin_system(A,B,n,nrhs)
 !
 !  Solve the linear system
 !
 !                   A X = B
 !
 !  where A is a n-x-n matrix and B is n-x-nrhs
 !
 use SLK_m,          ONLY:SLK_ORTHO
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LALGEBRA_WS,&
&                         LINEAR_ALGEBRA_WS_clean_up
 use matrix,         ONLY:PAR_matrix
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#include<memory.h>
 !
 integer           :: n,nrhs
 type(PAR_matrix)  :: A, B
 !
 character(64)     :: subname="PARALLEL_lin_system"
 type(LALGEBRA_WS) :: WS
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
#if defined _TIMING
 call timing('PARALLEL_lin_system',OPR='start')
#endif
 !
 ! better estimate of the dimensions to be included
 !
 YAMBO_FREE(WS%v_int)
 YAMBO_ALLOC(WS%v_int,(n+A%nrows))
 !
#if defined _DOUBLE
 CALL PZGESV&
#else
 CALL PCGESV&
#endif
& ( n, nrhs, A%blc(:,:,A%I), 1, 1, A%desc, WS%v_int, B%blc(:,:,B%I), 1, 1, B%desc, WS%i_fail)
 !
 if (WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing P(Z/C)GESV')
 !
 call LINEAR_ALGEBRA_WS_clean_up(WS) 
 ! 
#if defined _TIMING
 call timing('PARALLEL_lin_system',OPR='stop')
#endif
 !
end subroutine
