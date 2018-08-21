!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, AF
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
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
subroutine PARALLEL_inversion(ORTHO,POOL,M_slk,N)
 !
 ! perform the inversion by using scalapack
 !
 use SLK_m,          ONLY:POOL_group,ORTHO_group,SLK_matrix
 use linear_algebra, ONLY:WS,LINEAR_ALGEBRA_error,WS
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#include<memory.h>
 !
 integer           :: N
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 type(SLK_matrix)  :: M_slk
 !
 character(64) :: subname="PARALLEL_inversion"
 !
 if (ORTHO%coordinate(1)==-1) return
 !
#if defined _TIMING
 call timing('PARALLEL_inversion',OPR='start')
#endif
 !
 ! LWORK  = LOCr(N+MOD(IA-1,MB_A))*NB_A
 ! LIWORK = LOCc( N_A + MOD(JA-1, NB_A) ) + NB_A
 !
 M_slk%lwork  = M_slk%nrows*M_slk%nrows
 M_slk%liwork = M_slk%nrows+M_slk%nrows
 !
 YAMBO_ALLOC(WS%v_cmplx,(M_slk%lwork))
 YAMBO_ALLOC(WS%vp_int,(M_slk%liwork))
 YAMBO_ALLOC(WS%v_int,(N+M_slk%nrows))
 !
#if defined _DOUBLE
 CALL PZGETRF( N, N, M_slk%blc, 1, 1, M_slk%desc, WS%v_int, M_slk%info )
 if ( M_slk%info /= 0 ) call LINEAR_ALGEBRA_error(subname,'performing PZGETRF')
 !
 CALL PZGETRI( N, M_slk%blc, 1, 1, M_slk%desc,WS%v_int, WS%v_cmplx, M_slk%lwork, WS%vp_int, M_slk%liwork, M_slk%info )
 if ( M_slk%info /= 0 ) call LINEAR_ALGEBRA_error(subname,'performing PZGETRI')
#else
 CALL PCGETRF( N, N, M_slk%blc, 1, 1, M_slk%desc, WS%v_int, M_slk%info )
 if ( M_slk%info /= 0 ) call LINEAR_ALGEBRA_error(subname,'performing PCGETRF')
 !
 CALL PCGETRI( N, M_slk%blc, 1, 1, M_slk%desc,WS%v_int, WS%v_cmplx, M_slk%lwork, WS%vp_int, M_slk%liwork, M_slk%info )
 if ( M_slk%info /= 0 ) call LINEAR_ALGEBRA_error(subname,'performing PCGETRI')
#endif
 ! 
#if defined _TIMING
 call timing('PARALLEL_inversion',OPR='stop')
#endif
 !
end subroutine PARALLEL_inversion
