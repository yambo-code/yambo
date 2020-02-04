!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine PARALLEL_inversion(M,N)
 !
 ! perform the inversion by using scalapack
 !
 use SLK_m,          ONLY:SLK_ORTHO
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LALGEBRA_WS,&
&                         LINEAR_ALGEBRA_WS_reset
 use matrix,         ONLY:PAR_matrix
 use timing_m,       ONLY:timing
#include<memory.h>
 !
 integer           :: N
 type(PAR_matrix)  :: M
 !
 character(64)     :: subname="PARALLEL_inversion"
 type(LALGEBRA_WS) :: WS
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
 call timing('PARALLEL_inversion',OPR='start')
 !
 ! LWORK  = LOCr(N+MOD(IA-1,MB_A))*NB_A
 ! LIWORK = LOCc( N_A + MOD(JA-1, NB_A) ) + NB_A
 !
 M%lwork  = M%BLCnrows*M%BLCnrows
 M%liwork = M%BLCnrows+M%BLCnrows
 !
 YAMBO_ALLOC(WS%v_cmplx,(M%lwork))
 YAMBO_ALLOC(WS%vp_int,(M%liwork))
 YAMBO_ALLOC(WS%v_int,(N+M%nrows))
 !
#if defined _DOUBLE
 CALL PZGETRF&
#else
 CALL PCGETRF&
#endif
& ( N, N, M%blc(:,:,M%I), 1, 1, M%desc, WS%v_int, M%info )
 !
 if ( M%info /= 0 ) call LINEAR_ALGEBRA_error(subname,'performing P(Z/C)GETRF')
 !
#if defined _DOUBLE
 CALL PZGETRI&
#else
 CALL PCGETRI&
#endif
& ( N, M%blc(:,:,M%I),1, 1, M%desc,WS%v_int, WS%v_cmplx, M%lwork, WS%vp_int, M%liwork, M%info )
 !
 if ( M%info /= 0 ) call LINEAR_ALGEBRA_error(subname,'performing P(Z/C)GETRI')
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 ! 
 call timing('PARALLEL_inversion',OPR='stop')
 !
end subroutine PARALLEL_inversion
