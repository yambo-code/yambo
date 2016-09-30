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
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
subroutine PARALLEL_diagonalization(ORTHO, POOL, M_slk, EIGEN_slk, n, M, E )
 !
 ! perform the diagonalization by using scalapack
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:PP_redux_wait
 use SLK_m,          ONLY:POOL_group,ORTHO_group,SLK_matrix
 use linear_algebra, ONLY:WS,LINEAR_ALGEBRA_error,WS,LINEAR_ALGEBRA_WS_clean_up
 !
 implicit none
 !
 integer           :: n
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 type(SLK_matrix)  :: M_slk,EIGEN_slk
 complex(SP)       :: M(n,n)
 real(SP)          :: E(n)
 !
 character(64) :: subname="PARALLEL_diagonalization"
 logical :: do_SLK

 call LINEAR_ALGEBRA_error(subname,'PARALLEL diagonalization not implemented')
 !
#if defined _DOUBLE
 !if ( info /= 0 ) call LINEAR_ALGEBRA_error('performing PZGETRF',' ')
 !
#else
 !call PCHEEV('V','U',n,WS%m1_cmplx,1,1,descA,E,WS%m2_cmplx,1,1,descZ,WS%v_cmplx,lwork,WS%vp_cmplx,lrwork,info)
 !if ( info /= 0 ) call LINEAR_ALGEBRA_error('performing PCGETRF',' ')
 !
#endif
 !
 ! gather the eigenvectors ...
 !
 M=0.0_SP
 M(M_slk%rows(1):M_slk%rows(2),M_slk%cols(1):M_slk%cols(2))=WS%m1_cmplx(:,:)
 !
 ! gather the eigenvectors ...
 !
 call PP_redux_wait(M,COMM=POOL%INTRA_comm)
 !
 call LINEAR_ALGEBRA_WS_clean_up(WS)
 ! 
end subroutine PARALLEL_diagonalization
