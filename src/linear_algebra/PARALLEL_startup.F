!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine PARALLEL_startup(ORTHO, POOL, M_slk, N, M )
 !
 use pars,           ONLY:SP
 use SLK_m,          ONLY:POOL_group,ORTHO_group,SLK_matrix
 use linear_algebra, ONLY:WS,LINEAR_ALGEBRA_error
 !
 implicit none
 !
 integer           :: N
 type(ORTHO_group) :: ORTHO
 type(POOL_group)  :: POOL
 type(SLK_matrix)  :: M_slk
 complex(SP)       :: M(N,N)
 !
 ! Work Space
 !
 integer   :: lld,N_blc,nprow,npcol,myrow,mycol
 !
 ! init global blacs grid
 !
 call BLACS_GRIDINFO( ORTHO%ortho_cntx, ORTHO%grid(1), ORTHO%grid(2), ORTHO%coordinate(1), ORTHO%coordinate(2) )
 !
 nprow=ORTHO%grid(1)
 npcol=ORTHO%grid(2)
 myrow=ORTHO%coordinate(1)
 mycol=ORTHO%coordinate(2)
 !
 ! spectator tasks
 !
 if (ORTHO%coordinate(1) == -1) call LINEAR_ALGEBRA_error("PARALLEL_startup"," ")
 !
 ! distribute the matrix on the process grid
 ! Initialize the array descriptors for the matrices A and B
 !
 N_blc = int(N/nprow)
 if (N_blc*nprow < N ) N_blc=N_blc+1
 !
 lld = N_blc
 !
 call DESCINIT( M_slk%desc, N, N, N_blc, N_blc, 0, 0, ORTHO%ortho_cntx, lld, M_slk%info )
 !
 allocate( WS%m1_cmplx(N_blc,N_blc) )
 allocate( WS%v_int(N+N_blc) )
 !
 ! LWORK  = LOCr(N+MOD(IA-1,MB_A))*NB_A
 ! LIWORK = LOCc( N_A + MOD(JA-1, NB_A) ) + NB_A
 !
 M_slk%lwork  = N_blc*N_blc
 M_slk%liwork = N_blc+N_blc
 !
 allocate( WS%v_cmplx(M_slk%lwork) )
 allocate( WS%vp_int(M_slk%liwork) )
 !
 ! distribute the matrix A
 !
 M_slk%rows=(/myrow*N_blc+1,min(myrow*N_blc+N_blc,N)/)
 M_slk%cols=(/mycol*N_blc+1,min(mycol*N_blc+N_blc,N)/)
 !
 WS%m1_cmplx=0.0_SP
 WS%m1_cmplx=M(M_slk%rows(1):M_slk%rows(2),M_slk%cols(1):M_slk%cols(2))
 !
end subroutine 
