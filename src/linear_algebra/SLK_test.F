!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine SLK_test(E,k,q) 
 ! 
 ! simple program to check parallel linear algebra
 !
 use pars,           ONLY:SP
 use com,            ONLY:msg
 use SLK_m,          ONLY:POOL_group,ORTHO_group,SLK_test_H_dim,SLK_COM_A2A
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use interfaces,     ONLY:PARALLEL_global_indexes,LINEAR_ALGEBRA_driver
 use linear_algebra, ONLY:INV,DIAGO,LIN_SYS,MAT_MUL,USE_LK,USE_SLK
 use matrix_operate, ONLY:hermitian
 !
 implicit none
 !
 type(levels)   ::E
 type(bz_samp)  ::k,q
 !
 ! Work Space
 !
 integer                    :: ndim,ierr
 complex(SP),   allocatable :: A_in(:,:),A_out(:,:),A_out_par(:,:)
 complex(SP),   allocatable :: B_in(:,:),M_mat(:,:)
 real(SP),      allocatable :: W(:), W_par(:)
 type (POOL_group)          :: POOL
 type (ORTHO_group)         :: ORTHO
 !
 call section("*","ScaLapacK test")
 !
 call PARALLEL_global_indexes(E,k,q,"ScaLapacK")
 !
 ndim    = SLK_test_H_dim
 !
 ! workspace
 !
 YAMBO_ALLOC(A_in,(ndim,ndim))
 YAMBO_ALLOC(A_out,(ndim,ndim))
 YAMBO_ALLOC(A_out_par,(ndim,ndim))
 !
 ! matrix inversion
 !
 call section("=",'Matrix Inversion')
 call build_matrix( ndim, A_in )
 !
 A_out  =A_in
 call LINEAR_ALGEBRA_driver(INV,M=A_out,lib_in=USE_LK)
 !
 A_out_par =A_in
 call LINEAR_ALGEBRA_driver(INV,M=A_out_par,lib_in=USE_SLK)
 !
 call matrix_check( "InvMat", ndim, ndim, A_out, A_out_par )
 call inverse_check( ndim, A_in, A_out_par )
 !
 ! Linear system solving
 !
 call section("=",'Linear system solution')
 !
 YAMBO_ALLOC(B_in,(ndim,ndim))
 YAMBO_ALLOC(M_mat,(ndim,ndim))
 call build_matrix( ndim, A_in )
 call build_matrix( ndim, B_in )
 !
 M_mat = A_in
 A_out = B_in
 call LINEAR_ALGEBRA_driver(LIN_SYS,M=M_mat,B=A_out,lib_in=USE_LK)
 !
 M_mat     = A_in
 A_out_par = B_in
 call LINEAR_ALGEBRA_driver(LIN_SYS,M=M_mat,B=A_out_par,lib_in=USE_SLK)
 !
 call matrix_check( "LinSys X", ndim, ndim, A_out, A_out_par )
 YAMBO_FREE(B_in)
 YAMBO_FREE(M_mat)
 !
 ! matrix multiplication
 !
 call section("=",'Matrix multiplication')
 !
 call build_matrix( ndim, A_in )
 !
 call LINEAR_ALGEBRA_driver(MAT_MUL,M=A_in,B=A_in,C=A_out,lib_in=USE_LK)
 !
 call LINEAR_ALGEBRA_driver(MAT_MUL,M=A_in,B=A_in,C=A_out_par,lib_in=USE_SLK)
 !
 call matrix_check( "Mat Mult", ndim, ndim, A_out, A_out_par )
 !
 ! diagonalization
 !
 call section("=",'Matrix Hermitean Diagonalization')
 !
 call build_matrix( ndim, A_in )
 A_in=hermitian(A_in)
 !
 YAMBO_ALLOC(W,(ndim))
 YAMBO_ALLOC(W_par,(ndim))
 !
 A_out  =A_in
 call LINEAR_ALGEBRA_driver(DIAGO,M=A_out,lib_in=USE_LK,e_real=W)
 !
 A_out_par =A_in
 call LINEAR_ALGEBRA_driver(DIAGO,M=A_out_par,lib_in=USE_SLK,e_real=W_par)
 !
 call matrix_check( " Eigenval", ndim, 1, cmplx(W,KIND=SP), cmplx(W_par,KIND=SP))
 !
 ! in order to check the eigenvectors
 ! an overall phase must be aligned.
 ! At least we can check their moduli
 !
 call matrix_check( "EigenVect", ndim, ndim, cmplx(abs(A_out),KIND=SP), cmplx(abs(A_out_par),KIND=SP) )
 !
 YAMBO_FREE(W)
 YAMBO_FREE(W_par)
 !
 ! cleanup
 !
 YAMBO_FREE(A_in)
 YAMBO_FREE(A_out)
 YAMBO_FREE(A_out_par)
 !
end subroutine SLK_test 
!
!====================================
 subroutine build_matrix(ndim, Amat )
 !===================================
 !
 ! build A = I + scal * randmat
 ! a small value of scal ensures A is invertible
 !
 use pars,       ONLY:SP,DP
 use parallel_m, ONLY:PP_redux_wait
 use SLK_m,      ONLY:SLK_COM_A2A
 use com,        ONLY:msg
 !
 implicit none
 !
 integer           :: ndim,iseed(8)
 complex(SP)       :: Amat(ndim,ndim)
 character(12)     :: ch(3)
 real(DP),external :: dlaran
 !
 integer   :: i, j, ierr
 real(SP) :: scal=0.2_SP 
 !
 call date_and_time(ch(1),ch(2),ch(3),iseed)
 iseed=iabs(iseed)
 !
 iseed(4)=2*(iseed(4)/2)+1
 !
 Amat = 0.0_SP
 !
 if ( SLK_COM_A2A(1)%CPU_id == 0 ) then
   !
   do i = 1, ndim
     Amat(i,i) = 1.0_SP
   enddo
   !
   do j = 1, ndim
     do i = j, ndim
       Amat(i,j) = Amat(i,j) + scal * dlaran(iseed(4:))+(0,1.)*dlaran(iseed(4:))
       Amat(j,i) = conjg(Amat(i,j))
     enddo
   enddo
   !
 endif
 !
 call PP_redux_wait(Amat,COMM=SLK_COM_A2A(1)%COMM)
 !
end subroutine build_matrix
!
!============================================
 subroutine inverse_check( ndim, Amat, Ainv )
 !===========================================
 use pars,       ONLY:SP
 use com,        ONLY:error,msg
 use parallel_m, ONLY:mpi_comm_world
 use wrapper,    ONLY:M_by_M
#include<memory.h>
 !
 integer            :: ndim
 complex(SP) :: Amat(ndim,ndim)
 complex(SP) :: Ainv(ndim,ndim)
 !
 integer   :: i, j, ierr
 logical   :: lerror
 real(SP)  :: toll=1._SP
 complex(SP), allocatable :: zmat(:,:)
 !
 YAMBO_ALLOC(zmat,(ndim,ndim))
 !
 zmat=0._SP
 !
 call M_by_M( 'n','n', ndim,  Amat, Ainv, zmat)
 !
 lerror = .false.
 outer_loop:&
 do j = 1, ndim
   do i = j+1, ndim
     if ( abs( zmat(i,j) ) > toll ) then 
       lerror = .true.
       exit outer_loop
     endif
   enddo
 enddo outer_loop
 !
 do i = 1, ndim
   if ( abs( zmat(i,i)-1.0_SP ) > toll ) then 
     lerror = .true.
     exit
   endif
 enddo
 !
 if ( .not. lerror ) call msg("sr",'  Inverse_check:   passed')
 if (       lerror ) call msg("sr",'  Inverse_check:   failed')
 !
 YAMBO_FREE(zmat)
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine inverse_check
!
!=========================================================
 subroutine matrix_check( label, ndim1, ndim2, Amat, Bmat)
 !========================================================
 use pars,       ONLY:SP
 use com,        ONLY:error,msg
 use parallel_m, ONLY:mpi_comm_world
 implicit none
 !
 integer     :: ndim1,ndim2
 character(*):: label
 complex(SP) :: Amat(ndim1,ndim2)
 complex(SP) :: Bmat(ndim1,ndim2)
 !
 integer   :: i, j
 logical   :: lerror=.false.
 real(SP)  :: toll=1.0E-4
 !
 outer_loop:&
 do j = 1, ndim2
   do i = 1, ndim1
     if ( abs( Amat(i,j)-Bmat(i,j) ) > toll ) then 
       lerror = .true.
       exit outer_loop
     endif
   enddo
 enddo outer_loop
 !
 if ( .not. lerror ) call msg("sr",'  matrix_check on  '//trim(label)//' :   passed')
 if (       lerror ) call msg("sr",'  matrix_check on  '//trim(label)//' :   failed')
 !
end subroutine matrix_check
!
!======================================================
 subroutine matrix_write( filename, ndim1, ndim2, Amat)
 !=====================================================
 use pars,       ONLY:SP
 use parallel_m, ONLY:myid
 implicit none
 !
 integer     :: ndim1,ndim2
 character(*):: filename
 complex(SP) :: Amat(ndim1,ndim2)
 !
 integer :: i,j
 !
 if (myid/=0) return
 if (ndim1>100) return
 open(10,file=filename)
 do j = 1, ndim2
   do i = 1, ndim1
      write(10,"(2i5,2f20.12)") i,j, Amat(i,j)
   enddo
 enddo
 close(10)
 !
end subroutine matrix_write