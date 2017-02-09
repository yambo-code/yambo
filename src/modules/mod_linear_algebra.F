!
!        Copyright (C) 2000-2017 the YAMBO team
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
module linear_algebra
 !
 use pars,       ONLY:SP,schlen
 use parallel_m, ONLY:MPI_comm
#include<memory.h>
 !
 integer, parameter :: USE_LK=1
 integer, parameter :: USE_SLK=2
 integer, parameter :: ALREADY_FILLED=3
 integer, parameter :: DIAGO=4
 integer, parameter :: INV=5
 integer, parameter :: PARTIAL_DIAGO=6
 integer, parameter :: SVD_INV=7
 integer, parameter :: LIN_SYS=8
 integer, parameter :: MAT_MUL=9
 integer, parameter :: min_cpu_block_size=50
 !
 ! Common Work Space
 !
 type LALGEBRA_WS
   real(SP),    allocatable :: v_real(:)
   real(SP),    allocatable :: vp_real(:)
   complex(SP), allocatable :: v_cmplx(:)
   complex(SP), allocatable :: vp_cmplx(:)
   complex(SP), allocatable :: m1_cmplx(:,:)
   complex(SP), allocatable :: m2_cmplx(:,:)
   integer    , allocatable :: v_int(:)
   integer    , allocatable :: vp_int(:)
   integer                  :: i_fail
   integer                  :: dim
 end type LALGEBRA_WS
 !
 type(LALGEBRA_WS) :: WS
 !
 public :: LU_factorization
 public :: LU_inversion
 public :: SV_decomposition
 public :: M_eigenvalues
 !
 ! Distributed Matrix structure
 !==============================
 !
 integer, parameter   :: desc_len= 9
 logical              :: I_do_RCV=.TRUE.
 logical              :: I_do_SND=.TRUE.
 type(MPI_comm)       :: TRANSFER_world
 !
 type PAR_matrix
   !
   character(3) :: kind = "NAN" ! "PAR" "SLK" "XUP" "XDN" "SER"
   !
   ! Dimensions
   !
   integer :: I          = 1  ! Block element
   integer :: N          = -1 ! Global dimension (Parent Matrix)
   integer :: rows(2)    = 0  ! Real
   integer :: cols(2)    = 0  !
   integer :: nrows      = -1 ! 
   integer :: ncols      = -1 ! 
   !
   ! BLACS
   !
   integer :: BLCnrows      = -1 ! Dimension of the BLACS matrix (in general larger then the Real's)
   integer :: BLCncols      = -1 !
   integer :: BLCrows(2)    = 0  ! Dimension of the BLACS matrix (in general larger then the Real's)
   integer :: BLCcols(2)    = 0  !
   integer :: desc(desc_len)
   integer :: lwork      = -1
   integer :: lrwork     = -1
   integer :: liwork     = -1
   integer :: info       = -1  
   !
   ! COMMs
   !
   type(MPI_comm) :: INTRA_comm
   type(MPI_comm) :: INTER_comm 
   !
   ! DATA
   !
   integer :: Nb         = 1  ! # of Blocks
   !
   complex(SP), allocatable :: blc(:,:,:)
   !
 end type PAR_matrix 
 !
 interface M_eigenvalues
   module procedure heev,geev
 end interface
 !
 contains
   !
   subroutine MATRIX_reset(M_par)
     use parallel_m, ONLY:COMM_reset
     type(PAR_matrix)   :: M_par
     !
     M_par%kind       = "NAN"
     !
     M_par%I          = 1
     M_par%N          = -1
     M_par%rows       = 0
     M_par%cols       = 0
     M_par%nrows      = -1
     M_par%ncols      = -1
     !
     M_par%BLCrows    = 0
     M_par%BLCcols    = 0
     M_par%BLCnrows   = -1
     M_par%BLCncols   = -1
     M_par%desc       = 0
     M_par%lwork      = -1
     M_par%lrwork     = -1
     M_par%liwork     = -1
     M_par%info       = -1
     !
     M_par%Nb         = 1
     !
     call COMM_reset(M_par%INTER_comm)
     call COMM_reset(M_par%INTRA_comm)
     !
   end subroutine
   !
   subroutine LINEAR_ALGEBRA_WS_clean_up(WS)
     type(LALGEBRA_WS) :: WS
     Y_MEM_FREE(WS%v_real)
     Y_FREE(WS%v_real)
     Y_MEM_FREE(WS%vp_real)
     Y_FREE(WS%vp_real)
     Y_MEM_FREE(WS%v_cmplx)
     Y_FREE(WS%v_cmplx)
     Y_MEM_FREE(WS%vp_cmplx)
     Y_FREE(WS%vp_cmplx)
     Y_MEM_FREE(WS%m1_cmplx)
     Y_FREE(WS%m1_cmplx)
     Y_MEM_FREE(WS%m2_cmplx)
     Y_FREE(WS%m2_cmplx)
     Y_MEM_FREE(WS%v_int)
     Y_FREE(WS%v_int)
     Y_MEM_FREE(WS%vp_int)
     Y_FREE(WS%vp_int)
   end subroutine
   !
   subroutine LINEAR_ALGEBRA_error(calling_subr,message_)
     use stderr,  ONLY:string_pack
     use com,     ONLY:error
     character(*) calling_subr,message_
     call error( trim( string_pack('LINEAR ALGEBRA driver [',trim(calling_subr),']:',trim(message_)) ))
   end subroutine
   !
   !============================
   ! SINGLE VALUE DECOMPOSITION
   !============================
   !
   subroutine SV_decomposition(msize, M, SV, U, VH, work, lwork, r_WK, ifail)
     !
     integer,    intent(in)   :: msize, lwork
     integer,    intent(out)  :: ifail
     !     
     real(SP),   intent(out)  :: r_WK(*), SV(*)
     complex(SP),intent(inout):: M(msize,*)
     complex(SP),intent(out)  :: U(msize,*), VH(msize,*), work(*)
     !
#if defined _DOUBLE
   call ZGESVD('S','A',msize,msize,M,msize,SV,U,msize,VH,msize,work,lwork,r_WK,ifail)
#else
   call CGESVD('S','A',msize,msize,M,msize,SV,U,msize,VH,msize,work,lwork,r_WK,ifail)
#endif
     !
   end subroutine SV_decomposition
   !
   !=========================================
   ! LOWER/UPPER FACTORIZATION and INVERISON
   !=========================================
   !
   subroutine LU_factorization(msize,M,ipvt,ifail)
     !
     integer,     intent(in)   :: msize
     integer,     intent(out)  :: ifail, ipvt(*)
     complex(SP), intent(inout):: M(msize,*)
     !
#if defined _DOUBLE
     call zgetrf(msize,msize,M,msize,ipvt,ifail)
#else
     call cgetrf(msize,msize,M,msize,ipvt,ifail)
#endif
     !
   end subroutine LU_factorization
   !
   subroutine LU_inversion(msize,M,ipvt,work,lwork,ifail)
     !
     integer,     intent(in)   :: msize, lwork, ipvt(*)
     integer,     intent(out)  :: ifail
     complex(SP), intent(inout):: M(msize,*)
     complex(SP), intent(out)  :: work(*)
     !
#if defined _DOUBLE
     call zgetri(msize,M,msize,ipvt,work,lwork,ifail)
#else
     call cgetri(msize,M,msize,ipvt,work,lwork,ifail)
#endif
     !
   end subroutine LU_inversion
   !
   !==============================
   ! EIGENVALUES AND EIGENVECTORS 
   !==============================
   !
   subroutine heev(msize,M,E_real,work,lwork,rwk,ifail)
     !
     integer,    intent(in)    :: msize,lwork
     integer,    intent(out)   :: ifail
     real(SP),   intent(out)   :: E_real(*), rwk(*)
     complex(SP),intent(out)   :: work(*)
     complex(SP),intent(inout) :: M(msize,*)
     !
#if defined _DOUBLE
   call ZHEEV('V','U',msize,M,msize,E_real,work,lwork,rwk,ifail)
#else
   call CHEEV('V','U',msize,M,msize,E_real,work,lwork,rwk,ifail)
#endif
     !
   end subroutine heev 
   !
   subroutine geev(msize,M,E_cmpl,V_left,V_right,work,lwork,rwk,ifail)
     !
     integer,    intent(in)    :: msize,lwork
     integer,    intent(out)   :: ifail
     real(SP),   intent(out)   :: rwk(*)
     complex(SP),intent(out)   :: E_cmpl(*),V_left(msize,*),V_right(msize,*),work(*)
     complex(SP),intent(inout) :: M(msize,*)
     !
#if defined _DOUBLE
   call ZGEEV('V','V',msize,M,msize,E_cmpl,V_left,msize,V_right,msize,work,lwork,rwk,ifail)
#else
   call CGEEV('V','V',msize,M,msize,E_cmpl,V_left,msize,V_right,msize,work,lwork,rwk,ifail)
#endif
     !
   end subroutine geev
   !
end module linear_algebra
