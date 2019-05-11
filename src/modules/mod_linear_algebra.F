!
!        Copyright (C) 2000-2019 the YAMBO team
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
#ifdef _CUDA
 use cudafor
 use cusolverDn
#endif
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
   !
   integer                  :: i_fail
   integer                  :: dim
   !
#ifdef _CUDA
   real(SP),    allocatable, device :: v_real_d(:)
   real(SP),    allocatable, device :: vp_real_d(:)
   complex(SP), allocatable, device :: v_cmplx_d(:)
   complex(SP), allocatable, device :: vp_cmplx_d(:)
   complex(SP), allocatable, device :: m1_cmplx_d(:,:)
   complex(SP), allocatable, device :: m2_cmplx_d(:,:)
   integer    , allocatable, device :: v_int_d(:)
   integer    , allocatable, device :: vp_int_d(:)
#endif
   !
 end type LALGEBRA_WS
 !
 public :: LU_factorization
 public :: LU_inversion
 public :: SV_decomposition
 public :: M_eigenvalues
 !
 interface M_eigenvalues
   module procedure heev,geev
 end interface
 !
 contains
   !
   subroutine LINEAR_ALGEBRA_WS_reset(WS)
     type(LALGEBRA_WS) :: WS
     YAMBO_FREE(WS%v_real)
     YAMBO_FREE(WS%vp_real)
     YAMBO_FREE(WS%v_cmplx)
     YAMBO_FREE(WS%vp_cmplx)
     YAMBO_FREE(WS%m1_cmplx)
     YAMBO_FREE(WS%m2_cmplx)
     YAMBO_FREE(WS%v_int)
     YAMBO_FREE(WS%vp_int)
     !
#ifdef _CUDA
     YAMBO_FREE(WS%v_real_d)
     YAMBO_FREE(WS%vp_real_d)
     YAMBO_FREE(WS%v_cmplx_d)
     YAMBO_FREE(WS%vp_cmplx_d)
     YAMBO_FREE(WS%m1_cmplx_d)
     YAMBO_FREE(WS%m2_cmplx_d)
     YAMBO_FREE(WS%v_int_d)
     YAMBO_FREE(WS%vp_int_d)
#endif
   end subroutine
   !
   subroutine LINEAR_ALGEBRA_error(calling_subr,message_)
     use stderr,  ONLY:STRING_pack
     use com,     ONLY:error
     character(*) calling_subr,message_
     call error( trim( STRING_pack('LINEAR ALGEBRA driver [',trim(calling_subr),']:',trim(message_)) ))
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
