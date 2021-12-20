!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, AF
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
#include<dev_defs.h>
!
module wrapper
 !
 ! To remember:
 !
 ! TRANSA = 'N' or 'n',  op( A ) = A.
 ! TRANSA = 'T' or 't',  op( A ) = A'.
 ! TRANSA = 'C' or 'c',  op( A ) = conjg( A' ).
 !
 use pars,   ONLY:SP,cI
 use device_linalg_m,   only: M_by_M_gpu=>dev_xGEMM_gpu,dev_xGEMM_gpu,dev_xGEMV_gpu,&
&                             dev_xAXPY_gpu,dev_xDOTC_gpu,dev_xDOTU_gpu
 use dev_cublas_m
 !
 implicit none
 !
 interface M_by_M
   module procedure :: mm_cgemm
   module procedure :: mm_c
#ifdef _CUDAF
          procedure :: dev_xGEMM_gpu
#endif
 end interface
 !
 interface M_by_V
   module procedure :: mv_cgemv,mv_sgemv,mv_c,mv_r
#ifdef _CUDAF
   module procedure :: mv_c_gpu
!          procedure :: dev_xGEMV_gpu
#endif
 end interface
 !
 interface V_plus_alpha_V
   module procedure :: vv_s,vv_c,vv_saxpy,vv_caxpy,MM_caxpy
#ifdef _CUDAF
   module procedure :: vv_c_gpu
!          procedure :: dev_xAXPY_gpu
#endif
 end interface
 !
 interface M_plus_alpha_M
   module procedure :: MM_caxpy
 end interface
 !
 interface V_by_V_pwise
   module procedure :: V_by_V_pwise_cpu
 end interface
 !
 interface Vstar_dot_V
   module procedure :: Vstar_dot_V_c1_cpu, Vstar_dot_V_c2_cpu
 end interface
 !
 interface Vstar_dot_VV
   module procedure :: Vstar_dot_VV_c1_cpu
 end interface
 !
 interface V_dot_V
   module procedure :: V_dot_V_r1_cpu, V_dot_V_c1_cpu, V_dot_V_c2_cpu
 end interface
 !
 interface V_dot_VV
   module procedure :: V_dot_VV_c1_cpu
 end interface
 !
 ! GPU-aware interfaces
 ! defined but unsuable or working on host memory when undef _GPU
 ! (de facto, place holders to have _gpu names defined)
 !
 interface V_by_V_pwise_gpu
   module procedure :: V_by_V_pwise_gpu
 end interface
 interface Vstar_dot_V_gpu
   module procedure :: Vstar_dot_V_c1_gpu, Vstar_dot_V_c2_gpu
 end interface
 interface Vstar_dot_VV_gpu
   module procedure :: Vstar_dot_VV_c1_gpu
 end interface
 interface V_dot_V_gpu
   module procedure :: V_dot_V_c1_gpu
 end interface
 interface V_dot_VV_gpu
   module procedure :: V_dot_VV_c1_gpu
 end interface
 !
 public :: V_by_V_pwise_gpu
 public :: Vstar_dot_V_gpu
 public :: Vstar_dot_VV_gpu
 public :: V_dot_V_gpu
 public :: V_dot_VV_gpu
 public :: M_by_M_gpu
 !
 public :: V_by_V_pwise
 public :: Vstar_dot_V
 public :: Vstar_dot_VV
 public :: V_dot_V
 public :: V_dot_VV
 public :: V_plus_alpha_V
 public :: M_by_V
 public :: M_by_M
 !
 public :: V_copy
 public :: FADEVA
 !
 contains
   !
   !===================
   ! interface M_by_M 
   !===================
   !
   subroutine mm_cgemm(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
     implicit none
     !
     ! CGEMM  performs one of the matrix-matrix operations
     !
     !    C := alpha*op( A )*op( B ) + beta*C,
     !
     ! where  op( X ) is one of
     !
     !    op( X ) = X   or   op( X ) = X**T   or   op( X ) = X**H,
     !
     ! alpha and beta are scalars, and A, B and C are matrices, with op( A )
     ! an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
     !
     complex(SP), intent(in)  :: ALPHA,BETA
     integer,     intent(in)  :: K,LDA,LDB,LDC,M,N
     character,   intent(in)  :: TRANSA,TRANSB
     complex(SP), intent(in)  :: A(LDA,*),B(LDB,*)
     complex(SP), intent(out) :: C(LDC,*)
#if defined _DOUBLE
     call ZGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#else
     call CGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#endif
   end subroutine mm_cgemm
   !
   subroutine mm_cgemm_gpu(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
     implicit none
     complex(SP),          intent(in)  :: ALPHA,BETA
     integer,              intent(in)  :: K,LDA,LDB,LDC,M,N
     character,            intent(in)  :: TRANSA,TRANSB
     complex(SP) DEV_ATTR, intent(in)  :: A(LDA,*),B(LDB,*)
     complex(SP) DEV_ATTR, intent(out) :: C(LDC,*)
     !
#if defined _GPU
     !DEV_ACC data present(A,B,C)
     !DEV_ACC host_data use_device(A,B,C)
#  if defined _DOUBLE
     call cublasZGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#  else
     call cublasCGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#  endif
     !DEV_ACC end host_data
     !DEV_ACC end data
#else
#  if defined _DOUBLE
     call ZGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#  else
     call CGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#  endif
#endif
   end subroutine mm_cgemm_gpu

   subroutine mm_c(TRANSA,TRANSB,msize,A,B,C)
     integer,  intent(in)  :: msize
     complex(SP), intent(in)  :: A(msize,msize),B(msize,msize)
     complex(SP), intent(out) :: C(msize,msize)
     character,   intent(in)  :: TRANSA,TRANSB
#if defined _DOUBLE
     call ZGEMM(TRANSA,TRANSB,msize,msize,msize,(1._SP,0._SP),A,msize,B,msize,(0._SP,0._SP),C,msize)
#else
     call CGEMM(TRANSA,TRANSB,msize,msize,msize,(1._SP,0._SP),A,msize,B,msize,(0._SP,0._SP),C,msize)
#endif
   end subroutine mm_c
   !
   !===================
   ! interface M_by_V 
   !===================
   !
   subroutine mv_sgemv(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
     real(SP), intent(in) :: ALPHA,BETA
     integer,  intent(in) :: INCX,INCY,LDA,M,N
     character,intent(in) :: TRANS
     real(SP), intent(in) :: A(LDA,*),X(*)
     real(SP), intent(out):: Y(*)
#if defined _DOUBLE
     call DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#else
     call SGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#endif
   end subroutine mv_sgemv
   !
   subroutine mv_cgemv(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
     complex(SP), intent(in) :: ALPHA,BETA
     integer,     intent(in) :: INCX,INCY,LDA,M,N
     character,   intent(in) :: TRANS
     complex(SP), intent(in) :: A(LDA,*),X(*)
     complex(SP), intent(out):: Y(*)
#if defined _DOUBLE
     call ZGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#else
     call CGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#endif
   end subroutine mv_cgemv
   !
   subroutine mv_c(TRANS,msize,A,X,Y)
     integer,     intent(in) :: msize
     complex(SP), intent(in) :: A(msize,*),X(*)
     complex(SP), intent(out):: Y(*)
     character,   intent(in) :: TRANS
#if defined _DOUBLE
     call ZGEMV(TRANS,msize,msize,(1._SP,0._SP),A,msize,X,1,(0._SP,0._SP),Y,1)
#else
     call CGEMV(TRANS,msize,msize,(1._SP,0._SP),A,msize,X,1,(0._SP,0._SP),Y,1)
#endif
   end subroutine mv_c
   !
   subroutine mv_c_gpu(TRANS,msize,A,X,Y)
     integer,              intent(in) :: msize
     complex(SP) DEV_ATTR, intent(in) :: A(msize,msize),X(msize)
     complex(SP) DEV_ATTR, intent(out):: Y(msize)
     character,            intent(in) :: TRANS
     !
     call dev_xGEMV_gpu(TRANS,msize,msize,(1._SP,0._SP),A,msize,X,1,(0._SP,0._SP),Y,1)
     !
   end subroutine mv_c_gpu
   !
   subroutine mv_r(TRANS,msize,A,X,Y)
     integer,  intent(in) :: msize
     real(SP), intent(in) :: A(msize,*),X(*)
     real(SP), intent(out):: Y(*)
     character,intent(in) :: TRANS
#if defined _DOUBLE
     call DGEMV(TRANS,msize,msize,1._SP,A,msize,X,1,0._SP,Y,1)
#else
     call SGEMV(TRANS,msize,msize,1._SP,A,msize,X,1,0._SP,Y,1)
#endif
   end subroutine mv_r
   !
   !=========================
   ! interface M_plus_alpha_M
   !=========================
   !
   subroutine MM_caxpy(N, CA, CX,  CY )
     complex(SP), intent(in) :: CA
     integer,     intent(in) :: N
     complex(SP), intent(in) :: CX(N,N)
     complex(SP), intent(out):: CY(N,N)
#if defined _DOUBLE
     call ZAXPY(N**2,CA,CX,1,CY,1)
#else
     call CAXPY(N**2,CA,CX,1,CY,1)
#endif
   end subroutine MM_caxpy
   !
   !=========================
   ! interface V_plus_alpha_V
   !=========================
   !
   subroutine vv_c(N, CA, CX,  CY )
     complex(SP), intent(in) :: CA
     integer,     intent(in) :: N
     complex(SP), intent(in) :: CX(*)
     complex(SP), intent(out):: CY(*)
#if defined _DOUBLE
     call ZAXPY(N,CA,CX,1,CY,1)
#else
     call CAXPY(N,CA,CX,1,CY,1)
#endif
   end subroutine vv_c

   !
   subroutine vv_caxpy(N, CA, CX, LDA,  CY, LDB )
     complex(SP), intent(in) :: CA
     integer,     intent(in) :: N
     complex(SP), intent(in) :: CX(*)
     complex(SP), intent(out):: CY(*)
     integer,     intent(in) :: LDA,LDB
#if defined _DOUBLE
     call ZAXPY(N,CA,CX,LDA,CY,LDB)
#else
     call CAXPY(N,CA,CX,LDA,CY,LDB)
#endif
   end subroutine vv_caxpy
   !
   subroutine vv_c_gpu(N, CA, CX, CY )
     complex(SP),          intent(in)  :: CA
     integer,              intent(in)  :: N
     complex(SP) DEV_ATTR, intent(in)  :: CX(N)
     complex(SP) DEV_ATTR, intent(out) :: CY(N)
     !
     call dev_xAXPY_gpu(N,CA,CX,1,CY,1)
   end subroutine vv_c_gpu
   !
   subroutine vv_s(N, CA, CX, CY )
     real(SP),    intent(in) :: CA
     integer,     intent(in) :: N
     real(SP),    intent(in) :: CX(*)
     real(SP),    intent(out):: CY(*)
#if defined _DOUBLE
     call DAXPY(N,CA,CX,1,CY,1)
#else
     call SAXPY(N,CA,CX,1,CY,1)
#endif
   end subroutine vv_s   
   !
   subroutine vv_saxpy(N, CA, CX, INCX, CY, INCY )
     real(SP),    intent(in) :: CA
     integer,     intent(in) :: N
     real(SP),    intent(in) :: CX(*)
     real(SP),    intent(out):: CY(*)
     integer,     intent(in) :: INCX,INCY
#if defined _DOUBLE
     call DAXPY(N,CA,CX,INCX,CY,INCY)
#else
     call SAXPY(N,CA,CX,INCX,CY,INCY)
#endif
   end subroutine vv_saxpy   

   !
   !======
   ! COPY 
   !======
   !
   subroutine V_copy(N,CX,CY)
     integer,    intent(in)  :: N
     complex(SP),intent(in)  :: CX(*)
     complex(SP),intent(out) :: CY(*)
#if defined _DOUBLE
     call zcopy(N,CX,1,CY,1)
#else
     call ccopy(N,CX,1,CY,1)
#endif
   end subroutine V_copy   
   !
   !==============
   ! DOT PRODUCTS: Vstar_dot_V
   !==============
   !
   complex(SP) function Vstar_dot_V_c1_cpu(N,CX,CY)
     implicit none
     integer,    intent(in) :: N
     complex(SP),intent(in) :: CX(:),CY(:)
#if defined _DOUBLE
     complex(SP)::zdotc
     Vstar_dot_V_c1_cpu=ZDOTC(N,CX,1,CY,1)
#else
     complex(SP)::cdotc
     Vstar_dot_V_c1_cpu=CDOTC(N,CX,1,CY,1)
#endif
   end function Vstar_dot_V_c1_cpu
   !
   complex(SP) function Vstar_dot_V_c2_cpu(N,CX,CY)
     implicit none
     integer,    intent(in) :: N
     complex(SP),intent(in) :: CX(:,:),CY(:,:)
#if defined _DOUBLE
     complex(SP)::zdotc
     Vstar_dot_V_c2_cpu=ZDOTC(N,CX,1,CY,1)
#else
     complex(SP)::cdotc
     Vstar_dot_V_c2_cpu=CDOTC(N,CX,1,CY,1)
#endif
   end function Vstar_dot_V_c2_cpu
   !
   complex(SP) function Vstar_dot_V_c1_gpu(N,CX,CY)
     implicit none
     integer,              intent(in) :: N
     complex(SP) DEV_ATTR, intent(in) :: CX(:),CY(:)
     !
     Vstar_dot_V_c1_gpu=dev_xDOTC_gpu(N,CX,1,CY,1)
     !
   end function Vstar_dot_V_c1_gpu
   !
   complex(SP) function Vstar_dot_V_c2_gpu(N,CX,CY)
     implicit none
     integer,              intent(in) :: N
     complex(SP) DEV_ATTR, intent(in) :: CX(:,:),CY(:,:)
     !
#if defined _GPU
     !DEV_ACC data present(CX,CY)
     !DEV_ACC host_data use_device(CX,CY)
#  if defined _DOUBLE
     Vstar_dot_V_c2_gpu=cublasZdotc(N,CX,1,CY,1)
#  else
     Vstar_dot_V_c2_gpu=cublasCdotc(N,CX,1,CY,1)
#  endif
     !DEV_ACC end host_data
     !DEV_ACC end data
#else
#  if defined _DOUBLE
     complex(SP)::zdotc
     Vstar_dot_V_c2_gpu=ZDOTC(N,CX,1,CY,1)
#  else
     complex(SP)::cdotc
     Vstar_dot_V_c2_gpu=CDOTC(N,CX,1,CY,1)
#  endif
#endif
   end function Vstar_dot_V_c2_gpu
   !
   !==============
   ! DOT PRODUCTS: Vstar_dot_VV
   !==============
   !
   complex(SP) function Vstar_dot_VV_c1_cpu(N,CX,CY,CZ)
     implicit none
     integer,    intent(in) :: N
     complex(SP),intent(in) :: CX(N),CY(N),CZ(N)
#if defined _DOUBLE
     complex(SP)::zdotc
     Vstar_dot_VV_c1_cpu=ZDOTC(N,CX,1,CY*CZ,1)
#else
     complex(SP)::cdotc
     Vstar_dot_VV_c1_cpu=CDOTC(N,CX,1,CY*CZ,1)
#endif
   end function Vstar_dot_VV_c1_cpu
   !
   complex(SP) function Vstar_dot_VV_c1_gpu(N,CX,CY,CZ)
     implicit none
     integer,              intent(in) :: N
     complex(SP) DEV_ATTR, intent(in) :: CX(N),CY(N),CZ(N)
     integer :: i
     !
     Vstar_dot_VV_c1_gpu=0.0_SP
     !DEV_ACC data present(CX,CY,CZ)
     !DEV_ACC parallel loop
     !DEV_CUF kernel do(1) <<<*,*>>>
     do i = 1, N
       Vstar_dot_VV_c1_gpu=Vstar_dot_VV_c1_gpu+conjg(CX(i))*CY(i)*CZ(i)
     enddo
     !DEV_ACC end data
   end function Vstar_dot_VV_c1_gpu
   !
   !==============
   ! DOT PRODUCTS: V_dot_V
   !==============
   !
   real(SP) function V_dot_V_r1_cpu(N,CX,CY)
     implicit none
     integer, intent(in) :: N
     real(SP),intent(in) :: CX(:),CY(:)
#if defined _DOUBLE
     real(SP)::ddot
     V_dot_V_r1_cpu=DDOT(N,CX,1,CY,1)
#else
     real(SP)::sdot
     V_dot_V_r1_cpu=SDOT(N,CX,1,CY,1)
#endif
   end function V_dot_V_r1_cpu
   !
   complex(SP) function V_dot_V_c1_cpu(N,CX,CY)
     implicit none
     integer,    intent(in) :: N
     complex(SP),intent(in) :: CX(:),CY(:)
#if defined _DOUBLE
     complex(SP)::zdotu
     V_dot_V_c1_cpu=ZDOTU(N,CX,1,CY,1)
#else
     complex(SP)::cdotu
     V_dot_V_c1_cpu=CDOTU(N,CX,1,CY,1)
#endif
   end function V_dot_V_c1_cpu
   !
   complex(SP) function V_dot_V_c2_cpu(N,CX,CY)
     implicit none
     integer,    intent(in) :: N
     complex(SP),intent(in) :: CX(:,:),CY(:,:)
#if defined _DOUBLE
     complex(SP)::zdotu
     V_dot_V_c2_cpu=ZDOTU(N,CX,1,CY,1)
#else
     complex(SP)::cdotu
     V_dot_V_c2_cpu=CDOTU(N,CX,1,CY,1)
#endif
   end function V_dot_V_c2_cpu
   !
   complex(SP) function V_dot_V_c1_gpu(N,CX,CY)
     implicit none
     integer,              intent(in) :: N
     complex(SP) DEV_ATTR, intent(in) :: CX(:),CY(:)
     !
     V_dot_V_c1_gpu=dev_xDOTU_gpu(N,CX,1,CY,1)
     !
   end function V_dot_V_c1_gpu
   !
   !==============
   ! DOT PRODUCTS: V_dot_VV
   !==============
   !
   complex(SP) function V_dot_VV_c1_cpu(N,CX,CY,CZ)
     implicit none
     integer,     intent(in) :: N
     complex(SP), intent(in) :: CX(N),CY(N),CZ(N)
#if defined _DOUBLE
     complex(SP)::zdotu
     V_dot_VV_c1_cpu=ZDOTU(N,CX,1,CY*CZ,1)
#else
     complex(SP)::cdotu
     V_dot_VV_c1_cpu=CDOTU(N,CX,1,CY*CZ,1)
#endif
   end function V_dot_VV_c1_cpu
   !
   complex(SP) function V_dot_VV_c1_gpu(N,CX,CY,CZ)
     integer,              intent(in) :: N
     complex(SP) DEV_ATTR, intent(in) :: CX(N),CY(N),CZ(N)
     integer :: i
     !
     V_dot_VV_c1_gpu=0.0_SP
     !
     !DEV_ACC data present(CX,CY,CZ)
     !DEV_ACC parallel loop async
     !DEV_CUF kernel do(1) <<<*,*>>>
     do i = 1, N
       V_dot_VV_c1_gpu=V_dot_VV_c1_gpu+CX(i)*CY(i)*CZ(i)
     enddo
     !DEV_ACC end data
   end function V_dot_VV_c1_gpu
   !
   !=========
   ! MISC
   !=========
   !
   complex(SP) function FADEVA(Z)
     !
     complex(SP), intent(in) :: Z
     real(SP)    :: rW(2),rZ(2)
     integer     :: istatus
     !
     istatus=0
     rZ=(/real(Z,SP),aimag(Z)/)
     !
     ! Compute rW=w(-z)
     !=================
#if defined _DOUBLE
     call zwofz(rZ,rW,istatus)
#else
     call cwofz(rZ,rW,istatus)
#endif
     !
     FADEVA=cmplx(rW(1),rW(2),SP)
     !
   end function
   !
   subroutine V_by_V_pwise_cpu(N,CZ,CX,CY)
     integer,    intent(in) :: N
     complex(SP),intent(in) :: CX(:),CY(:)
     complex(SP),intent(out):: CZ(:)
     !
     integer :: i
     do i=1,N
        CZ(i)=CX(i)*CY(i)
     enddo
   end subroutine V_by_V_pwise_cpu
   !
   subroutine V_by_V_pwise_gpu(N,CZ,CX,CY)
     integer,              intent(in) :: N
     complex(SP) DEV_ATTR, intent(in) :: CX(N),CY(N)
     complex(SP) DEV_ATTR, intent(out):: CZ(N)
     !
     integer :: i
     !DEV_ACC data present(CX,CY,CZ)
     !DEV_ACC parallel loop
     !DEV_CUF kernel do(1) <<<*,*>>>
     do i=1,N
        CZ(i)=CX(i)*CY(i)
     enddo
     !DEV_ACC end data
   end subroutine V_by_V_pwise_gpu
   !
end module
