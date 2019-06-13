!
!        Copyright (C) 2000-2019 the YAMBO team
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
module wrapper
 !
 ! To remember:
 !
 ! TRANSA = 'N' or 'n',  op( A ) = A.
 ! TRANSA = 'T' or 't',  op( A ) = A'.
 ! TRANSA = 'C' or 'c',  op( A ) = conjg( A' ).
 !
 use pars,   ONLY:SP,cI
#ifdef _CUDA
 use cublas
#endif
 !
 implicit none
 !
 interface M_by_M
   module procedure mm_cgemm,mm_c 
 end interface
 !
 interface M_by_V 
   module procedure mv_cgemv,mv_sgemv,mv_c,mv_r
#ifdef _CUDA
   module procedure                   mv_c_gpu
#endif
 end interface
 !
 interface V_by_V_plus_V
   module procedure vv_saxpy,vv_caxpy,MM_caxpy
#ifdef _CUDA
   module procedure          vv_caxpy_gpu
#endif
 end interface
 !
 interface V_by_V_pwise
   module procedure V_by_V_pwise_cpu
#ifdef _CUDA
   module procedure V_by_V_pwise_gpu
#endif
 end interface
 !
 interface Vstar_dot_V
   module procedure Vstar_dot_V_c1_cpu, Vstar_dot_V_c2_cpu
#ifdef _CUDA
   module procedure Vstar_dot_V_c1_gpu, Vstar_dot_V_c2_gpu
#endif
 end interface
 !
 interface Vstar_dot_VV
   module procedure Vstar_dot_VV_c1_cpu
#ifdef _CUDA
   module procedure Vstar_dot_VV_c1_gpu
#endif
 end interface
 !
 interface V_dot_V
   module procedure V_dot_V_r1_cpu, V_dot_V_c1_cpu, V_dot_V_c2_cpu
#ifdef _CUDA
   module procedure V_dot_V_c1_gpu
#endif
 end interface
 !
 interface V_dot_VV
   module procedure V_dot_VV_c1_cpu
#ifdef _CUDA
   module procedure V_dot_VV_c1_gpu
#endif
 end interface
 !
 public :: V_copy
 public :: Vstar_dot_V
 public :: Vstar_dot_VV
 public :: V_dot_V
 public :: V_dot_VV
 public :: M_by_V
 public :: V_by_V_plus_V
 public :: V_by_V_pwise
 public :: FADEVA
 !
 contains
   !
   !===================
   ! interface M_by_M 
   !===================
   !
   subroutine mm_cgemm(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
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
#ifdef _CUDA
   subroutine mv_c_gpu(TRANS,msize,A,X,Y)
     integer,     intent(in) :: msize
     complex(SP), device, intent(in) :: A(msize,*),X(*)
     complex(SP), device, intent(out):: Y(*)
     character,   intent(in) :: TRANS
#if defined _DOUBLE
     call cublasZgemv(TRANS,msize,msize,(1._SP,0._SP),A,msize,X,1,(0._SP,0._SP),Y,1)
#else
     call cublasCgemv(TRANS,msize,msize,(1._SP,0._SP),A,msize,X,1,(0._SP,0._SP),Y,1)
#endif
   end subroutine mv_c_gpu
#endif
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
   ! interface V_by_V_plus_V 
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
   subroutine vv_caxpy(N, CA, CX,  CY )
     complex(SP), intent(in) :: CA
     integer,     intent(in) :: N
     complex(SP), intent(in) :: CX(*)
     complex(SP), intent(out):: CY(*)
#if defined _DOUBLE
     call ZAXPY(N,CA,CX,1,CY,1)
#else
     call CAXPY(N,CA,CX,1,CY,1)
#endif
   end subroutine vv_caxpy
   !
#ifdef _CUDA
   subroutine vv_caxpy_gpu(N, CA, CX, CY )
     complex(SP), intent(in) :: CA
     integer,     intent(in) :: N
     complex(SP), device, intent(in) :: CX(*)
     complex(SP), device, intent(out):: CY(*)
#if defined _DOUBLE
     call cublasZaxpy(N,CA,CX,1,CY,1)
#else
     call cublasCaxpy(N,CA,CX,1,CY,1)
#endif
   end subroutine vv_caxpy_gpu
#endif
   !
   subroutine vv_saxpy(N, CA, CX, CY )
     real(SP),    intent(in) :: CA
     integer,     intent(in) :: N
     real(SP),    intent(in) :: CX(*)
     real(SP),    intent(out):: CY(*)
#if defined _DOUBLE
     call DAXPY(N,CA,CX,1,CY,1)
#else
     call SAXPY(N,CA,CX,1,CY,1)
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
     complex(SP),intent(in) :: CX(*),CY(*)
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
#ifdef _CUDA
   complex(SP) function Vstar_dot_V_c1_gpu(N,CX,CY)
     implicit none
     integer,             intent(in) :: N
     complex(SP), device, intent(in) :: CX(*),CY(*)
     !
#if defined _DOUBLE
     Vstar_dot_V_c1_gpu=cublasZdotc(N,CX,1,CY,1)
#else
     Vstar_dot_V_c1_gpu=cublasCdotc(N,CX,1,CY,1)
#endif
   end function Vstar_dot_V_c1_gpu
   !
   complex(SP) function Vstar_dot_V_c2_gpu(N,CX,CY)
     implicit none
     integer,             intent(in) :: N
     complex(SP), device, intent(in) :: CX(:,:),CY(:,:)
     !
#if defined _DOUBLE
     Vstar_dot_V_c2_gpu=cublasZdotc(N,CX,1,CY,1)
#else
     Vstar_dot_V_c2_gpu=cublasCdotc(N,CX,1,CY,1)
#endif
   end function Vstar_dot_V_c2_gpu
   !
#endif
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
#ifdef _CUDA
   complex(SP) function Vstar_dot_VV_c1_gpu(N,CX,CY,CZ)
     implicit none
     integer,             intent(in) :: N
     complex(SP), device, intent(in) :: CX(N),CY(N),CZ(N)
     integer :: i
     !
     Vstar_dot_VV_c1_gpu=0.0_SP
     !$cuf kernel do(1) <<<*,*>>>
     do i = 1, N
       Vstar_dot_VV_c1_gpu=Vstar_dot_VV_c1_gpu+conjg(CX(i))*CY(i)*CZ(i)
     enddo
   end function Vstar_dot_VV_c1_gpu
#endif
   !
   !==============
   ! DOT PRODUCTS: V_dot_V
   !==============
   !
   real(SP) function V_dot_V_r1_cpu(N,CX,CY)
     implicit none
     integer, intent(in) :: N
     real(SP),intent(in) :: CX(*),CY(*)
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
     complex(SP),intent(in) :: CX(*),CY(*)
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
#ifdef _CUDA
   complex(SP) function V_dot_V_c1_gpu(N,CX,CY)
     implicit none
     integer,    intent(in) :: N
     complex(SP), device, intent(in) :: CX(*),CY(*)
#if defined _DOUBLE
     complex(SP)::zdotu
     V_dot_V_c1_gpu=cublasZDOTU(N,CX,1,CY,1)
#else
     complex(SP)::cdotu
     V_dot_V_c1_gpu=cublasCDOTU(N,CX,1,CY,1)
#endif
   end function V_dot_V_c1_gpu
#endif
   !
   !==============
   ! DOT PRODUCTS: V_dot_VV
   !==============
   !
   complex(SP) function V_dot_VV_c1_cpu(N,CX,CY,CZ)
     implicit none
     integer,    intent(in) :: N
     complex(SP),intent(in) :: CX(N),CY(N),CZ(N)
#if defined _DOUBLE
     complex(SP)::zdotu
     V_dot_VV_c1_cpu=ZDOTU(N,CX,1,CY*CZ,1)
#else
     complex(SP)::cdotu
     V_dot_VV_c1_cpu=CDOTU(N,CX,1,CY*CZ,1)
#endif
   end function V_dot_VV_c1_cpu
   !
#ifdef _CUDA
   complex(SP) function V_dot_VV_c1_gpu(N,CX,CY,CZ)
     integer,    intent(in) :: N
     complex(SP), device, intent(in) :: CX(N),CY(N),CZ(N)
     integer :: i
     !
     V_dot_VV_c1_gpu=0.0_SP
     !$cuf kernel do(1) <<<*,*>>>
     do i = 1, N
       V_dot_VV_c1_gpu=V_dot_VV_c1_gpu+CX(i)*CY(i)*CZ(i)
     enddo
   end function V_dot_VV_c1_gpu
#endif
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
     complex(SP),intent(in) :: CX(*),CY(*)
     complex(SP),intent(out):: CZ(*)
     !
     integer :: i
     do i=1,N
        CZ(i)=CX(i)*CY(i)
     enddo
   end subroutine V_by_V_pwise_cpu
   !
#ifdef _CUDA
   subroutine V_by_V_pwise_gpu(N,CZ,CX,CY)
     integer,    intent(in) :: N
     complex(SP), device, intent(in) :: CX(N),CY(N)
     complex(SP), device, intent(out):: CZ(N)
     !
     integer :: i
     !$cuf kernel do(1) <<<*,*>>> 
     do i=1,N
        CZ(i)=CX(i)*CY(i)
     enddo
   end subroutine V_by_V_pwise_gpu
#endif
   !
end module
