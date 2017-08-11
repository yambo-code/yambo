!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, AF, AM
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
module wrapper_omp
 !
 ! To remember:
 !
 ! TRANSA = 'N' or 'n',  op( A ) = A.
 ! TRANSA = 'T' or 't',  op( A ) = A'.
 ! TRANSA = 'C' or 'c',  op( A ) = conjg( A' ).
 !
 use pars,    ONLY:SP,cZERO,cONE
 use wrapper, ONLY:V_copy,MM_caxpy,vv_caxpy,vv_saxpy,Vstar_dot_V,real_V_dot_V,V_dot_V
 use openmp,  ONLY:n_threads_now
 !
 implicit none
 !
 interface M_by_M_omp
   module procedure mm_cgemm_omp,mm_c_omp 
 end interface
 !
 interface M_by_V_omp
   module procedure mv_cgemv_omp,mv_sgemv_omp,mv_c_omp,mv_r_omp
 end interface
 !
 interface V_by_V_plus_V_omp
   module procedure vv_saxpy_omp,vv_caxpy_omp,MM_caxpy_omp
 end interface
 !
 public :: V_copy_omp
 public :: Vstar_dot_V_omp
 public :: V_dot_V_omp
 public :: real_V_dot_V_omp
 public :: M_by_V_omp
 !
 contains
   !
   !===================
   ! interface M_by_M 
   !===================
   !
   subroutine mm_cgemm_omp(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
     complex(SP), intent(in)  :: ALPHA,BETA
     integer,     intent(in)  :: K,LDA,LDB,LDC,M,N
     character,   intent(in)  :: TRANSA,TRANSB
     complex(SP), intent(in)  :: A(LDA,*),B(LDB,*)
     complex(SP), intent(out) :: C(LDC,*)
#if defined _OPENMP && defined _DOUBLE
     call ZGEMM_omp(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#elif defined _OPENMP && !defined _DOUBLE
     call CGEMM_omp(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#elif defined _DOUBLE
     call ZGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#else
     call CGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
#endif
   end subroutine mm_cgemm_omp
   !
   subroutine mm_c_omp(TRANSA,TRANSB,msize,A,B,C)
     integer,  intent(in)  :: msize
     complex(SP), intent(in)  :: A(msize,msize),B(msize,msize)
     complex(SP), intent(out) :: C(msize,msize)
     character,   intent(in)  :: TRANSA,TRANSB
     !
#if defined _OPENMP && defined _DOUBLE
     call ZGEMM_omp(TRANSA,TRANSB,msize,msize,msize,cONE,A,msize,B,msize,cZERO,C,msize)
#elif defined _OPENMP && !defined _DOUBLE
     call CGEMM_omp(TRANSA,TRANSB,msize,msize,msize,cONE,A,msize,B,msize,cZERO,C,msize)
#elif defined _DOUBLE
     call ZGEMM(TRANSA,TRANSB,msize,msize,msize,cONE,A,msize,B,msize,cZERO,C,msize)
#else
     call CGEMM(TRANSA,TRANSB,msize,msize,msize,cONE,A,msize,B,msize,cZERO,C,msize)
#endif
   end subroutine mm_c_omp
   !
   !===================
   ! interface M_by_V 
   !===================
   !
   subroutine mv_sgemv_omp(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
     real(SP), intent(in) :: ALPHA,BETA
     integer,  intent(in) :: INCX,INCY,LDA,M,N
     character,intent(in) :: TRANS
     real(SP), intent(in) :: A(LDA,*),X(*)
     real(SP), intent(out):: Y(*)
#if defined _OPENMP && defined _DOUBLE
     call DGEMV_omp(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#elif defined _OPENMP && !defined _DOUBLE
     call SGEMV_omp(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#elif defined _DOUBLE
     call DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#else
     call SGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#endif
   end subroutine mv_sgemv_omp
   !
   subroutine mv_cgemv_omp(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
     complex(SP), intent(in) :: ALPHA,BETA
     integer,     intent(in) :: INCX,INCY,LDA,M,N
     character,   intent(in) :: TRANS
     complex(SP), intent(in) :: A(LDA,*),X(*)
     complex(SP), intent(out):: Y(*)
#if defined _OPENMP && defined _DOUBLE
     call ZGEMV_omp(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#elif defined _OPENMP && !defined _DOUBLE
     call CGEMV_omp(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#elif defined _DOUBLE
     call ZGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#else
     call CGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#endif
   end subroutine mv_cgemv_omp
   !
   subroutine mv_c_omp(TRANS,msize,A,X,Y)
     integer,     intent(in) :: msize
     complex(SP), intent(in) :: A(msize,*),X(*)
     complex(SP), intent(out):: Y(*)
     character,   intent(in) :: TRANS
#if defined _OPENMP && defined _DOUBLE
     call ZGEMV_omp(TRANS,msize,msize,cONE,A,msize,X,1,cZERO,Y,1)
#elif defined _OPENMP && !defined _DOUBLE
     call CGEMV_omp(TRANS,msize,msize,cONE,A,msize,X,1,cZERO,Y,1)
#elif defined _DOUBLE
     call ZGEMV(TRANS,msize,msize,cONE,A,msize,X,1,cZERO,Y,1)
#else
     call CGEMV(TRANS,msize,msize,cONE,A,msize,X,1,cZERO,Y,1)
#endif
   end subroutine mv_c_omp
   !
   subroutine mv_r_omp(TRANS,msize,A,X,Y)
     integer,  intent(in) :: msize
     real(SP), intent(in) :: A(msize,*),X(*)
     real(SP), intent(out):: Y(*)
     character,intent(in) :: TRANS
#if defined _OPENMP && defined _DOUBLE
     call DGEMV_omp(TRANS,msize,msize,1._SP,A,msize,X,1,0._SP,Y,1)
#elif defined _OPENMP && !defined _DOUBLE
     call SGEMV_omp(TRANS,msize,msize,1._SP,A,msize,X,1,0._SP,Y,1)
#elif defined _DOUBLE
     call DGEMV(TRANS,msize,msize,1._SP,A,msize,X,1,0._SP,Y,1)
#else
     call SGEMV(TRANS,msize,msize,1._SP,A,msize,X,1,0._SP,Y,1)
#endif
   end subroutine mv_r_omp
   !
   !=========================
   ! interface V_by_V_plus_V 
   !=========================
   !
   subroutine MM_caxpy_omp(N, CA, CX,  CY )
     complex(SP), intent(in) :: CA
     integer,     intent(in) :: N
     complex(SP), intent(in) :: CX(N,N)
     complex(SP), intent(out):: CY(N,N)
     !
#if defined _OPENMP 
     integer :: i,j
     if (n_threads_now>1) then
!$omp parallel do default(shared), private(i,j)
       do j=1,N
         do i=1,N
           CY(i,j) = CY(i,j) + CA * CX(i,j)
         enddo
       enddo
!$omp end parallel do
     else
       call MM_caxpy(N, CA, CX,  CY )
     endif
     !
#else
    call MM_caxpy(N, CA, CX,  CY )
#endif
   end subroutine MM_caxpy_omp
   !
   subroutine vv_caxpy_omp(N, CA, CX,  CY )
     complex(SP), intent(in) :: CA
     integer,     intent(in) :: N
     complex(SP), intent(in) :: CX(*)
     complex(SP), intent(out):: CY(*)
     !
#if defined _OPENMP
     integer :: i
     if (n_threads_now>1) then
!$omp parallel do default(shared), private(i)
       do i=1,N
          CY(i) = CY(i) + CA * CX(i)
       enddo
!$omp end parallel do
     else
       call vv_caxpy(N, CA, CX,  CY )
     endif
     !
#else
     call vv_caxpy(N, CA, CX,  CY )
#endif
   end subroutine vv_caxpy_omp
   !
   subroutine vv_saxpy_omp(N, CA, CX, CY )
     real(SP),    intent(in) :: CA
     integer,     intent(in) :: N
     real(SP),    intent(in) :: CX(*)
     real(SP),    intent(out):: CY(*)
     !
#if defined _OPENMP
     integer :: i
     if (n_threads_now>1) then
!$omp parallel do default(shared), private(i)
       do i=1,N
         CY(i) = CY(i) + CA * CX(i)
       enddo
!$omp end parallel do
     else
       call vv_saxpy(N, CA, CX,  CY )
     endif
     !
#else
     call vv_saxpy(N, CA, CX,  CY )
#endif
   end subroutine vv_saxpy_omp   
   !
   !======
   ! COPY 
   !======
   !
   subroutine V_copy_omp(N,CX,CY)
     integer,    intent(in)  :: N
     complex(SP),intent(in)  :: CX(*)
     complex(SP),intent(out) :: CY(*)
     !
#if defined _OPENMP
     integer :: i
     if (n_threads_now>1) then
!$omp parallel do default(shared), private(i)
       do i=1,N
         CY(i)=CX(i)
       enddo
!$omp end parallel do
     else
       call V_copy(N,CX,CY)
     endif
#else
     call V_copy(N,CX,CY)
#endif
     !
   end subroutine V_copy_omp   
   !
   !==============
   ! DOT PRODUCTS
   !==============
   !
   complex(SP) function Vstar_dot_V_omp(N,CX,CY)
     integer,    intent(in) :: N
     complex(SP),intent(in) :: CX(*),CY(*)
     !
#if defined _OPENMP
     integer     :: i
     complex(SP) :: ctmp
     if (n_threads_now>1) then
       ctmp=cZERO
!$omp parallel do default(shared), private(i), reduction(+:ctmp)
       do i=1,N
         ctmp=ctmp +conjg(CX(i))*CY(i)
       enddo 
!$omp end parallel do
       Vstar_dot_V_omp=ctmp
     else
       Vstar_dot_V_omp=Vstar_dot_V(N,CX,CY)
     endif
#else
     Vstar_dot_V_omp=Vstar_dot_V(N,CX,CY)
#endif
     !
   end function Vstar_dot_V_omp   
   !
   real(SP) function real_V_dot_V_omp(N,CX,CY)
     integer, intent(in) :: N
     real(SP),intent(in) :: CX(*),CY(*)
     !
#if defined _OPENMP
     integer  :: i
     real(SP) :: rtmp
     if (n_threads_now>1) then
       rtmp=0.0_SP
!$omp parallel do default(shared), private(i), reduction(+:rtmp)
       do i=1,N
         rtmp=rtmp +CX(i)*CY(i)
       enddo 
!$omp end parallel do
       real_V_dot_V_omp=rtmp
     else
       real_V_dot_V_omp=real_V_dot_V(N,CX,CY)
     endif
#else
     real_V_dot_V_omp=real_V_dot_V(N,CX,CY)
#endif
   end function real_V_dot_V_omp
   !
   complex(SP) function V_dot_V_omp(N,CX,CY)
     integer,    intent(in) :: N
     complex(SP),intent(in) :: CX(*),CY(*)
     !
#if defined _OPENMP
     integer     :: i
     complex(SP) :: ctmp
     if (n_threads_now>1) then
       ctmp=0.0_SP
!$omp parallel do default(shared), private(i), reduction(+:ctmp)
       do i=1,N
         ctmp=ctmp +CX(i)*CY(i)
       enddo 
!$omp end parallel do
       V_dot_V_omp=ctmp
     else
       V_dot_V_omp=V_dot_V(N,CX,CY)
     endif
#else
     V_dot_V_omp=V_dot_V(N,CX,CY)
#endif
     !
   end function V_dot_V_omp
   !
end module
