!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): CA AF
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
 use pars,             only:SP,DP,cI
 !
 ! Implementation based on C_F_POINTER assumes that variables are contiguous,
 ! which may not be the case
 !
 use iso_c_binding,    only:C_LOC,C_F_POINTER
#ifdef _CUDAF
 use cudafor,          only:c_devloc
#endif
 !
 use devxlib_cublas
 use devxlib_linalg,   only: M_by_M_gpu=>devxlib_xGEMM_gpu,devxlib_xGEMM_gpu,devxlib_xGEMV_gpu,&
&                            devxlib_xAXPY_gpu,devxlib_xDOT_gpu,devxlib_xDOTC_gpu,devxlib_xDOTU_gpu
 !
 implicit none
 !
 interface M_by_M
   module procedure :: mm_cgemm
   module procedure :: mm_c,mm_cg
 end interface
 !
 interface M_by_V
   module procedure :: mv_cgemv,mv_sgemv,mv_c,mv_r
 end interface
 !
 interface V_plus_alpha_V
   module procedure :: vv_s,vv_c,vv_saxpy,vv_caxpy,MM_caxpy
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
   module procedure :: Vstar_dot_V_c1_cpu,Vstar_dot_V_c2_cpu,V_dot_V_r1_cpu,V_dot_V_r2_cpu
 end interface
 !
 interface Vstar_dot_VV
   module procedure :: Vstar_dot_VV_c1_cpu
 end interface
 !
 interface V_dot_V
   module procedure :: V_dot_V_r1_cpu,V_dot_V_r2_cpu,V_dot_V_c1_cpu,V_dot_V_c2_cpu
 end interface
 !
 interface V_dot_VV
   module procedure :: V_dot_VV_c1_cpu
 end interface
 !
 ! GPU-aware interfaces
 ! resorting to host functions when _GPU is not defined
 !
 interface V_by_V_pwise_gpu
   module procedure :: V_by_V_pwise_gpu
 end interface
 interface Vstar_dot_V_gpu
     module procedure :: Vstar_dot_V_c1_gpu,Vstar_dot_V_c2_gpu,V_dot_V_r1_gpu,V_dot_V_r2_gpu
 end interface
 interface Vstar_dot_VV_gpu
   module procedure :: Vstar_dot_VV_c1_gpu
 end interface
 interface V_dot_V_gpu
     module procedure :: V_dot_V_r1_gpu,V_dot_V_r2_gpu,V_dot_V_c1_gpu,V_dot_V_c2_gpu
 end interface
 interface V_dot_VV_gpu
   module procedure :: V_dot_VV_c1_gpu
 end interface
 interface V_plus_alpha_V_gpu
   module procedure :: vv_c_gpu
 end interface
 interface M_by_V_gpu
   module procedure :: mv_c_gpu, mv_sgemv_gpu, mv_cgemv_gpu
 end interface
 !
 !
 public :: V_by_V_pwise_gpu
 public :: Vstar_dot_V_gpu
 public :: Vstar_dot_VV_gpu
 public :: V_dot_V_gpu
 public :: V_dot_VV_gpu
 public :: V_plus_alpha_V_gpu
 public :: M_by_M_gpu
 public :: M_by_V_gpu
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
#if !defined _GPU
   !
   ! DS: 2023/05/03
   ! Workaround to fix final linking, otherwise it fails.
   ! The fails happens because lapack functions/subroutines are called
   ! by devxlib but not linked from the compiler if not present in
   ! the yambo sources. Probably the devxlib should depend on the
   ! lapack (similarly to the petsc) (?)
   !
#if !defined _DOUBLE
   integer function fix_compilation()
     implicit none
     real(SP)   ::r_mat(1,1),r_var(1),r_val
     complex(SP)::c_mat(1,1),c_var(1),c_val
     real(SP)   ::ddot
     complex(SP)::zdotu,zdotc
     r_val=DDOT(1,r_var,1,r_var,1)
     c_val=ZDOTU(1,c_var,1,c_var,1)
     c_val=ZDOTC(1,c_var,1,c_var,1)
     call DAXPY(1,r_val,r_var,1,r_var,1)
     call ZAXPY(1,c_val,c_var,1,c_var,1)
     return
   end function fix_compilation
#else
   integer function fix_compilation()
     implicit none
     real(selected_real_kind(6,30))   ::r_mat(1,1),r_var(1),r_val
     complex(selected_real_kind(6,30))::c_mat(1,1),c_var(1),c_val
     real(selected_real_kind(6,30))   ::sdot
     complex(selected_real_kind(6,30))::cdotu,cdotc
     r_val=SDOT(1,r_var,1,r_var,1)
     c_val=CDOTU(1,c_var,1,c_var,1)
     c_val=CDOTC(1,c_var,1,c_var,1)
     call SAXPY(1,r_val,r_var,1,r_var,1)
     call CAXPY(1,c_val,c_var,1,c_var,1)
     call SGEMV("n",1,1,r_val,r_mat,1,r_var,1,r_val,r_var,1)
     call CGEMV("n",1,1,c_val,c_mat,1,c_var,1,c_val,c_var,1)
     call CGEMM("n","n",1,1,1,c_val,c_mat,1,c_mat,1,c_val,c_mat,1)
     return
   end function fix_compilation
#endif
   !
#endif
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
   subroutine mm_c(TRANSA,TRANSB,msize,A,B,C)
     integer,     intent(in)  :: msize
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
   subroutine mm_cg(TRANSA,TRANSB,msize,A,B,BETA,C)
     complex(SP), intent(in)  :: BETA
     integer,     intent(in)  :: msize
     complex(SP), intent(in)  :: A(msize,msize),B(msize,msize)
     complex(SP), intent(out) :: C(msize,msize)
     character,   intent(in)  :: TRANSA,TRANSB
#if defined _DOUBLE
     call ZGEMM(TRANSA,TRANSB,msize,msize,msize,(1._SP,0._SP),A,msize,B,msize,BETA,C,msize)
#else
     call CGEMM(TRANSA,TRANSB,msize,msize,msize,(1._SP,0._SP),A,msize,B,msize,BETA,C,msize)
#endif
   end subroutine mm_cg
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
   subroutine mv_sgemv_gpu(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
     real(SP), intent(in) :: ALPHA,BETA
     integer,  intent(in) :: INCX,INCY,LDA,M,N
     character,intent(in) :: TRANS
     real(SP), intent(in)  DEV_ATTR :: A(:,:),X(:)
     real(SP), intent(out) DEV_ATTR :: Y(:)
     !
     call devxlib_xGEMV_gpu(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
   end subroutine mv_sgemv_gpu
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
   subroutine mv_cgemv_gpu(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
     complex(SP), intent(in) :: ALPHA,BETA
     integer,     intent(in) :: INCX,INCY,LDA,M,N
     character,   intent(in) :: TRANS
     complex(SP), intent(in)  DEV_ATTR :: A(:,:),X(:)
     complex(SP), intent(out) DEV_ATTR :: Y(:)
     !
     call devxlib_xGEMV_gpu(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
   end subroutine mv_cgemv_gpu
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
     call devxlib_xGEMV_gpu(TRANS,msize,msize,(1._SP,0._SP),A,msize,X,1,(0._SP,0._SP),Y,1)
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
   subroutine vv_caxpy(N, CA, CX, INCX,  CY, INCY )
     complex(SP), intent(in) :: CA
     integer,     intent(in) :: N
     complex(SP), intent(in) :: CX(*)
     complex(SP), intent(out):: CY(*)
     integer,     intent(in) :: INCX,INCY
#if defined _DOUBLE
     call ZAXPY(N,CA,CX,INCX,CY,INCY)
#else
     call CAXPY(N,CA,CX,INCX,CY,INCY)
#endif
   end subroutine vv_caxpy
   !
   subroutine vv_c_gpu(N, CA, CX, CY )
     complex(SP),          intent(in)  :: CA
     integer,              intent(in)  :: N
     complex(SP) DEV_ATTR, intent(in)  :: CX(N)
     complex(SP) DEV_ATTR, intent(out) :: CY(N)
     !
     call devxlib_xAXPY_gpu(N,CA,CX,1,CY,1)
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
     complex(SP),target,contiguous,intent(in) :: CX(:,:),CY(:,:)
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
     Vstar_dot_V_c1_gpu=devxlib_xDOTC_gpu(N,CX,1,CY,1)
     !
   end function Vstar_dot_V_c1_gpu
   !
   complex(SP) function Vstar_dot_V_c2_gpu(N,CX,CY)
     use iso_c_binding
     implicit none
     integer,              intent(in) :: N
     complex(SP) DEV_ATTR, target, intent(in) :: CX(:,:),CY(:,:)
     complex(SP), pointer DEV_ATTR :: CX_p(:), CY_p(:)
     !
#ifdef _CUDAF
     call c_f_pointer(c_devloc(CX),CX_p,[size(CX)])
     call c_f_pointer(c_devloc(CY),CY_p,[size(CY)])
#else
     call c_f_pointer(c_loc(CX),CX_p,[size(CX)])
     call c_f_pointer(c_loc(CY),CY_p,[size(CY)])
#endif
     !
     Vstar_dot_V_c2_gpu=devxlib_xDOTC_gpu(N,CX_p,1,CY_p,1)
     !     
   end function Vstar_dot_V_c2_gpu
   !
   !==============
   ! DOT PRODUCTS: Vstar_dot_VV
   !==============
   !
   complex(SP) function Vstar_dot_VV_c1_cpu(N,CX,CY,CZ,bounds)
     implicit none
     integer,    intent(in) :: N
     complex(SP),intent(in) :: CX(N),CY(N),CZ(N)
     integer, optional, intent(in) :: bounds(2)
     !
     integer :: bounds_(2),N_
     complex(DP), external :: zdotc
     complex(SP), external :: cdotc
     !
     bounds_(:)=(/1,N/)
     if (present(bounds)) bounds_=bounds
     N_=bounds_(2)-bounds_(1)+1
     !
#if defined _DOUBLE
     Vstar_dot_VV_c1_cpu=ZDOTC(N_,CX(bounds_(1):),1,CY(bounds_(1):)*CZ(bounds_(1):),1)
#else
     Vstar_dot_VV_c1_cpu=CDOTC(N_,CX(bounds_(1):),1,CY(bounds_(1):)*CZ(bounds_(1):),1)
#endif
   end function Vstar_dot_VV_c1_cpu
   !
   complex(SP) function Vstar_dot_VV_c1_gpu(N,CX,CY,CZ,bounds)
     implicit none
     integer,              intent(in) :: N
     complex(SP) DEV_ATTR, intent(in) :: CX(N),CY(N),CZ(N)
     integer,    optional, intent(in) :: bounds(2)
     !
     complex(SP) :: ctmp
     integer :: bounds_(2)
     integer :: i
     !
     bounds_(:)=(/1,N/)
     if (present(bounds)) bounds_=bounds
     !
     Vstar_dot_VV_c1_gpu=0.0_SP
     ctmp=0.0
     !
     !DEV_ACC_DEBUG data present(CX,CY,CZ)
     !DEV_ACC parallel loop
     !DEV_CUF kernel do(1) <<<*,*>>>
     !DEV_OMPGPU target map(present,alloc:CX,CY,CZ) map(tofrom:ctmp)
     !DEV_OMPGPU teams loop reduction(+:ctmp)
     !DEV_OMP parallel do default(shared), private(i), reduction(+:ctmp)
     do i = bounds_(1),bounds_(2)
       ctmp=ctmp+conjg(CX(i))*CY(i)*CZ(i)
     enddo
     !DEV_OMPGPU end target
     !DEV_ACC_DEBUG end data
     !
     Vstar_dot_VV_c1_gpu=ctmp
     !
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
     !
#if defined _DOUBLE
     real(SP)::ddot
     V_dot_V_r1_cpu=DDOT(N,CX,1,CY,1)
#else
     real(SP)::sdot
     V_dot_V_r1_cpu=SDOT(N,CX,1,CY,1)
#endif
     !
   end function V_dot_V_r1_cpu
   !
   real(SP) function V_dot_V_r2_cpu(N,CX,CY)
     implicit none
     integer, intent(in) :: N
     real(SP),target,intent(in) :: CX(:,:),CY(:,:)
     !
#if defined _DOUBLE
     real(SP)::ddot
     V_dot_V_r2_cpu=DDOT(N,CX,1,CY,1)
#else
     real(SP)::sdot
     V_dot_V_r2_cpu=SDOT(N,CX,1,CY,1)
#endif
     !
   end function V_dot_V_r2_cpu
   !
   real(SP) function V_dot_V_r1_gpu(N,CX,CY)
     implicit none
     integer,    intent(in) :: N
     real(SP) DEV_ATTR, intent(in) :: CX(:),CY(:)
     !
     V_dot_V_r1_gpu=devxlib_xDOT_gpu(N,CX,1,CY,1)
     !
   end function V_dot_V_r1_gpu
   !
   real(SP) function V_dot_V_r2_gpu(N,CX,CY)
     implicit none
     integer,    intent(in) :: N
     real(SP) DEV_ATTR, target, intent(in) :: CX(:,:),CY(:,:)
     real(SP), pointer DEV_ATTR :: CX_p(:), CY_p(:)
     !
#ifdef _CUDAF
     call c_f_pointer(c_devloc(CX),CX_p,[size(CX)])
     call c_f_pointer(c_devloc(CY),CY_p,[size(CY)])
#else
     call c_f_pointer(c_loc(CX),CX_p,[size(CX)])
     call c_f_pointer(c_loc(CY),CY_p,[size(CY)])
#endif
     V_dot_V_r2_gpu=devxlib_xDOT_gpu(N,CX_p,1,CY_p,1)
     !
   end function V_dot_V_r2_gpu
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
     complex(SP),target,intent(in) :: CX(:,:),CY(:,:)
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
     V_dot_V_c1_gpu=devxlib_xDOTU_gpu(N,CX,1,CY,1)
     !
   end function V_dot_V_c1_gpu
   !
   complex(SP) function V_dot_V_c2_gpu(N,CX,CY)
     implicit none
     integer,    intent(in) :: N
     complex(SP) DEV_ATTR, target, intent(in) :: CX(:,:),CY(:,:)
     complex(SP), pointer DEV_ATTR :: CX_p(:), CY_p(:)
     !
#ifdef _CUDAF
     call c_f_pointer(c_devloc(CX),CX_p,[size(CX)])
     call c_f_pointer(c_devloc(CY),CY_p,[size(CY)])
#else
     call c_f_pointer(c_loc(CX),CX_p,[size(CX)])
     call c_f_pointer(c_loc(CY),CY_p,[size(CY)])
#endif
     V_dot_V_c2_gpu=devxlib_xDOTU_gpu(N,CX_p,1,CY_p,1)
     !
   end function V_dot_V_c2_gpu
   !
   !==============
   ! DOT PRODUCTS: V_dot_VV
   !==============
   !
   complex(SP) function V_dot_VV_c1_cpu(N,CX,CY,CZ)
     implicit none
     integer,     intent(in) :: N
     complex(SP), intent(in) :: CX(:),CY(:),CZ(:)
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
     complex(SP) DEV_ATTR, intent(in) :: CX(:),CY(:),CZ(:)
     integer :: i
     complex(SP) :: ctmp
     !
     V_dot_VV_c1_gpu=0.0_SP
     ctmp=0.0_SP
     !
     !DEV_ACC_DEBUG data present(CX,CY,CZ)
     !DEV_ACC parallel loop
     !DEV_CUF kernel do(1) <<<*,*>>>
     !DEV_OMPGPU target map(present,alloc:CX,CY,CZ) map(tofrom:ctmp)
     !DEV_OMPGPU teams loop reduction(+:ctmp)
     do i = 1, N
       ctmp=ctmp+CX(i)*CY(i)*CZ(i)
     enddo
     !DEV_OMPGPU end target
     !DEV_ACC_DEBUG end data
     !
     V_dot_VV_c1_gpu=ctmp
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
     !$omp parallel do default(shared), private(i)
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
     !DEV_ACC_DEBUG data present(CX,CY,CZ)
     !DEV_ACC parallel loop
     !DEV_CUF kernel do(1) <<<*,*>>>
     !DEV_OMPGPU target map(present,alloc:CX,CY,CZ)
     !DEV_OMPGPU teams loop
     !DEV_OMP parallel do default(shared), private(i)
     do i=1,N
        CZ(i)=CX(i)*CY(i)
     enddo
     !DEV_OMPGPU end target
     !DEV_ACC_DEBUG end data
   end subroutine V_by_V_pwise_gpu
   !
end module
