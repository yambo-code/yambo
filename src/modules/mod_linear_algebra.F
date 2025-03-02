!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module linear_algebra
 !
 use iso_c_binding
 use iso_fortran_env,  ONLY: int64
 use pars,             ONLY: SP,schlen
 !
#ifdef _MAGMA
 use magma,            ONLY: magmaf_init,magmaf_queue_create,&
&                            magmaf_cgeev_m,magmaf_zgeev_m,MagmaVec
#endif
 !
#include<dev_defs.h>
#include<y_memory.h>
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
 ! magma vars
 !
 integer(int64)     :: magma_queue    !! magma_queue_t
 logical            :: magma_init_done = .false.
#if defined _MAGMA
 logical, parameter :: have_magma=.true.
#else
 logical, parameter :: have_magma=.false.
#endif
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
   real(SP),    allocatable DEV_ATTR :: v_real_d(:)
   real(SP),    allocatable DEV_ATTR :: vp_real_d(:)
   complex(SP), allocatable DEV_ATTR :: v_cmplx_d(:)
   complex(SP), allocatable DEV_ATTR :: vp_cmplx_d(:)
   complex(SP), allocatable DEV_ATTR :: m1_cmplx_d(:,:)
   complex(SP), allocatable DEV_ATTR :: m2_cmplx_d(:,:)
   integer    , allocatable DEV_ATTR :: v_int_d(:)
   integer    , allocatable DEV_ATTR :: vp_int_d(:)
   !
 end type LALGEBRA_WS

 !
 !===============
 ! INTERFACES
 !===============
 !
 ! Eigensolvers
 !
 interface la_xgeev
   subroutine zgeev(jobvl,jobvr,n,A,lda,w,vl,ldvl,vr,ldvr,work,lwork,rwork,info)
     use iso_fortran_env, ONLY : WP => real64
     character(1) :: jobvl,jobvr 
     integer      :: n,lda,ldvl,ldvr,lwork,info
     complex(WP)  :: w(*), A(lda,*), vl(ldvl,*), vr(ldvr,*), work(*)
     real(WP)     :: rwork(*)
   end subroutine
   subroutine cgeev(jobvl,jobvr,n,A,lda,w,vl,ldvl,vr,ldvr,work,lwork,rwork,info)
     use iso_fortran_env, ONLY : WP => real32
     character(1) :: jobvl,jobvr 
     integer      :: n,lda,ldvl,ldvr,lwork,info
     complex(WP)  :: w(*), A(lda,*), vl(ldvl,*), vr(ldvr,*), work(*)
     real(WP)     :: rwork(*)
   end subroutine
 end interface
 !
 interface la_xheev
   subroutine zheev(jobz,uplo,n,A,lda,w,work,lwork,rwork,info)
     use iso_fortran_env, ONLY : WP => real64
     character(1) :: jobz,uplo
     integer      :: n,lda,lwork,info
     complex(WP)  :: A(lda,*), work(*)
     real(WP)     :: w(*), rwork(*)
   end subroutine
   subroutine cheev(jobz,uplo,n,A,lda,w,work,lwork,rwork,info)
     use iso_fortran_env, ONLY : WP => real32
     character(1) :: jobz,uplo
     integer      :: n,lda,lwork,info
     complex(WP)  :: A(lda,*), work(*)
     real(WP)     :: w(*), rwork(*)
   end subroutine
 end interface
 !
#ifdef _MAGMA
 !
 interface magmaf_xgeev_m
   procedure :: magmaf_cgeev_m,magmaf_zgeev_m
 end interface
 !
 interface magmaf_xheevd_m
   procedure :: magmaf_cheevd_m,magmaf_zheevd_m
 end interface
 !
#endif
 ! 
 ! Singular Value Decomposition
 !
 interface la_xgesvd
   subroutine zgesvd(jobu,jobvt,m,n,A,lda,s,U,ldu,VT,ldvt,work,lwork,rwork,info)
     use iso_fortran_env, ONLY : WP => real64
     character(1) :: jobu,jobvt
     integer      :: m,n,lda,ldu,ldvt,lwork,info
     complex(WP)  :: A(lda,*), U(ldu,*), VT(ldvt,*), work(*)
     real(WP)     :: s(*), rwork(*)
   end subroutine
   subroutine cgesvd(jobu,jobvt,m,n,A,lda,s,U,ldu,VT,ldvt,work,lwork,rwork,info)
     use iso_fortran_env, ONLY : WP => real32
     character(1) :: jobu,jobvt
     integer      :: m,n,lda,ldu,ldvt,lwork,info
     complex(WP)  :: A(lda,*), U(ldu,*), VT(ldvt,*), work(*)
     real(WP)     :: s(*), rwork(*)
   end subroutine
 end interface
 !
 ! Lower/Upper Factorization and Inverison
 !
 interface la_xgetrf
   subroutine zgetrf(m,n,A,lda,ipiv,info)
     use iso_fortran_env, ONLY : WP => real64
     integer      :: m,n,lda,info
     integer      :: ipiv(*)
     complex(WP)  :: A(lda,*) 
   end subroutine
   subroutine cgetrf(m,n,A,lda,ipiv,info)
     use iso_fortran_env, ONLY : WP => real32
     integer      :: m,n,lda,info
     integer      :: ipiv(*)
     complex(WP)  :: A(lda,*) 
   end subroutine
 end interface
 !
 interface la_xgetri
   subroutine zgetri(n,A,lda,ipiv,work,lwork,info)
     use iso_fortran_env, ONLY : WP => real64
     integer      :: n,lda,info,lwork
     integer      :: ipiv(*)
     complex(WP)  :: A(lda,*), work(*)
   end subroutine
   subroutine cgetri(n,A,lda,ipiv,work,lwork,info)
     use iso_fortran_env, ONLY : WP => real32
     integer      :: n,lda,info,lwork
     integer      :: ipiv(*)
     complex(WP)  :: A(lda,*), work(*)
   end subroutine
 end interface
 !
 interface la_xgetrs
   subroutine zgetrs(trans,n,nrhs,A,lda,ipiv,B,ldb,info)
     use iso_fortran_env, ONLY : WP => real64
     character(1) :: trans
     integer      :: n,nrhs,lda,ldb,info
     integer      :: ipiv(*)
     complex(WP)  :: A(lda,*), B(ldb,*)
   end subroutine
   subroutine cgetrs(trans,n,nrhs,A,lda,ipiv,B,ldb,info)
     use iso_fortran_env, ONLY : WP => real32
     character(1) :: trans
     integer      :: n,nrhs,lda,ldb,info
     integer      :: ipiv(*)
     complex(WP)  :: A(lda,*), B(ldb,*)
   end subroutine
 end interface


 !
 !====================
 ! PUBLIC STATEMENTS
 !====================
 !
 public :: la_xgeev, la_xheev
#ifdef _MAGMA
 public :: magmaf_xgeev_m,magmaf_xheevd_m
#endif
 public :: la_xgesvd
 public :: la_xgetrf, la_xgetri, la_xgetrs
 !
 !
 contains
   !
   !============================
   ! AUXILIARY FUNCTIONS
   !============================
   !
   subroutine magma_setup()
#ifdef _MAGMA
     call magmaf_init()
     call magmaf_queue_create( 0, magma_queue )
     magma_init_done=.true.
#endif
     return
   end subroutine
   !
   subroutine LINEAR_ALGEBRA_WS_reset(WS)
     use drivers,   ONLY:l_nl_optics,l_real_time
     type(LALGEBRA_WS) :: WS
     !
     if(.not.(l_nl_optics.or.l_real_time)) then
       YAMBO_FREE_GPU(DEV_VAR(WS%v_real))
       YAMBO_FREE_GPU(DEV_VAR(WS%vp_real))
       YAMBO_FREE_GPU(DEV_VAR(WS%v_cmplx))
       YAMBO_FREE_GPU(DEV_VAR(WS%vp_cmplx))
       YAMBO_FREE_GPU(DEV_VAR(WS%m1_cmplx))
       YAMBO_FREE_GPU(DEV_VAR(WS%m2_cmplx))
       YAMBO_FREE_GPU(DEV_VAR(WS%v_int))
       YAMBO_FREE_GPU(DEV_VAR(WS%vp_int))
       !
       YAMBO_FREE(WS%v_real)
       YAMBO_FREE(WS%vp_real)
       YAMBO_FREE(WS%v_cmplx)
       YAMBO_FREE(WS%vp_cmplx)
       YAMBO_FREE(WS%m1_cmplx)
       YAMBO_FREE(WS%m2_cmplx)
       YAMBO_FREE(WS%vp_int)
       YAMBO_FREE(WS%v_int)
     else
#ifdef _CUDAF
       if(allocated(WS%v_real_d))   deallocate(WS%v_real_d)
       if(allocated(WS%vp_real_d))  deallocate(WS%vp_real_d)
       if(allocated(WS%v_cmplx_d))  deallocate(WS%v_cmplx_d)
       if(allocated(WS%vp_cmplx_d)) deallocate(WS%vp_cmplx_d)
       if(allocated(WS%m1_cmplx_d)) deallocate(WS%m1_cmplx_d)
       if(allocated(WS%m2_cmplx_d)) deallocate(WS%m2_cmplx_d)
       if(allocated(WS%vp_int_d))   deallocate(WS%vp_int_d)
       if(allocated(WS%v_int_d))    deallocate(WS%v_int_d)
#endif
#ifdef _OPENACC
       ! AF: This is only for NL or RT runs
       !     commented here for simplicity
       !
       !call error("[GPU] openacc FREE not implemented in LinAlg_WS_reset")
#endif
       !
       if(allocated(WS%v_real))   deallocate(WS%v_real)
       if(allocated(WS%vp_real))  deallocate(WS%vp_real)
       if(allocated(WS%v_cmplx))  deallocate(WS%v_cmplx)
       if(allocated(WS%vp_cmplx)) deallocate(WS%vp_cmplx)
       if(allocated(WS%m1_cmplx)) deallocate(WS%m1_cmplx)
       if(allocated(WS%m2_cmplx)) deallocate(WS%m2_cmplx)
       if(allocated(WS%vp_int))   deallocate(WS%vp_int)
       if(allocated(WS%v_int))    deallocate(WS%v_int)
       !
     endif
     !
   end subroutine
   !
   subroutine LINEAR_ALGEBRA_error(calling_subr,message_)
     use stderr,  ONLY:STRING_pack
     character(*) calling_subr,message_
     call error( trim( STRING_pack('LINEAR ALGEBRA driver [',trim(calling_subr),'] ',trim(message_)) ))
   end subroutine
   !
end module linear_algebra

