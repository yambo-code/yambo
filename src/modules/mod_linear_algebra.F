!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module linear_algebra
 !
 use pars,       ONLY:SP,schlen
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
 integer, parameter :: DIAGX=10
 integer, parameter :: CHASE=11
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
       if(allocated(WS%v_real))   deallocate(WS%v_real)
       if(allocated(WS%vp_real))  deallocate(WS%vp_real)
       if(allocated(WS%v_cmplx))  deallocate(WS%v_cmplx)
       if(allocated(WS%vp_cmplx)) deallocate(WS%vp_cmplx)
       if(allocated(WS%m1_cmplx)) deallocate(WS%m1_cmplx)
       if(allocated(WS%m2_cmplx)) deallocate(WS%m2_cmplx)
       if(allocated(WS%vp_int))   deallocate(WS%vp_int)
       if(allocated(WS%v_int))    deallocate(WS%v_int)
       !
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
