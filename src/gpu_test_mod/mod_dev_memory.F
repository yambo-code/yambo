
#include <dev_defs.h>
#define __USE_DEVXLIB__

!====================
module dev_memory_m
  !====================
  use iso_c_binding
#if defined __USE_DEVXLIB__
  use deviceXlib_m
#endif
  use kinds,         only: SP
  implicit none
  private


#if ! defined __USE_DEVXLIB__
  !
  interface dev_memcpy
#ifdef _CUDAF
    module procedure :: dev_memcpy_h2d_cuf
    module procedure :: dev_memcpy_d2h_cuf
#endif
!#ifdef _OPENACC
!    module procedure :: dev_memcpy_h2d_acc
!    module procedure :: dev_memcpy_d2h_acc
!#endif
    module procedure :: dev_memcpy_h2h
  end interface
  !
  interface dev_memcpy_h2d
#if defined _CUDAF
    module procedure :: dev_memcpy_h2d_cuf
#elif defined _OPENACC
    module procedure :: dev_memcpy_h2d_acc
#elif defined _OPENMP5
    module procedure :: dev_memcpy_h2d_omp5
#else
    module procedure :: dev_memcpy_h2d_acc
#endif
    module procedure dev_memcpy_dummy
  end interface
  !
  interface dev_memcpy_d2h
#if defined _CUDAF
    module procedure :: dev_memcpy_d2h_cuf
#elif defined _OPENACC
    module procedure :: dev_memcpy_d2h_acc
#elif defined _OPENMP5
    module procedure :: dev_memcpy_d2h_omp5
#else
    module procedure :: dev_memcpy_d2h_acc
#endif
    module procedure dev_memcpy_dummy
  end interface
  !
#else
  !
  interface dev_memcpy_h2d
    module procedure dev_memcpy_dummy
  end interface
  interface dev_memcpy_d2h
    module procedure dev_memcpy_dummy
  end interface
  
#endif
  !
  interface dev_mem_alloc
    module procedure :: dev_mem_alloc_r1
  end interface
  !
  interface dev_mem_free
    module procedure :: dev_mem_free_r1
  end interface
  
  public :: dev_mem_alloc
  public :: dev_mem_free
  public :: dev_memcpy
  public :: dev_memcpy_h2d
  public :: dev_memcpy_d2h

contains

  !
  ! alloc & free
  !
  subroutine dev_mem_alloc_r1(var,N,ierr)
    implicit none
    real(SP), allocatable DEV_ATTR :: var(:)
    integer      :: N
    integer      :: ierr
    !
    ierr=0
#ifdef _CUDAF
    if (.not.allocated(var)) allocate(var(N),stat=ierr)
#endif
    !DEV_ACC enter data create(var(1:N))
    !DEV_OMP5 target enter data map(alloc:var(1:N))
    return
  end subroutine dev_mem_alloc_r1
  !
  subroutine dev_mem_free_r1(var,ierr)
    implicit none
    real(SP), allocatable DEV_ATTR :: var(:)
    integer      :: ierr
    !
    ierr=0
    !$acc exit data delete(var)
    !$omp target exit data map(delete:var)
    !
#ifdef _CUDAF
    if (allocated(var)) deallocate(var,stat=ierr)
#endif
    return
  end subroutine dev_mem_free_r1

#if ! defined __USE_DEVXLIB__
  !
  ! memcpy
  !
  subroutine dev_memcpy_h2h(x,y)
    implicit none
    real(SP) :: x(:),y(:)
    x(:)=y(:)
    return
  end subroutine
  !
  subroutine dev_memcpy_h2d_cuf(x_d,y)
    implicit none
    real(SP) DEV_ATTR :: x_d(:)
    real(SP) :: y(:)
    x_d(:)=y(:)
    return
  end subroutine
  subroutine dev_memcpy_d2h_cuf(x,y_d)
    implicit none
    real(SP) :: x(:)
    real(SP) DEV_ATTR :: y_d(:)
    x(:)=y_d(:)
    return
  end subroutine
  !
  subroutine dev_memcpy_h2d_acc(x_d,y)
    implicit none
    real(SP) :: x_d(:)
    real(SP) :: y(:)
    !
    x_d=y
    !$acc update device(x_d)
  end subroutine
  !
  subroutine dev_memcpy_d2h_acc(x,y_d)
    implicit none
    real(SP) :: x(:)
    real(SP) :: y_d(:)
    !
    !$acc update host(y_d)
    x=y_d
  end subroutine
  !
  subroutine dev_memcpy_h2d_omp5(x_d,y)
    implicit none
    real(SP) :: x_d(:)
    real(SP) :: y(:)
    !
    x_d=y
    !$omp target update to(x_d)
  end subroutine
  !
  subroutine dev_memcpy_d2h_omp5(x,y_d)
    implicit none
    real(SP) :: x(:)
    real(SP) :: y_d(:)
    !
    !$omp target update from(y_d)
    x=y_d
  end subroutine
  !
#endif
  !
  subroutine dev_memcpy_dummy()
     return
  end subroutine
  !
end module dev_memory_m

