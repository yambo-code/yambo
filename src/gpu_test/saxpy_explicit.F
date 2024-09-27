
#include <dev_defs.h>

!================================
subroutine saxpy_cpu(N, a, x, y) 
  !================================
  use iso_c_binding
  use pars,   ONLY:SP
  implicit none
    integer  :: N
    real(SP) :: a, x(N), y(N)
    integer i
    !$omp parallel do default(shared), private(i)
    do i = 1, N
      y(i) = y(i) + a * x(i)
    end do
    !$omp end parallel do
    !
  end subroutine saxpy_cpu
  !
!================================
subroutine saxpy_explicit(N, a, x, y) 
  !================================
  use iso_c_binding
  use pars,   ONLY:SP
  implicit none
    integer  :: N
    real(SP) :: a
    real(SP) DEV_ATTR :: x(N), y(N)
    integer i
    !
    !DEV_CUF kernel do
    !DEV_ACC_DEBUG data present(x,y)
    !DEV_ACC parallel loop
    !DEV_OMPGPU target map(tofrom:x,y)
    !DEV_OMPGPU parallel do
    !DEV_OMP  parallel do default(shared), private (i)
    do i = 1, N
      y(i) = y(i) + a * x(i)
    end do
    !DEV_OMPGPU end target 
    !DEV_ACC_DEBUG end data
    !
  end subroutine saxpy_explicit
  
