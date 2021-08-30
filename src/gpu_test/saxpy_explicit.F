
#include <dev_defs.h>

!================================
subroutine saxpy_cpu(N, a, x, y) 
  !================================
  use iso_c_binding
  implicit none
    integer  :: N
    real(c_float) :: a, x(N), y(N)
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
  implicit none
    integer  :: N
    real(c_float) :: a
    real(c_float) DEV_ATTR :: x(N), y(N)
    integer i
    !
    !DEV_CUF kernel do
    !DEV_ACC data present(x,y)
    !DEV_ACC parallel loop
    !DEV_OMP5 target map(tofrom:x,y)
    !DEV_OMP5 parallel do
    !DEV_OMP  parallel do default(shared), private (i)
    do i = 1, N
      y(i) = y(i) + a * x(i)
    end do
    !DEV_OMP5 end target 
    !DEV_ACC end data
    !
  end subroutine saxpy_explicit
  
