
#include <dev_defs.h>

!====================
module dev_linalg_m
  !====================
  use iso_c_binding
  use gpu_m
  !
#if defined _CUDAF
  use cublas
#endif
#if defined _OPENACC || defined _OPENMP5
  use cublas_m
#endif
  !
  implicit none

contains

  subroutine saxpy_cpu(N, a, x, y) 
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
  subroutine saxpy_gpu(N, a, x, y) 
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
    do i = 1, N
      y(i) = y(i) + a * x(i)
    end do
    !DEV_OMP5 end target 
    !DEV_ACC end data
    !
  end subroutine saxpy_gpu
  !
  !
  subroutine dev_saxpy(N,a,x,inc_x,y,inc_y)
  implicit none
    integer  :: N, inc_x, inc_y
    real(c_float) :: a
    real(c_float)  DEV_ATTR :: x(N), y(N)
    integer  :: istat
    !
#if defined _CUDAF || defined _OPENACC || defined _OPENMP5
    !DEV_OMP5 target data map(tofrom:x,y), use_device_ptr(x,y)
    !DEV_ACC data present(x,y)
    !DEV_ACC host_data use_device(x,y)
    call cublasSaxpy(N, a, x, inc_x, y, inc_y)
    !DEV_ACC end host_data
    !DEV_ACC end data
    !DEV_OMP5 end target data
#else
    call saxpy(N, a, x, inc_x, y, inc_y)
#endif
    !
    ! alternative interface
    !
    !istat = cublasSaxpy(cublas_h, N, a, x, inc_x, y, inc_y)
  end subroutine
  !
end module dev_linalg_m

