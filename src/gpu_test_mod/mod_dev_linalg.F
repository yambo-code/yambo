
#include <dev_defs.h>

!====================
module dev_linalg_m
  !====================
  use iso_c_binding
  use gpu_m
  use pars,   ONLY:SP
  !
  use devxlib_cublas
  !
  implicit none

contains

  subroutine saxpy_cpu(N, a, x, y) 
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
  subroutine saxpy_gpu(N, a, x, y) 
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
    do i = 1, N
      y(i) = y(i) + a * x(i)
    end do
    !DEV_OMPGPU end target 
    !DEV_ACC_DEBUG end data
    !
  end subroutine saxpy_gpu
  !
  !
  subroutine dev_saxpy(N,a,x,inc_x,y,inc_y)
  implicit none
    integer  :: N, inc_x, inc_y
    real(SP) :: a
    real(SP)  DEV_ATTR :: x(N), y(N)
    integer  :: istat
    !
#if defined _CUDAF || defined _OPENACC || defined _OPENMP_GPU
    !DEV_OMPGPU target data map(tofrom:x,y), use_device_ptr(x,y)
    !DEV_ACC_DEBUG data present(x,y)
    !DEV_ACC host_data use_device(x,y)
#if defined _CUDA
    call cublasSaxpy(N, a, x, inc_x, y, inc_y)
#elif defined _HIP
!    call hipblasSaxpy(N, a, x, inc_x, y, inc_y)
#endif
    !DEV_ACC end host_data
    !DEV_ACC_DEBUG end data
    !DEV_OMPGPU end target data
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

