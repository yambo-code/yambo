

#include <dev_defs.h>

subroutine GPU_test(N)
  use iso_c_binding
  use gpu_m,            ONLY: GPU_test_dim, gpu_devsync, have_gpu
  use devxlib,          ONLY: devxlib_memcpy_d2h, devxlib_memcpy_h2d, devxlib_xaxpy, devxlib_xaxpy_gpu
  use data_m,           ONLY: data_t, data_alloc, data_free
  use pars,             ONLY: SP
  implicit none

  integer, intent(in) :: N
  real(SP), parameter :: eps=1.0e-6
  !
  real(SP) :: a
  !
  ! prototypes of a standard (and simple) F datastructure
  !
  type(data_t) :: var_ref
  type(data_t) :: var_cpu
  type(data_t), target :: var_gpu
  real(SP), pointer DEV_ATTR :: var_x_p(:)
  real(SP), pointer DEV_ATTR :: var_y_p(:)
  !
  character(64) :: device_str
  real(c_double)  :: t0,t1,t_g0,t_g1
  real(c_double), external :: qe_cclock
  integer :: i

!================


  write(*,"(/,50('='))")
  write(*,*) "HEADER"
  write(*,"(50('='),/)")
  write(*,*) "Problem dimension: ", N
  !
  call startup()

  !
  !=========
  ! alloc & init
  !=========
  !
  t_g0=qe_cclock()
  !
  write(*,*)
  write(*,*) "Allocate vars"
  call data_alloc(var_ref,N,cpu_only=.true.)
  call data_alloc(var_cpu,N,cpu_only=.true.)
  call data_alloc(var_gpu,N)
  !
  write(*,*) "Init data"
  a = 2.0
  do i = 1, var_ref%N
     var_ref%x(i) = 4.0 * i
     var_ref%y(i) = 3.0
  end do
  !
  write(*,*) "After Init, a = ", a
  write(*,*) "var_ref%x"
  write(*,"(8f15.9)") var_ref%x(1:16)
  write(*,*) "var_ref%y"
  write(*,"(8f15.9)") var_ref%y(1:16)
  
  !
  !=======
  ! CPU
  !=======
  !
  write(*,"(/,50('='))")
  write(*,*) "Running on CPU"
  write(*,"(50('='),/)")
  var_cpu%x=var_ref%x
  var_cpu%y=var_ref%y
  !
  t0=qe_cclock()
  !
  !----------
  ! prototype of a standard explicit loop to be computed on CPU
  !----------
  !
  call SAXPY(var_cpu%N, a, var_cpu%x, var_cpu%y)
  t1=qe_cclock()
  write(*,*) "Timing (CPU): ", t1-t0
  write(*,*) "Results CPU"
  write(*,"(8f15.9)") var_cpu%y(1:16)
  
  !
  !=======
  ! Explicit loop
  !=======
  ! prototype of a standard explicit loop to be computed on GPUs
  !
  device_str="GPU"
  if (.not. have_gpu) device_str="CPU"
  !
  write(*,"(/,50('='))")
  write(*,*) "Running "//trim(device_str)//"_EXPL"
  write(*,"(50('='),/)")
  var_gpu%x=var_ref%x
  var_gpu%y=var_ref%y
  !
  var_x_p=> DEV_VAR(var_gpu%x)
  var_y_p=> DEV_VAR(var_gpu%y)

  !
  ! data transfer host2dev
  !
  write(*,*) "Memcpy host2dev",gpu_devsync()
  call devxlib_memcpy_h2d(var_x_p,var_gpu%x)
  call devxlib_memcpy_h2d(var_y_p,var_gpu%y)
  !
  ! running on GPUs
  !
  write(*,*) "Compute EXPLICIT",gpu_devsync()
  t0=qe_cclock()
  !
  write(*,*) "saxpy EXPLICIT",gpu_devsync()
  call saxpy_explicit(var_gpu%N, a, var_x_p, var_y_p)
  write(*,*) "saxpy EXPLICIT done",gpu_devsync()
  !
  t1=qe_cclock()
  write(*,*) "Timing ("//trim(device_str)//"_EXPL): ", t1-t0,gpu_devsync()
  !
  ! data transfer dev2host
  !
  call devxlib_memcpy_d2h(var_gpu%y,var_y_p)
  !
  ! Check
  !
  write(*,*) "Results EXPLICIT",gpu_devsync()
  write(*,"(8f15.9)") var_gpu%y(1:16)
  !
  if ( any( abs(var_gpu%y(:)-var_cpu%y(:))>eps ) ) then
     write(*,*) "Test EXPLICIT: FAILED"
  else
     write(*,*) "Test EXPLICIT: OK"
  endif

  !
  !=======
  ! GPU (using CUBLAS)
  !=======
  ! prototype of a standard call to BLAS/CUBLAS libs
  !
  write(*,"(/,50('='))")
  write(*,*) "Running "//trim(device_str)//"_CUBLAS"
  write(*,"(50('='),/)")
  !
  var_gpu%x=var_ref%x
  var_gpu%y=var_ref%y
  !
  var_x_p=> DEV_VAR(var_gpu%x)
  var_y_p=> DEV_VAR(var_gpu%y)

  !
  ! data transfer host2dev
  !
  write(*,*) "Memcpy host2dev"
  call devxlib_memcpy_h2d(var_x_p,var_gpu%x)
  call devxlib_memcpy_h2d(var_y_p,var_gpu%y)
  !
  ! running on GPUs
  !
  write(*,*) "Compute with BLAS/CUBLAS"
  t0=qe_cclock()
  !
  call devxlib_xaxpy_gpu(var_gpu%N, a, var_x_p, 1, var_y_p, 1)
  !
  t1=qe_cclock()
  write(*,*) "Timing ("//trim(device_str)//"_CUBLAS): ", t1-t0
  !
  ! data transfer dev2host
  !
  call devxlib_memcpy_d2h(var_gpu%y,var_y_p)
  !
  ! Check
  !
  write(*,*) "Results BLAS/CUBLAS"
  write(*,"(8f15.9)") var_gpu%y(1:16)
  !
  if ( any( abs(var_gpu%y(:)-var_cpu%y(:))>eps ) ) then
     write(*,*) "Test BLAS/CUBLAS: FAILED"
  else
     write(*,*) "Test BLAS/CUBLAS: OK"
  endif

  !
  !==========
  ! FINALIZE
  !==========
  !
  t_g1=qe_cclock()
  write(*,"(/,50('='))")
  write(*,*) "FINALIZE"
  write(*,"(50('='),/)")
  write(*,*) "Timing (global): ", t_g1-t_g0
  !
  ! cleanup
  !
  call data_free(var_ref)
  call data_free(var_cpu)
  call data_free(var_gpu)

end subroutine GPU_test

