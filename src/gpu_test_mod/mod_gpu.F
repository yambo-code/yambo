
!=================
module gpu_m
  !=================
  !
  use cublas_m
  use cublas_core_m
  implicit none
  private

  logical :: cuda_linalg_init=.false.

#if defined _CUDAF 
  logical :: have_device=.true.
  logical :: have_cudaf=.true.
  logical :: have_openacc=.false.
  logical :: have_openmp5=.false.
#elif defined _OPENACC
  logical :: have_device=.true.
  logical :: have_cudaf=.false.
  logical :: have_openacc=.true.
  logical :: have_openmp5=.false.
#elif defined _OPENMP5
  logical :: have_device=.true.
  logical :: have_cudaf=.false.
  logical :: have_openacc=.false.
  logical :: have_openmp5=.true.
#else
  logical :: have_device=.false.
  logical :: have_cudaf=.false.
  logical :: have_openacc=.false.
  logical :: have_openmp5=.false.
#endif
  !
  type(cuBlasHandle) :: cublas_h

  public :: have_device
  public :: have_cudaf
  public :: have_openacc
  public :: have_openmp5
  public :: cuda_linalg_init
  public :: cuda_linalg_setup
  public :: cublas_h

contains

  subroutine cuda_linalg_setup()
    implicit none
#ifdef _CUDA
    integer :: istat 
    istat = cublasInit()
    if(istat/=0) stop 'cublasInit failed'
    istat =  cublasCreate(cublas_h)
    if(istat/=0) stop 'creating cublas handle'
    !istat = cusolverDnCreate(cusolv_h)
    !if(istat/=CUSOLVER_STATUS_SUCCESS) call error('cusolverDnCreate failed')
    !
#endif
    cuda_linalg_init=.true.
  end subroutine

end module
