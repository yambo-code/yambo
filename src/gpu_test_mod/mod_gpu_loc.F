
!=================
module gpu_loc_m
  !=================
  !
  use cusolverdn_y
  use devxlib_cublas
  implicit none
  private

  logical :: gpu_linalg_init=.false.

#if defined _CUDAF 
  logical :: have_device=.true.
  logical :: have_cudaf=.true.
  logical :: have_openacc=.false.
  logical :: have_ompgpu=.false.
#elif defined _OPENACC
  logical :: have_device=.true.
  logical :: have_cudaf=.false.
  logical :: have_openacc=.true.
  logical :: have_ompgpu=.false.
#elif defined _OPENMP_GPU
  logical :: have_device=.true.
  logical :: have_cudaf=.false.
  logical :: have_openacc=.false.
  logical :: have_ompgpu=.true.
#else
  logical :: have_device=.false.
  logical :: have_cudaf=.false.
  logical :: have_openacc=.false.
  logical :: have_ompgpu=.false.
#endif
  !
#if defined _GPU && defined _CUDA
  type(cuBlasHandle) :: cublas_h
  public :: cublas_h
#endif

  public :: have_device
  public :: have_cudaf
  public :: have_openacc
  public :: have_ompgpu
  public :: gpu_linalg_init
  public :: gpu_linalg_setup

contains

  subroutine gpu_linalg_setup()
    implicit none
#if defined _CUDAF || defined _OPENACC
    integer :: istat 
    istat = cublasInit()
    if(istat/=0) stop 'cublasInit failed'
    istat =  cublasCreate(cublas_h)
    if(istat/=0) stop 'creating cublas handle'
    !istat = cusolverDnCreate(cusolv_h)
    !if(istat/=CUSOLVER_STATUS_SUCCESS) call error('cusolverDnCreate failed')
    !
#endif
    gpu_linalg_init=.true.
  end subroutine

end module
