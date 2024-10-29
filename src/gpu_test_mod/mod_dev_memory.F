

!====================
module dev_memory_m
  !====================
  use iso_c_binding
  use pars,   ONLY:SP
  implicit none
  private

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
    !DEV_ACC exit data delete(var)
    !DEV_OMPGPU target exit data map(delete:var)
    !
#ifdef _CUDAF
    if (allocated(var)) deallocate(var,stat=ierr)
#endif
    return
  end subroutine dev_mem_free_r1

end module dev_memory_m
