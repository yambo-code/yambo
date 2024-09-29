
#include <dev_defs.h>

!====================
module data_m
  !====================
  use iso_c_binding
  use dev_memory_m,    ONLY:dev_mem_alloc,dev_mem_free
  use pars,            ONLY:SP
  implicit none

type data_t 
  integer :: N
  real(SP), allocatable :: x(:)
  real(SP), allocatable :: y(:)
  real(SP), allocatable DEV_ATTR :: x_d(:)
  real(SP), allocatable DEV_ATTR :: y_d(:)
  !
  logical             :: alloc=.false.
  logical             :: alloc_gpu=.false.
end type

contains

  subroutine data_alloc(var,N,cpu_only)
    implicit none
    type(data_t) :: var
    integer      :: N
    logical, optional :: cpu_only
    !
    integer :: ierr
    logical :: cpu_only_

    cpu_only_=.false.
    if (present(cpu_only)) cpu_only_=cpu_only
    if (var%alloc) call data_free(var) 
    !
    var%alloc=.true.
    var%N=N
    allocate(var%x(N))
    allocate(var%y(N))
    !   
    if (cpu_only_) return
    !
    call dev_mem_alloc(DEV_VAR(var%x),var%N,ierr)
    call dev_mem_alloc(DEV_VAR(var%y),var%N,ierr)
    var%alloc_gpu=.true.
    !
    return
  end subroutine data_alloc
  !
  subroutine data_free(var)
    implicit none
    type(data_t) :: var
    integer :: ierr
    !
    if (.not.var%alloc) return
    !
    if (var%alloc_gpu) then
      call dev_mem_free(DEV_VAR(var%x),ierr)
      call dev_mem_free(DEV_VAR(var%y),ierr)
      var%alloc_gpu=.false.
    endif
    !
    if (allocated(var%x)) deallocate(var%x)
    if (allocated(var%y)) deallocate(var%y)
    var%alloc=.false.
    ! 
  end subroutine data_free

end module data_m

