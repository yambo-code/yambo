!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AF
!
module qe_pseudo_m
  use parameters,      only : ntypx
  use kinds,           only : DP
  use control_flags
  use io_global
  use io_files
  use lsda_mod
  use noncollin_module
  use spin_orb
  use ions_base
  use cell_base
  use cellmd
  use klist
  use wvfct
  use gvecw
  use gvect
  use us
  use uspp_param
  use uspp
  use pseudo_types
  use becmod
  use symm_base
  implicit none

contains

  subroutine qe_pseudo_allocate()
    implicit none
    integer :: i
    !
    allocate(upf(nsp))
    do i = 1, nsp
      call nullify_pseudo_upf(upf(i))
    enddo
    !
    allocate(ityp(nat))
    allocate(tau(3,nat))
    !
    call allocate_nlpot()
    ! 
  end subroutine

  subroutine qe_pseudo_deallocate()
    use atom,          only : rgrid, msh
    use radial_grids
    implicit none
    integer :: i
    !
    call us_deallocate()
    call deallocate_uspp()
    !
    if (allocated(upf)) then
      do i = 1, size(upf)
        call deallocate_pseudo_upf(upf(i))
      enddo
      deallocate(upf)
    endif
    !
    if (allocated(ityp)) deallocate(ityp)
    if (allocated(tau))  deallocate(tau)
    !
    if (allocated(rgrid)) then
      do i = 1, size(rgrid)
        call deallocate_radial_grid(rgrid(i))
      enddo
      deallocate(rgrid)
    endif
    if (allocated(msh)) deallocate(msh)
    !
    if (allocated(g))      deallocate(g)
    if (allocated(gg))     deallocate(gg)
    if (allocated(mill))   deallocate(mill)
    if (allocated(eigts1)) deallocate(eigts1)
    if (allocated(eigts2)) deallocate(eigts2)
    if (allocated(eigts3)) deallocate(eigts3)
    !
  end subroutine qe_pseudo_deallocate

end module

