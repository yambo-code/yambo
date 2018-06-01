!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
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
    allocate(ityp(nsp))
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

