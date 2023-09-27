!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP
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
module vlocal
   use kinds, only : DP
   real(DP), allocatable    :: vloc(:,:)
   ! vloc(ngl,ntyp) from QE [only needed by p2y]
   real(DP), allocatable    :: vloc_yambo(:,:)
   ! vloc_yambo(ngm,ntyp) stored in ns.vloc_pwscf by p2y
   complex(DP), allocatable :: vloc_full(:)
   ! vloc_full(ngm) including structure factors
   complex(DP), allocatable :: strf(:,:)
   ! srtf(ngm,ntyp) structure factors

   contains

     subroutine vloc_alloc(do_vloc)
       !
       ! Allocate Vlocal-related quantities for yambo
       !
       ! NB: p2y allocates vloc_yambo and strf independently
       !
       use gvect,      ONLY: ngm
       use ions_base,  ONLY: nsp
       !
       logical, intent(in), optional  :: do_vloc
       !
       ! Allocate vloc_yambo if reconstructing bare Hamiltonian...
       !
       if (present(do_vloc)) then
         !
         if (.not.allocated(vloc_yambo)) allocate(vloc_yambo(ngm,nsp))
         !
         if (.not.allocated(vloc_full)) allocate(vloc_full(ngm)) 
         !
       endif
       !
       ! ... otherwise just allocate structure factors
       !
       if (.not.allocated(strf)) allocate(strf(ngm,nsp))
       !
     end subroutine vloc_alloc
     !
     subroutine vloc_dealloc()
       !
       ! Dellocate Vlocal-related quantities
       !
       if (allocated(vloc_yambo)) deallocate(vloc_yambo) 
       if (allocated(vloc_full))  deallocate(vloc_full)
       if (allocated(strf))       deallocate(strf)
       !
     end subroutine vloc_dealloc
     !
end module
