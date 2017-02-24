! Self-energies and eXcitations (SaX)
! Copyright (C) 2006 SaX developers team
! 
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

!@ MANUAL
module pw_atoms_module
use pw_struct_module
use pw_pseudo_module
implicit none
private
public :: pw_atoms, &
          pw_atoms_init,pw_atoms_destroy,pw_atoms_set_dim

!@ END MANUAL


!@ MANUAL
type pw_atoms
  type (pw_struct), pointer :: struct
  integer                   :: natoms,ntypes
  type (pw_pseudo), pointer :: pseudo(:)
  character(100),   pointer :: names(:)
  real,             pointer :: positions(:,:)
  integer,          pointer :: type_map(:)
  real                      :: cutoff_vloc
end type pw_atoms
!@ END MANUAL

contains

subroutine pw_atoms_init(atoms,struct,cutoff_vloc)
  type (pw_atoms),          intent(out) ::atoms
  type (pw_struct), target, intent(in)  :: struct
  real, intent(in) :: cutoff_vloc
  atoms%struct => struct
  atoms%natoms = 0
  atoms%ntypes = 0
  atoms%cutoff_vloc = cutoff_vloc
  allocate(atoms%pseudo(0))
! allocate(atoms%names(0))
! allocate(atoms%positions(0,0))
! call pw_allocate(atoms%positions)
! allocate(atoms%type_map(0))
! call pw_allocate(atoms%type_map)
end subroutine pw_atoms_init

subroutine pw_atoms_destroy(atoms)
  type (pw_atoms), intent(inout) :: atoms
  call pw_pseudo_destroy(atoms%pseudo)
  deallocate(atoms%pseudo)
! deallocate(atoms%names)
! call pw_deallocate(atoms%positions)
! deallocate(atoms%positions)
! call pw_deallocate(atoms%type_map)
! deallocate(atoms%type_map)
end subroutine pw_atoms_destroy

subroutine pw_atoms_set_dim(atoms,natoms,ntypes)
  type (pw_atoms), intent(inout) :: atoms
  integer,         intent(in)    :: natoms,ntypes
  type (pw_struct), pointer :: struct
  struct => atoms%struct
  call pw_atoms_destroy(atoms)
  atoms%struct => struct
  atoms%natoms = natoms
  atoms%ntypes = ntypes
  allocate(atoms%pseudo(ntypes))
! allocate(atoms%names(ntypes))
! atoms%names = " "
! allocate(atoms%positions(3,natoms))
! call pw_allocate(atoms%positions)
! allocate(atoms%type_map(natoms))
! call pw_allocate(atoms%type_map)
  call pw_pseudo_init(atoms%pseudo)
! atoms%positions = 0.0
! atoms%type_map = 0
end subroutine pw_atoms_set_dim

end module pw_atoms_module

