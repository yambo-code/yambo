! Contains complete and partial routines from the code
! Self-energies and eXcitations (SaX)
! Copyright (C) 2006 SaX developers team
! Hacked by C. Hogan (2010)
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
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
! USA.

module sax_module
use pw_pseudo_module

!use pw_common_module NO
integer, parameter :: pw_basis_int_kind = selected_int_kind(3)
public :: pw_atoms,pw_atoms_init,pw_atoms_destroy,pw_atoms_read,pw_atoms_write
public :: pw_struct, pw_struct_set
public :: pw_atoms_set_dim
public :: pw_basis_set_struct, pw_basis_destroy, pw_basis_init

type pw_struct
  real a(3,3), b(3,3), a_omega, b_omega
end type pw_struct

type pw_atoms
  type (pw_struct), pointer :: struct
  integer                   :: natoms,ntypes
  type (pw_pseudo), pointer :: pseudo(:)
  character(100),   pointer :: names(:)
  real,             pointer :: positions(:,:)
  integer,          pointer :: type_map(:)
  real                      :: cutoff_vloc
end type pw_atoms

! This object contains the informations about the periodic cell
! a(:,1), a(:,2) and a(:,3) :: basis vectors (real space)
! b(:,1), b(:,2) and b(:,3) :: basis vectors (reciprocal space)
! a_omega                   :: cell volume (real space)
! b_omega                   :: cell volume (reciprocal space)
! All quantities in atomic units
!   a       [a.u.]
!   b       [a.u.^-1]
!   a_omega [a.u.^3]
!   b_omega [a.u.^-3]

type pw_basis
  type(pw_struct),                 pointer :: struct
  integer                                  :: npw
  real                                     :: r0(3),k(3)
  logical                                  :: conjg
  integer(kind=pw_basis_int_kind), pointer :: g(:,:)
  integer                                  :: gmax(3),gmin(3)
  integer                                  :: index_Gzero
end type pw_basis
! This object contains a plane-waves grid, a pointer to the
!   pw_struct object with lattice parameters
! struct   :: pointer to the struct object
! npw      :: number of plane waves
! r0(3)    :: origin of the grid
! k(3)     :: k vector, in unit of struct%b
!             NOTE this is the effective vector, eventually including the time-reversal
! conjg    :: a flag for time-reversal. if set to true, the Fourier coefficients
!             should be conjugated in reciprocal space.
! CHECK IF EVERYWHERE conjg IS CONSISTENT!!!
! g(3,npw) :: grid, in unit of struct%b
! gmax,gmin :: private utilities; calculated as
!                                gmax(1) = maxval(g(1,:)) ; gmax(2) = maxval(g(2,:)) gmax(3) = maxval(g(3,:))
!                                gmin(1) = minval(g(1,:)) ; gmin(2) = minval(g(2,:)) gmin(3) = minval(g(3,:))
interface pw_basis_init
  module procedure pw_basis_init00
  module procedure pw_basis_init10
end interface
interface pw_basis_destroy
  module procedure pw_basis_destroy0
  module procedure pw_basis_destroy1
end interface

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
  allocate(atoms%names(0))
  allocate(atoms%positions(0,0))
  allocate(atoms%type_map(0))
end subroutine pw_atoms_init

subroutine pw_atoms_destroy(atoms)
  type (pw_atoms), intent(inout) :: atoms
  call pw_pseudo_destroy(atoms%pseudo)
  deallocate(atoms%pseudo)
  deallocate(atoms%names)
  deallocate(atoms%positions)
  deallocate(atoms%type_map)
end subroutine pw_atoms_destroy
!
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
  allocate(atoms%names(ntypes))
  atoms%names = " "
  allocate(atoms%positions(3,natoms))
  allocate(atoms%type_map(natoms))
  call pw_pseudo_init(atoms%pseudo)
  atoms%positions = 0.0
  atoms%type_map = 0
end subroutine pw_atoms_set_dim

subroutine pw_struct_set(struct,a)
! Subroutine for struct creation.
! It takes as an argument the a matrix and sets up
! all quantities in struct.
! use num_module, only :: num_determinant, num_inverse
  type(pw_struct), intent(out) :: struct
  real,            intent(in)  :: a(3,3)
  real :: tmp(3,3)
! a(:,1), a(:,2) e a(:,3) :: basis vectors (real space, a.u.)
  struct%a = a
! struct%a_omega = num_determinant(struct%a)
  if(abs(struct%a_omega)<1e-8) call errore("pw_struct_set","a_omega zero",1)
! tmp = num_inverse(struct%a)
  struct%b = num_2pi * transpose(tmp)
! struct%b_omega = num_determinant(struct%b)
end subroutine pw_struct_set


subroutine pw_basis_init00(basis,struct)
! Initializes a basis object
  type(pw_basis),                    intent(out) :: basis
  type(pw_struct), optional, intent(in)  :: struct
! struct :: structure to be pointed to. it can be omitted and assigned later
  basis%npw   = 0
  basis%r0    = 0.0
  basis%k     = 0.0
  basis%conjg = .false.
  basis%gmax  = 0
  basis%gmin  = 0
  allocate(basis%g(3,0))
  nullify(basis%struct)
  if(present(struct)) call pw_basis_set_struct(basis,struct)
end subroutine pw_basis_init00

subroutine pw_basis_init10(basis,struct)
! Initializes an array of basis objects, all af them pointing to the same struct
  type(pw_basis),                    intent(out) :: basis(:)
  type(pw_struct), optional, intent(in)  :: struct
! struct :: structure to be pointed to. it can be omitted and assigned later
  integer :: i
  do i=1,size(basis)
    call pw_basis_init(basis(i),struct)
  end do
end subroutine pw_basis_init10

subroutine pw_basis_destroy0(basis)
! Destroys an object
  type(pw_basis), intent(inout) :: basis
  if(associated(basis%g)) deallocate(basis%g)
end subroutine pw_basis_destroy0

subroutine pw_basis_destroy1(basis)
! Destroys an array of basis objects
  type(pw_basis), intent(inout) :: basis(:)
  integer :: i
  do i=1,size(basis)
    call pw_basis_destroy0(basis(i))
  end do
end subroutine pw_basis_destroy1

subroutine pw_basis_set_struct(basis,struct)
! Associates a struct to a basis
  type(pw_basis),          intent(inout) :: basis
  type(pw_struct), target, intent(in)    :: struct
  if(.not.associated(basis%g)) call errore("pw_basis_set_struct", &
&  "not associated",1)
  basis%struct => struct
end subroutine pw_basis_set_struct


end module sax_module
