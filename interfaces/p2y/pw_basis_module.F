!@ MANUAL
module pw_basis_module
use pw_struct_module
implicit none
private
public :: pw_basis_int_kind,   &
          pw_basis,            &
          pw_basis_init,       &
          pw_basis_destroy,    &
          pw_basis_set_struct, &
          pw_basis_set_npw,    &
          assignment(=),       &
          operator(==)

integer, parameter :: pw_basis_int_kind = selected_int_kind(3)
!@ END MANUAL

!@ MANUAL
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
!@ END MANUAL

!@ MANUAL
interface operator(==)
  module procedure pw_basis_is_equal
end interface
interface assignment(=)
  module procedure pw_basis_copy
end interface
interface pw_basis_init
  module procedure pw_basis_init00
  module procedure pw_basis_init10
end interface
interface pw_basis_destroy
  module procedure pw_basis_destroy0
  module procedure pw_basis_destroy1
end interface
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_basis_init00(basis,struct)
! Initializes a basis object
  type(pw_basis),                    intent(out) :: basis
  type(pw_struct), optional, intent(in)  :: struct
! struct :: structure to be pointed to. it can be omitted and assigned later
!@ END MANUAL
  basis%npw   = 0
  basis%r0    = 0.0
  basis%k     = 0.0
  basis%conjg = .false.
  basis%gmax  = 0
  basis%gmin  = 0
  allocate(basis%g(3,0))
! call pw_allocate(basis%g)
  nullify(basis%struct)
  if(present(struct)) call pw_basis_set_struct(basis,struct)
end subroutine pw_basis_init00

!@ MANUAL
subroutine pw_basis_init10(basis,struct)
! Initializes an array of basis objects, all af them pointing to the same struct
  type(pw_basis),                    intent(out) :: basis(:)
  type(pw_struct), optional, intent(in)  :: struct
! struct :: structure to be pointed to. it can be omitted and assigned later
!@ END MANUAL
  integer :: i
  do i=1,size(basis)
    call pw_basis_init(basis(i),struct)
  end do
end subroutine pw_basis_init10

!@ MANUAL
subroutine pw_basis_destroy0(basis)
! Destroys an object
  type(pw_basis), intent(inout) :: basis
!@ END MANUAL
!  if(.not.associated(basis%g)) ERROR("")
! call pw_deallocate(basis%g)
  if(associated(basis%g)) deallocate(basis%g)
end subroutine pw_basis_destroy0

!@ MANUAL
subroutine pw_basis_destroy1(basis)
! Destroys an array of basis objects
  type(pw_basis), intent(inout) :: basis(:)
!@ END MANUAL
  integer :: i
  do i=1,size(basis)
    call pw_basis_destroy0(basis(i))
  end do
end subroutine pw_basis_destroy1

!@ MANUAL
subroutine pw_basis_set_struct(basis,struct)
! Associates a struct to a basis
  type(pw_basis),          intent(inout) :: basis
  type(pw_struct), target, intent(in)    :: struct
!@ END MANUAL
! if(.not.associated(basis%g)) ERROR("")
  if(.not.associated(basis%g)) call errore("pw_basis_set_struct", &
&  "not associated",1)
  basis%struct => struct
end subroutine pw_basis_set_struct

!@ MANUAL
subroutine pw_basis_set_npw(basis,npw)
! Sets the number of plane waves and evetually reallocates the grid
  type(pw_basis), intent(inout) :: basis
  integer,        intent(in)    :: npw
!@ END MANUAL
! if(.not.associated(basis%g)) ERROR("")
! if(.not.associated(basis%struct)) ERROR("")
  if(.not.associated(basis%g)) call errore("pw_basis_set_npw","not.assoc basis%g",1)
  if(.not.associated(basis%struct)) call errore("pw_basis_set_npw","not.assoc basis%struct",1)
  if(basis%npw /= npw) then
    basis%npw = npw
!   call pw_deallocate(basis%g)
    deallocate(basis%g)
    allocate(basis%g(3,npw))
!   call pw_allocate(basis%g)
  end if
  basis%g = 0
end subroutine pw_basis_set_npw

function pw_basis_is_equal(basis1,basis2)
! Compares two objects
! Interfaced with operator(==)
  logical pw_basis_is_equal
  type(pw_basis), intent(in) :: basis1,basis2
!@ END MANUAL
  if(.not.associated(basis1%g)) call errore("pw_basis_is_equal","not.assoc 1",1)
  if(.not.associated(basis1%struct)) call errore("pw_basis_is_equal","not.assoc 2",1)
  if(.not.associated(basis2%g)) call errore("pw_basis_is_equal","not.assoc 3",1)
  if(.not.associated(basis2%struct)) call errore("pw_basis_is_equal","not.assoc 4",1)
! Shortcut if the two basis are the same one
  if(associated(basis1%g,basis2%g)) then
    pw_basis_is_equal=.true.
    return
  end if
  pw_basis_is_equal = (basis1%struct == basis2%struct) .and. &
&                     all(basis1%k  - basis2%k  < 1e-6) .and. &
&                     all(basis1%r0 - basis2%r0 < 1e-6) .and. &
&                     basis1%npw == basis2%npw          .and. &
&   ((basis1%conjg.and.basis2%conjg).or.(.not.basis1%conjg.and..not.basis2%conjg)) .and. &
&                     all(basis1%g == basis2%g)
end function pw_basis_is_equal

subroutine pw_basis_copy(new_basis,old_basis)
! Copies an object
! Interfaced with operator(=)
  type (pw_basis), intent(inout) :: new_basis
  type (pw_basis), intent(in)    :: old_basis
!@ END MANUAL
  if(.not.associated(old_basis%g)) call errore("pw_basis_copy","not.assoc 1",1)
  if(.not.associated(old_basis%struct)) call errore("pw_basis_copy","not.assoc 2",1)
  if(.not.associated(new_basis%g)) call errore("pw_basis_copy","not.assoc 3",1)
  if(.not.associated(new_basis%struct)) call errore("pw_basis_copy","not.assoc 4",1)
  if(.not.new_basis%struct == old_basis%struct) call errore("pw_basis_copy","not.assoc 5",1)
  call pw_basis_set_npw(new_basis,old_basis%npw)
  new_basis%k  = old_basis%k
  new_basis%r0 = old_basis%r0
  new_basis%g  = old_basis%g
  new_basis%conjg  = old_basis%conjg
  new_basis%gmax  = old_basis%gmax
  new_basis%gmin  = old_basis%gmin
end subroutine pw_basis_copy

end module pw_basis_module
