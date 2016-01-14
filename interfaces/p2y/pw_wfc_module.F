module pw_wfc_module
use pars,        ONLY:SP
use pw_basis_module
implicit none
private
public :: pw_wfc,           &
          pw_wfc_init,      &
          pw_wfc_destroy,   &
          pw_wfc_set_basis, &
          pw_wfc_braket,    &
          assignment(=),    &
          operator(==),     &
          pw_wfc_scale,     &
          pw_wfc_p,         &
          pw_wfc_p_braket

type pw_wfc
  type(pw_basis), pointer :: basis
  integer                 :: npw
  complex(SP),    pointer :: val(:)
! This object contains a wavefunction in reciprocal space
! basis :: pointer to the associated basis grid
! npw   :: number of plane waves; redundant (it is size(%val))
! val   :: Fourier coefficients of the wavefunction, (1:npw)
end type pw_wfc
!@ END MANUAL

!@ MANUAL
interface operator(==)
  module procedure pw_wfc_is_equal
end interface
interface assignment(=)
  module procedure pw_wfc_copy0
  module procedure pw_wfc_copy1
end interface
interface pw_wfc_init
  module procedure pw_wfc_init00
  module procedure pw_wfc_init10
  module procedure pw_wfc_init11
  module procedure pw_wfc_init20
end interface
interface pw_wfc_destroy
  module procedure pw_wfc_destroy0
  module procedure pw_wfc_destroy1
  module procedure pw_wfc_destroy2
end interface
interface pw_wfc_scale
  module procedure pw_wfc_scale_r
  module procedure pw_wfc_scale_c
end interface

!@ END MANUAL

contains

!@ MANUAL
subroutine pw_wfc_init00(wfc,basis)
! Initializes a wfc object
  type(pw_wfc),                     intent(out) :: wfc
  type(pw_basis), target, optional, intent(in)  :: basis
! basis :: basis to be pointed to. it can be omitted and assigned later
!@ END MANUAL
  nullify(wfc%basis)
  wfc%npw   = 0
  allocate(wfc%val(0))
! call pw_allocate(wfc%val)
  if(present(basis)) call pw_wfc_set_basis(wfc,basis)
end subroutine pw_wfc_init00

subroutine pw_wfc_init10(wfc,basis)
! Initializes an array of wfc objects, all af them pointing to the same basis
  type(pw_wfc),                     intent(out) :: wfc(:)
  type(pw_basis), target, optional, intent(in)  :: basis
! basis :: basis to be pointed to. it can be omitted and assigned later
  integer :: i
  do i=1,size(wfc)
    if(present(basis)) then
      call pw_wfc_init(wfc(i),basis)
    else
      call pw_wfc_init(wfc(i))
    end if
  end do
end subroutine pw_wfc_init10

subroutine pw_wfc_init20(wfc,basis)
! Initializes a matrix of wfc objects, all af them pointing to the same basis
  type(pw_wfc),                     intent(out) :: wfc(:,:)
  type(pw_basis), target, optional, intent(in)  :: basis
! basis :: basis to be pointed to. it can be omitted and assigned later
  integer :: i,j
  do j=1,ubound(wfc,2)
    do i=1,ubound(wfc,1)
      if(present(basis)) then
        call pw_wfc_init(wfc(i,j),basis)
      else
        call pw_wfc_init(wfc(i,j))
      end if
    end do
  end do
end subroutine pw_wfc_init20

subroutine pw_wfc_init11(wfc,basis)
! Initializes an array of wfc objects, pointing to different basis
  type(pw_wfc),           intent(out) :: wfc(:)
  type(pw_basis), target, intent(in)  :: basis(:)
! basis :: basis to be pointed to.
  integer :: i
! if(size(wfc)/=size(basis)) ERROR("")
  do i=1,size(wfc)
    call pw_wfc_init(wfc(i),basis(i))
  end do
end subroutine pw_wfc_init11

!@ MANUAL
subroutine pw_wfc_destroy0(wfc)
! Destroys an object
  type(pw_wfc), intent(inout) :: wfc
!@ END MANUAL
!  if(.not.associated(wfc%val)) ERROR(" non e associato")
!  call pw_deallocate(wfc%val)
  if(associated(wfc%val)) deallocate(wfc%val)
!   if(associated(wfc%val)) write(0,*) "sono associato wfc"
end subroutine pw_wfc_destroy0

subroutine pw_wfc_destroy1(wfc)
! Destroys an array of objects
  type(pw_wfc), intent(inout) :: wfc(:)
  integer :: i
  do i=1,size(wfc)
    call pw_wfc_destroy(wfc(i))
  end do
end subroutine pw_wfc_destroy1

subroutine pw_wfc_destroy2(wfc)
! Destroys a matrix of objects
  type(pw_wfc), intent(inout) :: wfc(:,:)
  integer :: i,j
  do j=1,ubound(wfc,2)
    do i=1,ubound(wfc,1)
      call pw_wfc_destroy(wfc(i,j))
    end do
  end do
end subroutine pw_wfc_destroy2

subroutine pw_wfc_set_basis(wfc,basis)
! Set the basis pointer for a wfc object
  type(pw_wfc),           intent(inout) :: wfc
  type(pw_basis), target, intent(in)    :: basis
  if(.not.associated(wfc%val)) call errore("pw_wfc_set_basis",".not.associated(wfc%val",1)
  wfc%basis => basis
  if(wfc%npw==basis%npw) return
  wfc%npw = basis%npw
  deallocate(wfc%val)
  allocate(wfc%val(wfc%npw))
  wfc%val = 0.0
end subroutine pw_wfc_set_basis

!@ MANUAL
subroutine pw_wfc_copy0(new_wfc,old_wfc)
! Subroutine per copiare una wfc
  type(pw_wfc), intent(out) :: new_wfc
  type(pw_wfc), intent(in)  :: old_wfc
  integer :: ipw
!@ END MANUAL
  if(.not.associated(old_wfc%basis)) call errore("pw_wfc_copy0","Not assoc. 1",1)
  if(.not.associated(old_wfc%val)) call errore("pw_wfc_copy0","Not assoc. 2",1)
  if(.not.associated(new_wfc%basis)) call errore("pw_wfc_copy0","Not assoc. 3",1)
  if(.not.associated(new_wfc%val)) call errore("pw_wfc_copy0","Not assoc. 4",1)
  if(.not. new_wfc%basis==old_wfc%basis) call errore("pw_wfc_copy0","Not assoc. 5",1)
  do ipw=1,new_wfc%npw
    new_wfc%val(ipw) = old_wfc%val(ipw)
  enddo
end subroutine pw_wfc_copy0

subroutine pw_wfc_copy1(new_wfc,old_wfc)
  type(pw_wfc), intent(out) :: new_wfc(:)
  type(pw_wfc), intent(in)  :: old_wfc(:)
  integer ::i,n
  n=size(new_wfc)
  if(n/=size(old_wfc)) call errore("pw_wfc_copy1"," n not size old_wfc",1) 
  do i=1,n
    call pw_wfc_copy0(new_wfc(i),old_wfc(i))
  end do
end subroutine pw_wfc_copy1

function pw_wfc_is_equal(wfc1,wfc2)
! Subroutine per confrontare due wfc
  logical                  :: pw_wfc_is_equal
  type(pw_wfc), intent(in) :: wfc1,wfc2
!@ END MANUAL
  pw_wfc_is_equal = (wfc1%basis == wfc2%basis) .and. &
                    all(abs(wfc1%val-wfc2%val)<1e-7)
end function pw_wfc_is_equal

function pw_wfc_braket(bra,ket)
! Subroutine per il prodotto scalare di due funzioni d'onda
! use num_module
  complex(SP) :: pw_wfc_braket
  type(pw_wfc), intent(in) :: bra,ket
! if(.not.bra%basis == ket%basis) call errore("pw_wfC_braket","with different
! grids not implemented",1)
  pw_wfc_braket = dot_product(bra%val,ket%val)
end function pw_wfc_braket

subroutine pw_wfc_scale_r(wfc,scale)
! Subroutine per riscalare di un fattore reale una funzione d'onda
! use num_module
  type(pw_wfc), intent(inout) :: wfc
  real(SP),         intent(in)    :: scale
  integer :: i
!@ END MANUAL
! call lasi_scal(wfc%npw,scale,wfc%val,1)
! do i = 1,wfc%npw
!    wfc%val(i) = wfc%val(i)*scale
! enddo
  wfc%val = wfc%val*scale
end subroutine pw_wfc_scale_r

!@ MANUAL
subroutine pw_wfc_scale_c(wfc,scale)
! Subroutine per riscalare di un fattore complesso una funzione d'onda
! use num_module
  type(pw_wfc), intent(inout) :: wfc
  complex(SP),      intent(in)    :: scale
!@ END MANUAL
! call lasi_scal(wfc%npw,scale,wfc%val,1)
  wfc%val = wfc%val*scale
end subroutine pw_wfc_scale_c

function pw_wfc_p_braket(bra,ket)
! Effettua il valore di aspettazione di p
! pw_wfc_p_braket(:) e' p(:), dove p(1) e' p_x etc.
  complex(SP)                   :: pw_wfc_p_braket(3)
  type (pw_wfc), intent(in) :: bra,ket
  type(pw_wfc) :: wfc_tmp(3)
  call pw_wfc_init(wfc_tmp,bra%basis)
  call pw_wfc_p(wfc_tmp,ket)
  pw_wfc_p_braket(1) = pw_wfc_braket(bra,wfc_tmp(1))
  pw_wfc_p_braket(2) = pw_wfc_braket(bra,wfc_tmp(2))
  pw_wfc_p_braket(3) = pw_wfc_braket(bra,wfc_tmp(3))
  call pw_wfc_destroy(wfc_tmp)
end function pw_wfc_p_braket

subroutine pw_wfc_p(new_wfc,old_wfc)
! use num_module
! Applica l'operatore p
! NB new_wfc(1) e' p_x, new_wfc(2) e' p_y, new_wfc(3) e' p_z
  type (pw_wfc), intent(inout) :: new_wfc(3)
  type (pw_wfc), intent(in)    :: old_wfc
!@ END MANUAL
  integer :: ipw
  real(SP)    :: kg(3),p(3),b(3,3)
  b = old_wfc%basis%struct%b
  do ipw = 1,old_wfc%npw
    kg = old_wfc%basis%g(:,ipw)+old_wfc%basis%k
!   p  = 2.0 * num_matmul(b,kg)   !! CHECK
    p  = 2.0 * matmul(b,kg)
! il 2.0 serve perche' H_k=-\nabla^2 (Rydberg)
    new_wfc(1)%val(ipw) = old_wfc%val(ipw) * p(1)
    new_wfc(2)%val(ipw) = old_wfc%val(ipw) * p(2)
    new_wfc(3)%val(ipw) = old_wfc%val(ipw) * p(3)
  end do
end subroutine pw_wfc_p

end module pw_wfc_module
