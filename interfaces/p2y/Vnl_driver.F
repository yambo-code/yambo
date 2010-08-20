subroutine Vnl_driver
  use sax_module
  implicit none
  type(pw_struct) :: struct

  write(*,*) "Start Vnl driver"
! fill atoms 
  call fill_struct(struct)
  write(*,*) "Fill atoms"
  call fill_atoms(struct)




! Read pseudo data -> atoms

! call pw_pseudovelocity_init
! loop k
  write(*,*) "Fill basis"
  call fill_basis
! loop vb,cb
! fill basis
! call pw_pseudovelocity_braket
! endloop
! call pw_pseudovelocity_destroy

! call ioVnl

  return
end subroutine Vnl_driver

subroutine fill_struct(struct)
  use sax_module
  use pars,                  only : pi
  use R_lattice,             ONLY : b
  use D_lattice,             ONLY : DL_vol, a
  implicit none
  type(pw_struct), intent(out) :: struct

  !call pw_struct_set  ! probably not needed

  struct%a = a
  struct%b = b
  struct%a_omega = DL_vol
  struct%b_omega = 0.0 ! not used

  return
end subroutine fill_struct

!type pw_atoms
!  type (pw_struct), pointer :: struct
!  integer                   :: natoms,ntypes
!  type (pw_pseudo), pointer :: pseudo(:)
!  character(100),   pointer :: names(:)
!  real,             pointer :: positions(:,:)
!  integer,          pointer :: type_map(:)
!  real                      :: cutoff_vloc
!end type pw_atoms
subroutine fill_atoms(struct)
  use pw_data, only : psfile, nbeta, nmesh, nat_, atm_, tau_, ityp_
  use sax_module
  use D_lattice,   ONLY:n_atoms_species_max,n_atomic_species,n_atoms_species, &
&                        atom_pos, Z_species,atomic_number
  implicit none
  type(pw_atoms) :: atoms
  type(pw_pseudo) :: test_pseudo
  type(pw_struct), intent(in) :: struct
  integer        :: i

  call pw_atoms_init(atoms,struct,4.0)
  call pw_atoms_set_dim(atoms, nat_, n_atomic_species)


  atoms%names = atm_
  atoms%positions = tau_
  atoms%type_map = ityp_

  do i =1,n_atomic_species
     call fill_a_pseudo(atoms%pseudo,psfile(i),nbeta(i),nmesh(i))
  enddo

  call check_atoms(atoms)

return
end subroutine fill_atoms

!type pw_atoms
!  type (pw_struct), pointer :: struct
!  integer                   :: natoms,ntypes
!  type (pw_pseudo), pointer :: pseudo(:)
!  character(100),   pointer :: names(:)
!  real,             pointer :: positions(:,:)
!  integer,          pointer :: type_map(:)
!  real                      :: cutoff_vloc
!end type pw_atoms
subroutine check_atoms(atoms)
  use sax_module, only : pw_atoms
  implicit none
  type(pw_atoms) :: atoms
  integer :: i

  write(*,*) "Check atoms type:"
  write(*,*) "natoms:",atoms%natoms
  write(*,*) "ntypes:",atoms%ntypes
  write(*,*) "names :",(" ",trim(atoms%names(i)),i=1,atoms%ntypes)
  write(*,'(1x,a,9f6.3)') "a     :",atoms%struct%a
  write(*,'(1x,a,9f6.3)') "b     :",atoms%struct%b
 !write(*,'(a,//1x,i3,3f6.3,i3)') "pos/map   :",(i,atoms%positions(:,i),atoms%type_map(i),i=1,atoms%natoms)
  do i=1,atoms%natoms
    write(*,'(1x,i3,3f6.3,i3,a)') i,atoms%positions(:,i),atoms%type_map(i),trim(atoms%names(i))
  enddo
  write(*,*) "map   :",atoms%type_map

  return
end subroutine check_atoms

subroutine fill_a_pseudo(pseudo,psfile,nbeta, nmesh)
  use sax_module
! use pw_pseudo_module, only :  pw_pseudo_set_dim, pw_pseudo
  use qexml_module
  implicit none
  type(pw_pseudo), intent(out) :: pseudo
  integer, intent(inout)          :: nbeta, nmesh
  character(256), intent(in)   :: psfile

  call pw_pseudo_init(pseudo)
  call pw_pseudo_set_dim(pseudo,nbeta,nmesh)
  call read_pseudo_data(psfile,nmesh,nbeta,pseudo%z,pseudo%mesh,&
&       pseudo%wmesh,pseudo%vloc,pseudo%lbeta,pseudo%mesh_beta,pseudo%beta,pseudo%d)

  return
end subroutine fill_a_pseudo


subroutine fill_basis
use sax_module
implicit none
type(pw_basis) :: basis

!call pw_basis_init

!basis%struct => struct
!basis%npw = npw
!basis%g => gvec  ! If needed will need reduction routine used for surfaces
!basis%r0 = 0
!basis%k  = k
!basis%gmax
!basis%gmin

return
end subroutine fill_basis

