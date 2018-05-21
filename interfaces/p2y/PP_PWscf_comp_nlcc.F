! Self-energies and eXcitations (SaX)
! Copyright (C) 2006 SaX developers team
!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
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
!
subroutine PP_PWscf_comp_nlcc(basis,atoms)
  use pw_basis_module
  use pw_wfc_module
  use pw_atoms_module
  use numerical_module
  use num_interpolation_module
  use pw_pseudo_module
  !
  use pars,          ONLY:SP,DP
  use pseudo,        ONLY:pp_rhog_nlcc,pp_has_nlcc_atom
  use D_lattice,     ONLY:n_atomic_species,atom_pos,n_atoms_species
  !
  implicit none
  !
  type(pw_basis), target, intent(in) :: basis
  type(pw_atoms), intent(in) :: atoms
  type(pw_pseudo), pointer :: psp
  !
  integer :: is,ia,ig,npw
  !
  real(SP) :: b(3,3),G(3)
  real(SP) :: G_norm(basis%npw)
  real(DP) :: rhoG(basis%npw), V_m1
  real(DP) :: G_dot_R
  complex(DP) :: e_iGR,sf
  real(SP), allocatable :: G_cc(:,:)
  real(SP), allocatable :: pp_rhog_nlcc_atom(:,:)
  !
  b = basis%struct%b
  npw = basis%npw
  allocate(G_cc(3,npw))
  allocate(pp_rhog_nlcc_atom(npw,atoms%ntypes))
  !
  do ig=1,npw
    G = real(basis%g(:,ig),SP)
    G_cc(:,ig) = matmul(b,G)
    G_norm(ig) = sqrt(sum(G_cc(:,ig)**2))
  enddo
  !
  pp_rhog_nlcc(:npw)=0.0
  !
  do is=1,atoms%ntypes
    psp => atoms%pseudo(is)
    !
    rhog=0._SP
    pp_has_nlcc_atom(is)=0
    if (psp%has_nlcc) pp_has_nlcc_atom(is)=1
    !
    if (.not.psp%has_nlcc) cycle
    !
    do ig=1,npw
      rhog(ig) = num_interpolation_calc(psp%interp_nlcc,G_norm(ig),3)
    enddo
    V_m1 = 1.0d0/basis%struct%a_omega
    pp_rhog_nlcc_atom(:npw,is) = rhog(:npw)*V_m1
    !
    ! include the structure factors 
    ! 
    do ig = 1,npw
      !
      sf=0.0
      !
      do ia = 1,n_atoms_species(is)
        G_dot_R = dot_product(G_cc(:,ig),atom_pos(:,ia,is))
        e_iGR = cmplx(cos(G_dot_R),-sin(G_dot_R),DP)
        sf = sf + e_iGR
      enddo
      !  
      pp_rhog_nlcc(ig) = pp_rhog_nlcc(ig) + sf*pp_rhog_nlcc_atom(ig,is)
      !
    enddo
    !
  enddo
  !
  deallocate(pp_rhog_nlcc_atom)
  deallocate(G_cc)
  !
end subroutine PP_PWscf_comp_nlcc
