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
  use pseudo,        ONLY:pp_rho_nlcc,pp_has_nlcc
  !
  implicit none
  !
  type(pw_basis), target, intent(in) :: basis
  type(pw_atoms), intent(in) :: atoms
  type(pw_pseudo), pointer :: psp
  !
  integer :: itype,ipw,npw
  !
  real(SP) :: b(3,3),G(3),G_cc(3)
  real(SP) :: G_norm(basis%npw)
  real(DP) :: rhoG(basis%npw), V_m1
  !
  b = basis%struct%b
  npw = basis%npw
  do ipw=1,npw
    G = real(basis%g(:,ipw),SP)
    G_cc = matmul(b,G)
    G_norm(ipw) = sqrt(sum(G_cc(:)**2))
  enddo
  !
  do itype=1,atoms%ntypes
    psp => atoms%pseudo(itype)
    !
    rhog=0._SP
    pp_has_nlcc(itype)=0
    if (psp%psp_has_nlcc) pp_has_nlcc(itype)=1
    !
    if (.not.psp%psp_has_nlcc) cycle
    !
    do ipw=1,npw
      if(G_norm(ipw) < 1.E-5) cycle
      rhog(ipw) = num_interpolation_calc(psp%interpolation_nlcc,G_norm(ipw),3)
    enddo
    V_m1 = 1.0d0/basis%struct%a_omega
    pp_rho_nlcc(:npw,itype) = sqrt(abs(V_m1))*rhog(:npw)
  enddo
  !
end subroutine PP_PWscf_comp_nlcc
