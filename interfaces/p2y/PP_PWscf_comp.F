! Self-energies and eXcitations (SaX)
! Copyright (C) 2006 SaX developers team
!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine PP_PWscf_comp(basis,atoms)
  use pw_basis_module
  use pw_wfc_module
  use pw_atoms_module
  use numerical_module
  use num_interpolation_module
  use pw_pseudo_module
  !
  use pars,                  ONLY:SP,DP
  use electrons,     ONLY:n_spinor
  use pseudo,        ONLY:pp_kb,pp_kbd,pp_kbs
  !
  implicit none
  !
  type(pw_basis), target, intent(in) :: basis
  type(pw_atoms), intent(in) :: atoms
  type(pw_pseudo), pointer :: psp
  !
  integer :: itype,ibeta,nbeta,ipw,npw,l
  !
  real(SP) :: b(3,3),KpG(3),KpG_cc(3),kb_E
  real(SP) :: KpG_norm(basis%npw)
  real(SP) :: fl(basis%npw)
  real(SP) :: dfl(basis%npw)
  !
  b = basis%struct%b
  npw = basis%npw
  do ipw=1,npw
    KpG = basis%k + real(basis%g(:,ipw),SP)
    KpG_cc = matmul(b,KpG)
    KpG_norm(ipw) = sqrt(sum(KpG_cc(:)**2))
  enddo
  !
  do itype=1,atoms%ntypes
    psp => atoms%pseudo(itype)
    nbeta = psp%nbeta
    do ibeta=1,nbeta
      l=psp%lbeta(ibeta)
      fl=0._SP
      dfl=0._SP
      do ipw=1,npw
        if(KpG_norm(ipw) < 1.E-5) cycle
        fl(ipw) = num_interpolation_calc(psp%interp_beta(ibeta),KpG_norm(ipw),3)
        dfl(ipw) = num_interpolation_calc_der(psp%interp_beta(ibeta),KpG_norm(ipw),3,ider=1)
      enddo
      kb_E = psp%d(ibeta)/basis%struct%a_omega*0.5_SP  ! 0.5 = Ry2Ha
      pp_kbs(itype,ibeta) = kb_E/abs(kb_E)
      pp_kb(:npw,itype,ibeta,1) = sqrt(abs(kb_E))*fl(:npw)*KpG_norm(:npw)**l
      pp_kbd(:npw,itype,ibeta,1) = sqrt(abs(kb_E))*(dfl(:npw)*KpG_norm(:npw)**l+ &
&                                         fl(:npw)*real(l,SP)*KpG_norm(:npw)**(l-1) )
    enddo
  enddo
  !
end subroutine PP_PWscf_comp
