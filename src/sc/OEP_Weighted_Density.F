!
! Copyright (C) 2000-2011 M. Gruning and the YAMBO team
!              http://www.yambo-code.org
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
subroutine OEP_Weighted_Density(en,Xk,rho,force_si)
  !
  ! Modified el_density sub needed for Bulat Levy modified 
  ! Slater and OEP approx/procedure. 
  !
  use pars,          ONLY:SP
  use com,           ONLY:warning
  use electrons,     ONLY:levels,n_spin,n_sp_pol
  use R_lattice,     ONLY:bz_samp
  use D_lattice,     ONLY:nsym,i_time_rev
  use FFT_m,         ONLY:fft_size,fft_rot_r
  use wave_func,     ONLY:wf_state,wf
  use SC,            ONLY:l_oep_EWeight
  !
  implicit none
  type(bz_samp)::Xk
  type(levels) ::en       
  real(SP)     ::rho(fft_size)
  logical,     intent(in) :: force_si
  !
  ! Work Space 
  !
  integer :: i1,i2,ik,ifft,rho_syms
  real(SP):: rho_no_sym(fft_size),EWeight
  !
  rho=0._SP
  rho_no_sym=0._SP
  !
  do i1=1,en%nbm
    do i2=1,Xk%nibz
      if (size(wf_state,1)<i1) cycle
      if (size(wf_state,2)<i2) cycle
      !
      ifft=wf_state(i1,i2,1)
      EWeight = en%e(i1,i2,1)+en%Efermi(1)
      rho_no_sym(:)=rho_no_sym(:)+en%f(i1,i2,1)*Xk%weights(i2)/&
           &EWeight*abs(wf(:,ifft))**2
    enddo
  enddo
  !
  ! Symmetrization
  !
  rho_syms=nsym/(i_time_rev+1)
  if (force_si) rho_syms=nsym
  do i1=1,rho_syms
    rho(:)=rho(:)+real(rho_no_sym(fft_rot_r(i1,:)),SP)/real(nsym,SP)
  enddo
  !
  if (.not.force_si) rho=rho*(1+i_time_rev)
  !
end subroutine
