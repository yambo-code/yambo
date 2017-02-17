!
! Copyright (C) 2000-2010 D. De Fausti and the YAMBO team
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
subroutine el_density(en,Xk,rho,force_si)
  !
  !Electronic density.
  !
  !Note that here the identity
  !
  ! \sum_{S_k} F(S_k r) = 1/R_k \sum_{S} F(S r) (1)
  ! 
  !where S_k is a symm. op. in the star of k, S is a generic symm. op.
  !and R_k is the rank of the small group at k.
  !
  !Using (1) the density is calculated in two steps
  !
  ! rho(r) = 1/nkibz \sum_{ n k S_k } f_{n k} |wf_{n k}(S_k^-1 r)|^2=
  !        = 1/nkibz \sum_{S} \sum_{n k} f_{n k}/R_k |wf_{n k}(S^-1 r)|^2 =
  !        = 1/nsym \sum_{S} ( \sum_{n k} f_{n k} w_k |wf_{n k}(S^-1 r)|^2 )
  !
  !where we have used the relation
  !
  ! w_k = nsym/(nkibz * rank(k))
  !
  !
  use pars,          ONLY:SP
  use com,           ONLY:warning
  use electrons,     ONLY:levels,n_spin,n_sp_pol
  use R_lattice,     ONLY:bz_samp
  use D_lattice,     ONLY:nsym,i_time_rev
  use FFT_m,         ONLY:fft_size,fft_rot_r
  use wave_func,     ONLY:wf_state,wf
  !
  implicit none
  type(bz_samp)::Xk
  type(levels) ::en       
  real(SP)     ::rho(fft_size)
  logical,     intent(in) :: force_si
  !
  ! Work Space 
  !
  integer :: i1,i2,ik,i_spin,ifft,rho_syms,ifft2
  real(SP):: rho_no_sym(fft_size),f_occ
  logical :: warn_done
  integer :: iv_max,iv_min
  !
  rho=0._SP
  rho_no_sym=0._SP
  warn_done=.false.
  !
  iv_max=en%nbm
  iv_min=1
  !
  !
  do i1=iv_min,iv_max
    do i2=1,Xk%nibz
      !
      !        
      i_spin=1
      !
        !
        if (size(wf_state,1)<i1) cycle
        if (size(wf_state,2)<i2) cycle
        !
        ifft=wf_state(i1,i2,i_spin)
        !
        if (ifft==0) then
          if (.not.warn_done) call warning('Not enough states to calculate rho')
          warn_done=.true.
          cycle
        endif
        !
        f_occ=en%f(i1,i2,1)
        !
        !
        rho_no_sym(:)=rho_no_sym(:)+f_occ*Xk%weights(i2)*abs(wf(:,ifft))**2.
        !
      !
    enddo
  enddo
  !
  !
  !
  ! Simmetrization
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
