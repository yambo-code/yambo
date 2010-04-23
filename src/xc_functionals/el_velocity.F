!
! Copyright (C) 2000-2010 D. Sangalli and the YAMBO team
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
subroutine el_velocity(en,Xk)
  !
  use pars,          ONLY:SP
  use com,           ONLY:warning
  use electrons,     ONLY:levels,n_spin,n_sp_pol,spin_occ,default_nel
  use R_lattice,     ONLY:bz_samp
  use FFT_m,         ONLY:fft_size
  use wave_func,     ONLY:wf_state,wf,wf_x,wf_y,wf_z,wf_ng
  !
  implicit none
  type(bz_samp)::Xk
  type(levels) ::en       
  !
  ! Work Space 
  !
  integer  :: i_band,i_dir,i_sym,i_spin,ifft,i_wf
  logical  :: warn_done
  real(SP) :: tmp_velocity(3,fft_size)
  !
  warn_done=.false.
  !
!  do i_band=en%nbf+1,en%nbm
!    do i_spin=1,n_spin
!      ! 
!      tmp_velocity            = 0._SP
!      !
!      if (size(wf_state,3)<i_spin) cycle
!      if (size(wf_state,1)<i_band) cycle
!      if (size(wf_state,2)<ik) cycle
!      !
!      i_wf=wf_state(i_band,ik,i_spin)
!      !
!      if (i_wf==0) then
!        if (.not.warn_done) call warning("Couldn't evaluate the velocity of some states")
!        warn_done=.true.
!        cycle
!      endif
!      !
!      if(l_dir(1)) tmp_velocity(1,:)=real(wf(:,i_wf)*conjg(wf_x(:,i_wf)))
!      if(l_dir(2)) tmp_velocity(2,:)=real(wf(:,i_wf)*conjg(wf_y(:,i_wf)))
!      if(l_dir(3)) tmp_velocity(3,:)=real(wf(:,i_wf)*conjg(wf_z(:,i_wf)))
!      !
!      !
!      do i_dir=1,3
!        if(.not.l_dir(i_dir)) cycle
!        do ifft=1,fft_size
!          vel_per_state(i_dir,i_band,ik,i_spin)=vel_per_state(i_dir,i_band,ik,i_spin)+tmp_velocity(i_dir,ifft)
!        enddo
!      enddo
!      !
!    enddo
!  enddo
  !
end subroutine
