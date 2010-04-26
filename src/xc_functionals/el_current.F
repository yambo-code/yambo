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
subroutine el_current(en,Xk,ik,J,J_diam,l_directions)
  !
  ! Electronic current: j = (q/m)* < [p -   (q/c) *A] >
  !  (q=-1,m=1) ==>       =  -1    < [p + (1/137.)*A] >
  !
  use pars,          ONLY:SP
  use electrons,     ONLY:levels,n_spin,n_sp_pol,Boltz_f
  use R_lattice,     ONLY:bz_samp
  use D_lattice,     ONLY:DL_sop
  use FFT_m,         ONLY:fft_size,fft_rot_r
  use wave_func,     ONLY:wf_state,wf,wf_x,wf_y,wf_z
#if defined _MAGNETIC
  use magnetic,      ONLY:A_magn_x,A_magn_y,A_magn_z,MAG_landau
#endif
  use units,         ONLY:SPEED_OF_LIGHT
  !
  implicit none
  type(bz_samp)::Xk
  type(levels) ::en       
  !
  integer  :: ik
  logical  :: l_directions(3)
  real(SP) :: J(3,n_sp_pol,fft_size)
  real(SP) :: J_diam(3,n_sp_pol,fft_size)
  !
  ! Work Space 
  !
  integer :: i_b,i_spin,ifft,i_wf,i1,is
  real(SP):: J_dump(3,n_sp_pol,fft_size),J_dump_diam(3,n_sp_pol,fft_size)
  real(SP):: pre_factor
  !
  J_dump=0._SP
  J_dump_diam=0._SP
  !
  do i_b=en%nbf+1,en%nbm
    do i_spin=1,n_spin
      !
      i_wf=wf_state(i_b,ik,i_spin)
      !
      do i1=1,Xk%nstar(ik)
        !
        is=Xk%star(ik,i1)
        !
        pre_factor=Boltz_f(i_b,ik,is,i_spin)/float(Xk%nbz)
        !
        ! Paramagnetic term of the current
        !
        if (l_directions(1)) J_dump(1,i_spin,:)=pre_factor*real(wf(:,i_wf)*conjg(wf_x(:,i_wf)))
        if (l_directions(2)) J_dump(2,i_spin,:)=pre_factor*real(wf(:,i_wf)*conjg(wf_y(:,i_wf)))
        if (l_directions(3)) J_dump(3,i_spin,:)=pre_factor*real(wf(:,i_wf)*conjg(wf_z(:,i_wf)))
        !
        ! Diamagnetic term of the current
        !
#if defined _MAGNETIC
        if(MAG_landau) then
          if(allocated(A_magn_x)) J_dump_diam(1,i_spin,:)=pre_factor*abs(wf(:,i_wf))**2*A_magn_x(:)/SPEED_OF_LIGHT
          if(allocated(A_magn_y)) J_dump_diam(2,i_spin,:)=pre_factor*abs(wf(:,i_wf))**2*A_magn_y(:)/SPEED_OF_LIGHT
          if(allocated(A_magn_z)) J_dump_diam(3,i_spin,:)=pre_factor*abs(wf(:,i_wf))**2*A_magn_z(:)/SPEED_OF_LIGHT
        endif
#endif
        !
        forall(ifft=1:fft_size) J(:,i_spin,ifft)=J(:,i_spin,ifft)  &
&                         -matmul( DL_sop(:,:,is), J_dump(:,i_spin,fft_rot_r(is,ifft)) )
        forall(ifft=1:fft_size) J_diam(:,i_spin,ifft)=J_diam(:,i_spin,ifft)  &
&                         -matmul( DL_sop(:,:,is), J_dump_diam(:,i_spin,fft_rot_r(is,ifft)) )
        !
      enddo
      !
    enddo
    !DEBUG>
    !write (*,*) i_b,ik,sum( J(1,1,:) ), sum( J(2,1,:)), sum( J(3,1,:) )
    !DEBUG<
    !
  enddo
  !
end subroutine
