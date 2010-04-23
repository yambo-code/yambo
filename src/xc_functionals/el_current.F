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
subroutine el_current(en,Xk,ik,j_current,jd_current)
  !
  ! Electronic current: j = (q/m)* < [p -   (q/c) *A] >
  !  (q=-1,m=1) ==>       =  -1    < [p + (1/137.)*A] >
  !
  use pars,          ONLY:SP
  use com,           ONLY:warning
  use electrons,     ONLY:levels,n_spin,n_sp_pol,spin_occ,default_nel
  use R_lattice,     ONLY:bz_samp
  use D_lattice,     ONLY:nsym,i_time_rev,DL_sop
  use FFT_m,         ONLY:fft_size,fft_rot_r
  use wave_func,     ONLY:wf_state,wf,wf_x,wf_y,wf_z,wf_ng
#if defined _MAGNETIC
  use magnetic,      ONLY:A_magn_x,A_magn_y,A_magn_z,MAG_landau
#endif
  use units,         ONLY:SPEED_OF_LIGHT
  !
  implicit none
  type(bz_samp)::Xk
  type(levels) ::en       
  !
  real(SP)     :: j_current(3,n_sp_pol,fft_size)
  real(SP)     :: jd_current(3,n_sp_pol,fft_size)
  !
  ! Work Space 
  !
  integer :: i_band,ik,i_dir,i_sym,i_spin,ifft,i_wf
  real(SP):: j_current_no_sym(3,n_sp_pol,fft_size),jd_current_no_sym(3,n_sp_pol,fft_size)
  real(SP):: f_occ
  logical :: warn_done
  !
  if(i_time_rev==1) then
    j_current=(0.,0.)
    jd_current=(0.,0.)
    call warning('T-rev present --> j=0')
    return
  endif
  !
  j_current_no_sym=0._SP
  jd_current_no_sym=0._SP
  warn_done=.false.
  !
#if defined _MAGNETIC
  if(MAG_landau) call MAG_common_build_A(.true.)
#endif
  !
!  do i_band=1,en%nbm
  do i_band=en%nbf+1,en%nbm
    do i_spin=1,n_spin
      !
      if (size(wf_state,3)<i_spin) cycle
      if (size(wf_state,1)<i_band) cycle
      if (size(wf_state,2)<ik) cycle
      !
      i_wf=wf_state(i_band,ik,i_spin)
      !
      if (i_wf==0) then
        if (.not.warn_done) call warning('Not enough states to calculate rho')
        warn_done=.true.
        cycle
      endif
      !
      if(i_band<=int(default_nel/2.)) f_occ=en%f(i_band,ik,i_spin)-spin_occ
      if(i_band> int(default_nel/2.)) f_occ=en%f(i_band,ik,i_spin)
!      f_occ=en%f(i_band,ik,i_spin)
      !
      ! Paramagnetic term of the current
      j_current_no_sym(1,i_spin,:)=j_current_no_sym(1,i_spin,:) &
&                                  +f_occ*Xk%weights(ik)*real(wf(:,i_wf)*conjg(wf_x(:,i_wf)))
      j_current_no_sym(2,i_spin,:)=j_current_no_sym(2,i_spin,:) &
&                                  +f_occ*Xk%weights(ik)*real(wf(:,i_wf)*conjg(wf_y(:,i_wf)))
      j_current_no_sym(3,i_spin,:)=j_current_no_sym(3,i_spin,:) &
&                                  +f_occ*Xk%weights(ik)*real(wf(:,i_wf)*conjg(wf_z(:,i_wf)))
      !
      ! Diamagnetic term of the current
#if defined _MAGNETIC
      if(MAG_landau) then
        if(allocated(A_magn_x)) jd_current_no_sym(1,i_spin,:)=jd_current_no_sym(1,i_spin,:) &
&                                                     +f_occ*Xk%weights(ik)*abs(wf(:,i_wf))**2*A_magn_x(:)/SPEED_OF_LIGHT
        if(allocated(A_magn_y)) jd_current_no_sym(2,i_spin,:)=jd_current_no_sym(2,i_spin,:) &
&                                                     +f_occ*Xk%weights(ik)*abs(wf(:,i_wf))**2*A_magn_y(:)/SPEED_OF_LIGHT
        if(allocated(A_magn_z)) jd_current_no_sym(3,i_spin,:)=jd_current_no_sym(3,i_spin,:) &
&                                                     +f_occ*Xk%weights(ik)*abs(wf(:,i_wf))**2*A_magn_z(:)/SPEED_OF_LIGHT
      endif
#endif
      !
    enddo
    !
  enddo
  !
  ! This is the electromagnetic current
  !
  j_current(:,:,:)=j_current(:,:,:)-j_current_no_sym(:,:,:)
  jd_current(:,:,:)=jd_current(:,:,:)-jd_current_no_sym(:,:,:)
  !
  ! Free mem
  !
#if defined _MAGNETIC
  if(MAG_landau) call MAG_common_build_A(.false.)
#endif
  if(ik==Xk%nibz) then
    j_current_no_sym(:,:,:)=j_current(:,:,:)
    jd_current_no_sym(:,:,:)=jd_current(:,:,:)
    j_current=0._SP
    jd_current=0._SP
    !
    ! Simmetrization
    do i_sym=1,nsym
      do i_spin=1,n_sp_pol
        forall(ifft=1:fft_size) j_current(:,i_spin,ifft)=j_current(:,i_spin,ifft)  &
&                         +matmul( DL_sop(:,:,i_sym), j_current_no_sym(:,i_spin,fft_rot_r(i_sym,ifft)) )/real(nsym,SP)
        forall(ifft=1:fft_size) jd_current(:,i_spin,ifft)=jd_current(:,i_spin,ifft)  &
&                         +matmul( DL_sop(:,:,i_sym), jd_current_no_sym(:,i_spin,fft_rot_r(i_sym,ifft)) )/real(nsym,SP)
      enddo
    enddo
    !
  endif
  !
end subroutine
