!
! Copyright (C) 2000-2005 D. De Fausti and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
 subroutine el_magnetization(en,Xk,el_mag)
!=========================================
!
! Electronic Magnetization for electronic Spinors
!
!  M = Sum_I (PSI^I)* Sigma_Pauli* PSI^I  I=occupied states
!
!  PSI^I = spinor
!  Sigma_Pauli=the 3 Pauli matrices
!
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_spin,n_spinor
 use D_lattice,     ONLY:nsym,i_time_rev
 use FFT_m,         ONLY:fft_size,fft_rot_r
 use wave_func,     ONLY:wf_state,wf
 implicit none
 type(levels) ::en
 type(bz_samp)::Xk
 real(SP)     ::el_mag(fft_size,3)
 !
 ! WorkSpace
 !
 integer :: i1,i2,i1fft,i2fft,amag_syms,zeta,rho_syms
 real(SP):: cv(fft_size,3)
 !
 el_mag=0.
 cv=0.
 !
 do i1=1,en%nbm
   do i2=1,Xk%nibz
     i1fft=wf_state(i1,i2,1)
     i2fft=wf_state(i1,i2,2)
     if (i1fft==0 .or. i2fft==0) cycle
     if (n_spinor==2) then
       !
       ! mx
       !
       cv(:,1)=cv(:,1)+Xk%weights(i2)*(&
&      en%f(i1,i2,1)*conjg(wf(:,i1fft))*wf(:,i2fft)&
&     +en%f(i1,i2,2)*conjg(wf(:,i2fft))*wf(:,i1fft) )
       !
       ! my
       !
       cv(:,2)=cv(:,2)+Xk%weights(i2)*(0.,-1.d0)*(&
&      en%f(i1,i2,2)*conjg(wf(:,i1fft))*wf(:,i2fft)&
&     -en%f(i1,i2,1)*conjg(wf(:,i2fft))*wf(:,i1fft) )
     endif
     !
     ! mz
     !
     cv(:,3)=cv(:,3)+Xk%weights(i2)*(&
&    en%f(i1,i2,2)*conjg(wf(:,i1fft))*wf(:,i1fft)&
&   -en%f(i1,i2,1)*conjg(wf(:,i2fft))*wf(:,i2fft) )
   enddo
 enddo
 !
 do i1=1,nsym
   do i2=1,3
     el_mag(:,i2)=el_mag(:,i2)+real(cv(fft_rot_r(i1,:),i2)/real(nsym))
   end do
 enddo
 !
 end subroutine
!
