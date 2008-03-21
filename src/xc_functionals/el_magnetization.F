!
! Copyright (C) 2000-2008 D. De Fausti and the YAMBO team
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
subroutine el_magnetization(en,Xk,el_mag)
 !
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
 use D_lattice,     ONLY:nsym
 use FFT_m,         ONLY:fft_size,fft_rot_r
 use wave_func,     ONLY:wf_state,wf
 implicit none
 type(levels) ::en
 type(bz_samp)::Xk
 real(SP)     ::el_mag(fft_size,3)
 !
 ! Work Space
 !
 integer :: i1,i2,ifft_up,ifft_dn
 real(SP):: cv(fft_size,3)
 !
 el_mag=0.
 cv=0.
 !
 if (n_spin==1) return
 !
 do i1=1,en%nbm
   do i2=1,Xk%nibz
     ifft_up=wf_state(i1,i2,1)
     ifft_dn=wf_state(i1,i2,2)
     if (ifft_up==0 .or. ifft_dn==0) cycle
     if (n_spinor==2) then
       !
       ! mx
       !
       cv(:,1)=cv(:,1)+Xk%weights(i2)*(&
&      en%f(i1,i2,1)*conjg(wf(:,ifft_up))*wf(:,ifft_dn)&
&     +en%f(i1,i2,2)*conjg(wf(:,ifft_dn))*wf(:,ifft_up) )
       !
       ! my
       !
       cv(:,2)=cv(:,2)+Xk%weights(i2)*(0.,-1.d0)*(&
&      en%f(i1,i2,2)*conjg(wf(:,ifft_up))*wf(:,ifft_dn)&
&     -en%f(i1,i2,1)*conjg(wf(:,ifft_dn))*wf(:,ifft_up) )
     endif
     !
     ! mz
     !
     cv(:,3)=cv(:,3)+Xk%weights(i2)*(&
&    en%f(i1,i2,2)*conjg(wf(:,ifft_up))*wf(:,ifft_up)&
&   -en%f(i1,i2,1)*conjg(wf(:,ifft_dn))*wf(:,ifft_dn) )
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
