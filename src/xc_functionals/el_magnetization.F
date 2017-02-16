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
 !
end subroutine
