!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
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
 subroutine el_density(en,Xk,rho,use_si)
!=======================================
!
!Electronic density
!
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_spin
 use D_lattice,     ONLY:nsym,i_time_rev
 use FFT_m,         ONLY:fft_size,fft_rot_r
 use wave_func,     ONLY:wf_state,wf
 implicit none
 type(levels) ::en       
 type(bz_samp)::Xk
 real(SP)     ::rho(fft_size)
 logical      ::use_si
!ws
 integer :: i1,i2,is,ifft,rho_syms
 real(SP):: cv(fft_size)
 rho=0.
 cv=0.
 do i1=1,en%nbm
   do i2=1,Xk%nibz
     do is=1,n_spin
       ifft=wf_state(i1,i2,is)
       if (ifft==0) cycle
       cv(:)=cv(:)+en%f(i1,i2,is)*Xk%weights(i2)*abs(wf(:,ifft))**2.
     enddo
   enddo
 enddo
 rho_syms=nsym/(i_time_rev+1)
 if (use_si) rho_syms=nsym
 do i1=1,rho_syms
   rho(:)=rho(:)+real(cv(fft_rot_r(i1,:)))/real(nsym)
 enddo
 if (.not.use_si) rho=rho*(1+i_time_rev)
 end subroutine
!
