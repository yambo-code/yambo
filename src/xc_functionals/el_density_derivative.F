!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team
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
subroutine el_density_derivative(rho,rho_derivative)
 !
 use pars,          ONLY:SP
 use FFT_m,         ONLY:fft_size,fftw_plan,fft_g_table
 use R_lattice,     ONLY:g_vec
 use wave_func,     ONLY:wf_ng
 !
 implicit none
 real(SP)     ::rho(fft_size)
 real(SP)     ::rho_derivative(3,fft_size)
 !
 ! Work Space 
 !
 integer      ::ig,ic
 real(SP)     ::rho_SAVE(fft_size)
 !
 rho_SAVE=rho
 !
 fftw_plan=0
#if defined _FFTW
 call fft_3d(rho_SAVE,fft_size,-1,fftw_plan)
#else
 call fft_3d(rho_SAVE,fft_size,-1)
#endif
 !
 fftw_plan=0
 !
 do ic=1,3
   do ig=1,wf_ng
     rho_derivative(ic,ig)=g_vec(ig,ic)*rho_SAVE(fft_g_table(ig,1))
   enddo
   !
#if defined _FFTW
   call fft_3d(rho_derivative(ic,:),fft_size,1,fftw_plan)
#else
   call fft_3d(rho_derivative(ic,:),fft_size,1)
#endif
   !
 enddo
 !
end subroutine
