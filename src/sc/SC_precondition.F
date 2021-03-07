!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DV
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
subroutine SC_precondition(delta_rho)
  !
  ! Thomas Fermi precondition for the mixing of the charge density
  ! delta_rho(G)=delta_rho(G)*G**2/(G**2+q_TF**2) 
  !
 use pars,          ONLY: pi,DP,SP
 use R_lattice,     ONLY: bare_qpg
 use D_lattice,     ONLY: DL_vol
 use fft_m,         ONLY: fft_size,fft_dim,fftw_plan,fft_g_table
 use wave_func,     ONLY: wf_ng
 use electrons,     ONLY: nel
 !
 implicit none
 real :: delta_rho(fft_size)
 !
 ! Work Space
 !
 real(SP)    ::  rs
 real(SP)    :: qTF2
 complex(DP) :: D_rho_G(fft_size)
 !
 D_rho_G  = delta_rho
 !
 ! Define Qtf 
 !
 rs = (3.*DL_vol/4./pi/nel)**(1./3.)
 qtf2 = (12/pi)**(2./3.)/rs 
 !
 ! FFT call: delta_rho in reciprocal space
 !
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(D_rho_G,fft_dim,-1,fftw_plan)
#else
 call fft_3d(D_rho_G,fft_dim,-1)
#endif
 !
 D_rho_G(fft_g_table(2:wf_ng,1))=D_rho_G(fft_g_table(2:wf_ng,1))*bare_qpg(1,2:wf_ng)**2./(bare_qpg(1,2:wf_ng)**2.+qTF2)
 D_rho_G(1) = (0.,0.)
 !
 ! FFT call: delta_rho in direct space
 !
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(D_rho_G,fft_dim,1,fftw_plan)
#else
 call fft_3d(D_rho_G,fft_dim,1)
#endif
 !
 fftw_plan = 0
 !
 delta_rho = real(D_rho_G)/fft_size
 !
end subroutine SC_precondition
