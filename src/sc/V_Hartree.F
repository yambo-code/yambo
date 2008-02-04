!
! Copyright (C) 2000-2005 M. Gruening and the SELF team
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
subroutine V_Hartree(rho,vhr)
  !
  !  Hartree potential (vhr)
  !  sum_G 4pi*rho(G)/|G|**2.  with G!=0, for G=0 vhr(0)=0 
  !
 use pars,          ONLY: pi,DP
 use R_lattice,     ONLY: bare_qpg
 use D_lattice,     ONLY: DL_vol
 use fft_m,         ONLY: fft_size,fft_dim,fftw_plan,fft_g_table
 use wave_func,     ONLY: wf_ng
 !
 implicit none
 real :: rho(fft_size)
 real :: vhr(fft_size)
 !
 ! Work Space
 !
 complex(DP) :: rhog(fft_size), vhtmp(fft_size) 
 complex     :: vhg(wf_ng)
 !
 vhg   = (0.,0.)
 vhtmp = (0.,0.)
 rhog  = rho
 !
 ! FFT call: rho in reciprocal space
 !
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(rhog,fft_dim,-1,fftw_plan)
#else
 call fft_3d(rhog,fft_dim,-1)
#endif
 !
 vhg(2:wf_ng)=((4.*pi/DL_vol)*(rhog(fft_g_table(2:wf_ng,1))))&
&              /bare_qpg(1,2:wf_ng)**2.
 !
 vhtmp(fft_g_table(1:wf_ng,1))=vhg(1:wf_ng)
 !
 ! FFT call: vhr in direct space
 !
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(vhtmp,fft_dim,1,fftw_plan)
#else
 call fft_3d(vhtmp,fft_dim,1)
#endif
 !
 fftw_plan = 0
 !
 vhr = real(vhtmp)
 !
end subroutine V_hartree
