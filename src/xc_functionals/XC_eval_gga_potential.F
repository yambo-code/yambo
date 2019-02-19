!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG
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
subroutine XC_eval_gga_potential(vxc_tot,v_rho,v_drho)
  !
  ! Given the partial derivatives, v_rho, v_drho, provide the vxc_tot:
  !
  ! 1. Gv_drho(r) = -Grad \cdot v_drho(r,3)
  ! 2. vxc_tot(r) = v_rho(r) + Gv_drho(r)   
  ! 
  use pars,          ONLY:SP,DP,cI_DP,PI_DP
  use R_lattice,     ONLY:g_vec
  use D_lattice,     ONLY:alat
  use wave_func,     ONLY:wf_ng
  use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table
#if defined _FFTW
  use FFT_m,         ONLY:fftw_plan
#endif
  implicit none
  !
  real(SP),intent(inout) :: vxc_tot(fft_size)
  real(SP),intent(in) :: v_rho(fft_size), v_drho(3,fft_size)
  !
  ! Work space
  !
  integer       :: ic
  real(SP)      :: Gv_drho(fft_size)
  complex(DP)   :: Vr(fft_size), Vg(wf_ng)
  !
  ! 1. this is done by FFTing, by doing the scalar
  !    product with G, and FFT-1 the result
  !
  Vg(1:wf_ng) = (0._DP,0._DP)
  !
  do ic = 1,3
    !
    Vr(:) = (0._DP,0._DP)
    Vr(:) = v_drho(ic,:)
    !
#if defined _FFTW
    call dfftw_destroy_plan(fftw_plan)
    fftw_plan = 0
    call fft_3d(Vr,fft_dim,-1,fftw_plan)
#else
    call fft_3d(Vr,fft_dim,-1)
#endif
    !
    Vg(1:wf_ng)=Vg(1:wf_ng)-cI_DP*(2._DP*PI_DP*real(g_vec(1:wf_ng,ic)/alat(ic),DP)) * &
&                           Vr(fft_g_table(1:wf_ng,1))/real(fft_size,DP)
    !
  end do    
  !
  Vr = (0._DP,0._DP)
  Vr(fft_g_table(1:wf_ng,1)) = Vg(1:wf_ng)
#if defined _FFTW
  call dfftw_destroy_plan(fftw_plan)
  fftw_plan = 0
  call fft_3d(Vr,fft_dim,1,fftw_plan)
#else
  call fft_3d(Vr,fft_dim,1)
#endif
  Gv_drho(:) = real(Vr(:),SP)
  !
  ! 2: sum up with the other partial derivative (and in case the other xc component)
  !
  vxc_tot = vxc_tot + v_rho + Gv_drho
  !
end subroutine XC_eval_gga_potential


