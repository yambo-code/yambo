!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, DS
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
subroutine XC_eval_gga_potential(v1rho,v1sigma,drho)
  !
  ! Given the partial derivatives, v1rho, v1sigma, provide the vxc_tot:
  ! Remember sigma = drho(1)^2+drho(2)^2+drho(3)^2
  !
  ! 0   v_drho(r,:) = v1sigma(r) drho(r,1:3)
  ! 1. Gv_drho(r)   = -Grad(1:3) \cdot v_drho(r,1:3)
  ! 2. vxc_tot(r)   = v1rho(r) + G v_drho(r)   
  ! 
  use pars,          ONLY:SP,DP,cI_DP,PI_DP
  use R_lattice,     ONLY:g_vec
  use QP_m,          ONLY:QP_ng_Vxc
  use D_lattice,     ONLY:alat
  use electrons,     ONLY:n_spin
  use xc_functionals,ONLY:xc_spin,V_xc
  use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table
#if defined _FFTW
  use FFT_m,         ONLY:fftw_plan
#endif
  !
  implicit none
  !
  real(DP),intent(in) :: v1rho(n_spin,fft_size),v1sigma(xc_spin(1),fft_size)
  real(SP),intent(in) :: drho(fft_size,n_spin,3)
  !
  ! Work space
  !
  integer       :: ic,ifft,is1,is2,is_xc
  real(SP)      :: Gv_drho(fft_size),v_drho(fft_size,3)
  complex(DP)   :: Vr(fft_size), Vg(QP_ng_Vxc)
  !
  ! 0. define v_drho
  !
  do is1 = 1,n_spin
    v_drho = 0._SP 
    do is2 = 1,n_spin
      is_xc = max(is2*(is1+1)/2,is1*(is2+1)/2)
      forall(ic=1:3,ifft=1:fft_size) v_drho(ifft,ic) = v_drho(ifft,ic) + &
&                                         2._SP*real(v1sigma(is_xc,ifft),SP)*drho(ifft,is2,ic)
    end do
    !
    ! 1. this is done by FFTing, by doing the scalar
    !    product with G, and FFT-1 the result
    !
    Vg(1:QP_ng_Vxc) = (0._DP,0._DP)
    !
    do ic = 1,3
      !
      Vr = (0._DP,0._DP)
      Vr = v_drho(:,ic)
      !
#if defined _FFTW
      call dfftw_destroy_plan(fftw_plan)
      fftw_plan = 0
      call fft_3d(Vr,fft_dim,-1,fftw_plan)
#else
      call fft_3d(Vr,fft_dim,-1)
#endif
      !
      Vg(1:QP_ng_Vxc)=Vg(1:QP_ng_Vxc)-cI_DP*(2._DP*PI_DP*real(g_vec(1:QP_ng_Vxc,ic)/alat(ic),DP)) * &
&                             Vr(fft_g_table(1:QP_ng_Vxc,1))/real(fft_size,DP)
      !
    end do    
    !
    Vr = (0._DP,0._DP)
    Vr(fft_g_table(1:QP_ng_Vxc,1)) = Vg(1:QP_ng_Vxc)
#if defined _FFTW
    call dfftw_destroy_plan(fftw_plan)
    fftw_plan = 0
    call fft_3d(Vr,fft_dim,1,fftw_plan)
#else
    call fft_3d(Vr,fft_dim,1)
#endif
    Gv_drho = real(Vr,SP)
    !
    ! 2: sum up with the other partial derivative (and in case the other xc component)
    !
    V_xc(:,is1) = V_xc(:,is1) + real(v1rho(is1,:),SP) + Gv_drho(:)
    !
  enddo
  do ic=1,fft_size 
      write(50,*) ic,V_xc(ic,1), real(v1rho(1,ic),SP)
      write(51,*) ic,V_xc(ic,2), real(v1rho(2,ic),SP)
  enddo
  !
end subroutine XC_eval_gga_potential
