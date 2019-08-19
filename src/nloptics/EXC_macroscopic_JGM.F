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
subroutine EXC_macroscopic_JGM(en,Pmac,rho,Exc,V_xc)
  !
 use pars,          ONLY: pi,SP,DP,cI,cZERO
 use R_lattice,     ONLY: bare_qpg, g_vec
 use D_lattice,     ONLY: DL_vol, alat 
 use fft_m,         ONLY: fft_size,fft_dim,fft_g_table
 use nl_optics,     ONLY: NL_LRC_alpha
 use electrons,     ONLY: levels,n_spin
#if defined _FFTW
 use fft_m,         ONLY:fftw_plan
#endif
 use wave_func,     ONLY: wf_ng
 !
 implicit none
 real(SP),intent(in)  :: rho(fft_size)
 real(SP),intent(out) :: V_xc(fft_size,n_spin)
 complex(SP),intent(in) :: Pmac(3)
 complex(SP),intent(out) :: Exc(3)
 type(levels),intent(in) ::en 
 !
 ! Work Space
 !
 complex(DP) :: rhog(fft_size),alpha(fft_size),Vxc_tmp(fft_size)
 complex(DP) :: E_g(3)
 integer     :: ir, ig, igp
 !
 !
 ! Calculate alpha from the JGM in DL
 !
!$omp parallel do default(shared), private(ir)
 do ir = 1,fft_size
   alpha(ir)=  Alpha_JGM_R(rho(ir),en%E_ind_gap(1))
 end do
!$omp end parallel do
 !
 ! FT the alpha:
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan = 0
 call fft_3d(alpha,fft_dim,-1,fftw_plan)
#else
 call fft_3d(alpha,fft_dim,-1)
#endif
 NL_LRC_alpha = real(alpha(fft_g_table(1,1)),SP)/real(fft_size,SP)
 !
 ! First contribution to macroscopic Exc proportional to Macroscopic Polarization through  
 !
 Exc = NL_LRC_Alpha*Pmac !note that alpha is not changing very much (possible solution-> alpha at n_0 + FT once
 !                        
 ! Second contribution prop to microscopic longitudinal polarization: 
 ! from div p = -n => p(G) = -iG.n(G)/|G|^2. Need FT of density  
 !
 rhog        = rho
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan = 0
 call fft_3d(rhog,fft_dim,-1,fftw_plan)
#else
 call fft_3d(rhog,fft_dim,-1)
#endif
 !
 ! Contribution Micro->Macro
 !
!$omp parallel do reduction(+:Exc)
 do ig = 2,wf_ng
   Exc(:) = Exc(:)-cI*alpha(fft_g_table(ig,1))/real(fft_size,SP)*g_vec(ig,:)*2._SP*pi/alat(:) &
        &            *rhog(fft_g_table(ig,1))/DL_vol/bare_qpg(1,ig)**2/2._SP
 end do
!$omp end parallel do
 !
 ! Contribution Macro->Micro
 !
 ! The microscopic potential is related
 ! to the microscopic electric field  E_mic = - grad V
 ! Therefore V(g) = i E_g G /|G^2|
 !
 Vxc_tmp=cZERO
 !
!$omp parallel do default(shared), private(ig,E_g)
 do ig = 2,wf_ng
   !
   ! Macro->Micro
   E_g(:)=conjg(alpha(fft_g_table(ig,1)))/real(fft_size)*Pmac(:)/2._SP 
   !
   Vxc_tmp(fft_g_table(ig,1)) = -cI*sum(E_g(:)*g_vec(ig,:)*2._SP*pi/alat(:))/bare_qpg(1,ig)**2
   !
 enddo
!$omp end parallel do
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan = 0
 call fft_3d(Vxc_tmp,fft_dim,1,fftw_plan)
#else
 call fft_3d(Vxc_tmp,fft_dim,1)
#endif
 !
 V_xc(:,1)=real(Vxc_tmp,SP)
 if(n_spin==2) V_xc(:,2)=real(Vxc_tmp,SP)
 !
contains
  !
  real(SP) function Alpha_JGM_R(density_r,Egap)
    ! 
    real(SP) ::  B, Bgap, Rs, Egap, n
    real(SP) :: density_r
    !
    n = density_r/DL_vol*real(fft_size,SP)
    Rs = (3._SP/4._SP/pi/n)**(1._SP/3._SP)
    B = B_QMC(Rs)
    Bgap = (B + Egap)/(1._SP + Egap)
    Alpha_JGM_R =4._SP*pi*Bgap*(1._SP - exp(-Egap**2/(4._SP*pi*n*Bgap)))
  end function Alpha_JGM_R
  !
  real(SP) function B_QMC(Rs)
    ! PRL 75, 689
    real(SP),parameter:: a1=2.15_SP,a2=0.435_SP,b1=1.57_SP,b2=0.409_SP
    real(SP):: Rs, x
    !
    x = sqrt(Rs)
    B_QMC = (1._SP + a1*x + a2*x**3)/(3._SP + b1*x + b2*x**3)
    !
  end function B_QMC
  !
end subroutine EXC_macroscopic_JGM
