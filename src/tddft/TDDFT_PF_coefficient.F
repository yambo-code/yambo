!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine TDDFT_PF_coefficient(en,Xk,a_xc,b_xc)
 !
 ! Evaluate the alpha for the polarization functional 
 ! using different approximations
 !
 use pars,          ONLY:pi,SP,DP
 use X_m,           ONLY:Epsilon_ii
 use interfaces,    ONLY:el_density_and_current
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:DL_vol
 use electrons,     ONLY:levels,nel,n_spin,n_sp_pol
 use com,           ONLY:msg
 use tddft,         ONLY:Fxc_PF_alpha
 use FFT_m,         ONLY:fft_size, fft_dim,fftw_plan,fft_g_table
 implicit none
 !
 real(SP),intent(out) :: a_xc
 real(SP),intent(out) :: b_xc
 type(bz_samp)::Xk
 type(levels) ::en       
 !
 real(SP), allocatable :: rho(:)
 !
 b_xc = 0._SP
 !	
 select case(Fxc_PF_alpha)
   case('CUR')
     a_xc = Alpha_BergerStatic(real(Epsilon_ii(1,2)))
   case('JGM')
     if (fft_size==0) call fft_setup(0,1,.true.)
     allocate(rho(fft_size))
     rho(:) = 0._SP
     call el_density_and_current(en,Xk,rho=rho)
     a_xc = Alpha_JGM(rho,en%E_ind_gap(1))
   case('LOR')
     call Alpha_LOR(real(Epsilon_ii(1,2)),a_xc,b_xc)
   case('RBO')
     a_xc = Alpha_BootStrap(real(Epsilon_ii(1,1)),real(Epsilon_ii(1,2)))
   case('EMP')
     a_xc = Alpha_Empirical(real(Epsilon_ii(1,2)))
   case('LOC')
     if (fft_size==0) call fft_setup(0,1,.true.)
     allocate(rho(fft_size))
     rho(:) = 0._SP
     call el_density_and_current(en,Xk,rho=rho)
     a_xc = Alpha_LOC(rho)
   case default
     a_xc = 0._SP
 end select
 !
 call msg('nrs','[TDDFT] Static coefficient for polarization function ',a_xc)
 !
contains
   !
   real function Alpha_BergerStatic(epsM0)
     !
     ! Berger: using the RPA susceptibility following eq. 12 in PRL 115, 137402
     !         (=> Later to be add eq. 13-14 same paper)
     real(SP):: Xe0, epsM0
     !
     Xe0 = (epsM0 - 1._SP)/(4._SP*pi)
     Alpha_BergerStatic = 1._SP/(epsM0*Xe0)
     !
   end function Alpha_BergerStatic
   !
   real function Alpha_Empirical(epsM0)
     ! Botti PRB 72, 125203
     !
     real(SP):: epsM0
     real(SP), parameter :: A=4.615, B=-0.213
     !
     Alpha_Empirical = B+A/epsM0
     !
   end function Alpha_Empirical
   !
   real function Alpha_BootStrap(epsM0,epsMRPA)
     ! Rigamonti PRL 2015
     ! 
     real(SP):: Xe0, epsM0,epsMRPA
     !
     Xe0 = (epsM0 - 1._SP)/(4._SP*pi)
     Alpha_BootStrap = 1._SP/(epsMRPA*Xe0)
     !
   end function Alpha_BootStrap
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
   real function Alpha_JGM_RPA(density,Egap)   
     !  Trevisanutto et al (PRB 87, 205143) within RPA
     !
     integer :: ir
     real(SP) :: alpha_, Egap,n
     real(SP) :: density(fft_size)
     !
     alpha_ = 0._SP
     do ir = 1,fft_size
       n = density(ir)*real(fft_size,sP)/DL_vol
       alpha_ = alpha_ + Egap**2/n
     end do
     Alpha_JGM_RPA = alpha_/real(fft_size,SP)
     !
   end function Alpha_JGM_RPA
   !
   real function Alpha_JGM_R(density_r,Egap)
     ! JMG: Jellium model with gap from Trevisanutto et al (PRB 87, 205143)
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
   real function Alpha_JGM_alt(density,Egap)
     ! JMG: Jellium model with gap from Trevisanutto et al (PRB 87, 205143)
     ! Expectection value of the function Alpha_JGM_R from FT for test purposes
     integer :: ir
     complex(DP) :: alpha(fft_size)
     real(SP) :: density(fft_size),Egap
     !
     do ir = 1,fft_size
       alpha(ir)=  Alpha_JGM_R(density(ir),Egap)
     end do
     !
#if defined _FFTW
     call dfftw_destroy_plan(fftw_plan)
     fftw_plan = 0
     call fft_3d(alpha,fft_dim,-1,fftw_plan)
#else
     call fft_3d(alpha,fft_dim,-1)
#endif
     Alpha_JGM_alt = real(alpha(fft_g_table(1,1)),SP)/real(fft_size)
     !
   end function Alpha_JGM_alt
   !
   real function Alpha_JGM(density,Egap)
     ! JMG: Jellium model with gap from Trevisanutto et al (PRB 87, 205143)
     ! Expectation value of the function Alpha_JGM_R 
     integer :: ir
     real(SP) :: alpha_, B, Bgap, Rs, Egap, n
     real(SP) :: density(fft_size)
     !
     alpha_ = 0._SP
     do ir = 1,fft_size
       alpha_ = alpha_ + Alpha_JGM_R(density(ir),Egap)
     end do
     !
     Alpha_JGM =alpha_/real(fft_size,SP)  
     !
   end function Alpha_JGM
   !
   subroutine Alpha_LOR(epsM0,alpha,beta)
     ! Botti: only static now PRB 72, 125203, dynamic later on 
     !
     use units, ONLY:HA2EV
     real(SP) :: Wp, Egap, epsM0, alpha,beta
     real(SP), parameter :: A_Fit = 104.5/HA2EV 
     !
     Egap = Egap + sum(en%E(en%nbf+1,:,1)-en%E(en%nbf,:,1))/real(en%nk,SP)
     Wp = sqrt(4._SP*pi*nel/DL_vol)
     call msg('nrs','[TDDFT] Average Gap for Lorentzian model ',Egap*HA2EV)
     call msg('nrs','[TDDFT] Plasma frequency for Lorentzian model ',Wp*HA2EV)
     alpha = A_fit*Egap/(epsM0*Wp**2)
     beta = alpha/Egap**2
     !
   end subroutine
   !
   real function Alpha_LOC(density)
     ! Botti: only static now PRB 72, 125203, dynamic later on 
     ! Not working - seg fault in the eval_Gradient
     real(SP) :: density(fft_size),d_density(3,fft_size)
     real(SP) :: alpha_, mod_grad
     integer :: ir
     !
     call eval_Gradient(density(:),d_density)
     !
     alpha_ = 0._SP
     do ir = 1, fft_size
       mod_grad = sqrt(dot_product(d_density(:,ir),d_density(:,ir)))
       alpha_ = alpha_ + mod_grad/density(ir)
     end do
     !
     Alpha_LOC = -1.00778+ alpha_ *1.10507
     !
   end function Alpha_LOC
   !
end subroutine TDDFT_PF_coefficient

