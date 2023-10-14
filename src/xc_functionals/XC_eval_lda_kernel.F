!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine XC_eval_lda_kernel(v1rho,v2rho2)
 !
 use pars,           ONLY:SP,DP,cZERO
 use xc_functionals, ONLY:F_xc,magn,xc_spin,xc_spin_map1
 use electrons,      ONLY:n_spin,n_spinor,n_sp_pol
 use FFT_m,          ONLY:fft_size
 use BS,             ONLY:l_BS_magnons
 use D_lattice,      ONLY:DL_vol
 !
 implicit none
 !
 real(DP), intent(in) :: v1rho(n_spin,fft_size),v2rho2(xc_spin(1),fft_size)
 !
 integer :: is1,is2,is_xc,ifft
 real(SP):: one_over_magn,mod_mag
 !
 if (n_spin==1) then
   do ifft=1,fft_size
     F_xc(ifft,1,1) = F_xc(ifft,1,1) + real(v2rho2(1,ifft),SP)
   enddo
 endif
 !
 if (n_sp_pol==2) then
   if(.not.l_BS_magnons) then
     ! Optical Fxc
     do is1 = 1,n_sp_pol
       do is2 = 1,n_sp_pol
         is_xc = xc_spin_map1(is1,is2)
         forall(ifft=1:fft_size) F_xc(ifft,is1,is2) = F_xc(ifft,is1,is2) + real(v2rho2(is_xc,ifft),SP)
       enddo
     enddo
   else
     ! Magnons Fxc
     do ifft=1,fft_size
       mod_mag=abs(magn(ifft,3))*real(fft_size,SP)/DL_vol
       if(mod_mag< 1.E-8_SP) one_over_magn=0._SP
       if(mod_mag>=1.E-8_SP) one_over_magn=1._SP/mod_mag
       ! Here 1,2 means -+ and 21 +- (or viceversa, to be checked)
       F_xc(ifft,1,2) = F_xc(ifft,1,2) + real(v1rho(1,ifft)-v1rho(2,ifft),SP)*one_over_magn
       F_xc(ifft,2,1) = F_xc(ifft,2,1) + real(v1rho(1,ifft)-v1rho(2,ifft),SP)*one_over_magn
     enddo
     ! Here 1,1 means -- and 22 ++ (or viceversa, to be checked)
     F_xc(:,2,2)=F_xc(:,1,1)
   endif
 endif
 !
 if (n_spinor==2) then
   do is1 = 1,n_spinor
     do is2 = 1,n_spinor
       is_xc = xc_spin_map1(is1,is2)
       forall(ifft=1:fft_size) F_xc(ifft,is1,is2) = F_xc(ifft,is1,is2) + real(v2rho2(is_xc,ifft),SP)
     enddo
   enddo
 endif
 !
end subroutine
