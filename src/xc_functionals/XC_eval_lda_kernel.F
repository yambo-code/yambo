!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine XC_eval_lda_kernel(v1rho,v2rho2)
 !
 use pars,           ONLY:SP,DP
 use xc_functionals, ONLY:F_xc,magn,xc_spin,xc_spin_map1
 use electrons,      ONLY:n_spin,n_spinor,n_sp_pol
 use FFT_m,          ONLY:fft_size
 use BS,             ONLY:l_BS_magnons
 use D_lattice,      ONLY:DL_vol
 use vec_operate,    ONLY:v_norm
 !
 implicit none
 !
 real(DP), intent(in) :: v1rho(n_spin,fft_size),v2rho2(xc_spin(1),fft_size)
 !
 integer :: is1,is2,is_xc,ifft
 real(SP):: one_over_magn,mod_mag
 !
 if (n_spin==1) then
   forall(ifft=1:fft_size) F_xc(ifft,1,1) = F_xc(ifft,1,1) + real(v2rho2(1,ifft),SP)
 endif
 !
 if (n_sp_pol==2) then
   ! Optical Fxc
   if(.not.l_BS_magnons) then
     do is1 = 1,n_sp_pol
       do is2 = 1,n_sp_pol
         is_xc = xc_spin_map1(is1,is2)
         forall(ifft=1:fft_size) F_xc(ifft,is1,is2) = F_xc(ifft,is1,is2) + real(v2rho2(is_xc,ifft),SP)
       enddo
     enddo
#if defined _KERR
   ! Magnons Fxc
   else
     do ifft=1,fft_size
       mod_mag=v_norm(magn(ifft,:))
       if(mod_mag==0._SP) one_over_magn=0._SP
       if(mod_mag/=0._SP) one_over_magn=1._SP/mod_mag
       ! Here 1,1 means +- and 22 -+ (or viceversa, to be checked)
       F_xc(ifft,1,1) = F_xc(ifft,1,1) + real(v1rho(1,ifft)-v1rho(2,ifft),SP)*one_over_magn
       F_xc(ifft,2,2) = F_xc(ifft,2,2) + real(v1rho(1,ifft)-v1rho(2,ifft),SP)*one_over_magn
     enddo
     ! Here 2,1 means -- and 12 ++ (or viceversa, to be checked)
     F_xc(:,2,1)=F_xc(:,1,2)
#endif
   endif
 endif
 !
 if (n_spinor==2) then
   ! To be coded ...
   ! Here F_xc has 4 spin indexes. The old one is wrong
   do is1 = 1,n_sp_pol
     do is2 = 1,n_sp_pol
       is_xc = xc_spin_map1(is1,is2)
       forall(ifft=1:fft_size) F_xc(ifft,is1,is2) = F_xc(ifft,is1,is2) + real(v2rho2(is_xc,ifft),SP)
     enddo
   enddo
 endif
 !
end subroutine
