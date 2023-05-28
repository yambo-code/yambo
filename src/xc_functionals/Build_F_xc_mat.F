!
!        Copyright (C) 2000-2023 the YAMBO team
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
subroutine Build_F_xc_mat(V_xc,F_xc,F_xc_mat)
 !
 use pars,           ONLY:SP,cZERO,cI
 use BS,             ONLY:l_BS_magnons
 use FFT_m,          ONLY:fft_size
 use electrons,      ONLY:n_spin,n_spinor,n_sp_pol
 use xc_functionals, ONLY:magn
 use vec_operate,    ONLY:v_norm
 !
 implicit none
 !
 real(SP),    intent(in)  :: V_xc(fft_size,n_spin)
 real(SP),    intent(in)  :: F_xc(fft_size,n_spin,n_spin)
 complex(SP), intent(out) :: F_xc_mat(fft_size,n_spin,n_spin,n_spin,n_spin)
 !
 !  Work space
 !
 integer     :: ifft
 real(SP)    :: mod_mag,magn_versor(fft_size,3),magn_inv(fft_size),&
 &              Fxc_rho2(fft_size),Fxc_mag2(fft_size),Fxc_rhom(fft_size),Vxc_mag(fft_size)
 !
 F_xc_mat=cZERO
 !
 ! F_xc_mat ( 1 , 1 , 1 , 1 ) = up up up up  (opt)
 !
 ! F_xc_mat ( 1 , 1 , 1 , 2 ) = up up up dn  (mix)
 ! F_xc_mat ( 1 , 1 , 2 , 1 ) = up up dn up  (mix)
 ! F_xc_mat ( 1 , 2 , 1 , 1 ) = up dn up up  (mix)
 ! F_xc_mat ( 2 , 1 , 1 , 1 ) = dn up up up  (mix)
 !
 ! F_xc_mat ( 1 , 1 , 2 , 2 ) = up up dn dn  (opt)
 ! F_xc_mat ( 1 , 2 , 1 , 2 ) = up dn up dn  (mag)
 ! F_xc_mat ( 2 , 1 , 1 , 2 ) = dn up dn dn  (mag)
 ! F_xc_mat ( 1 , 2 , 2 , 1 ) = up dn dn up  (mag)
 ! F_xc_mat ( 2 , 1 , 2 , 1 ) = dn up dn up  (mag)
 ! F_xc_mat ( 2 , 2 , 1 , 1 ) = dn dn up up  (opt)
 !
 ! F_xc_mat ( 1 , 2 , 2 , 2 ) = up dn dn dn  (mix)
 ! F_xc_mat ( 2 , 1 , 2 , 2 ) = dn up dn dn  (mix)
 ! F_xc_mat ( 2 , 2 , 1 , 2 ) = dn dn up dn  (mix)
 ! F_xc_mat ( 2 , 2 , 2 , 1 ) = dn dn dn up  (mix)
 !
 ! F_xc_mat ( 2 , 2 , 2 , 2 ) = dn dn dn dn  (opt)
 !
 !
 ! Magnetization, V_xc + and -
 !
 if (n_spin==1) then
   !
   F_xc_mat(:,1,1,1,1)=F_xc(:,1,1)
   !
 else if (n_sp_pol==2) then
   !
   if (.not.l_BS_magnons) then
     !
     F_xc_mat(:,1,1,1,1)= F_xc(:,1,1)
     F_xc_mat(:,1,1,2,2)= F_xc(:,1,2)
     F_xc_mat(:,2,2,1,1)= F_xc(:,2,1)
     F_xc_mat(:,2,2,2,2)= F_xc(:,2,2)
     !
   else
     !
     ! The following is in principle ok
     ! In practice, due to numerical noise, it gives wrong results
     ! 
     !Vxc_mag(:)  =(V_xc(:,1)-V_xc(:,2))/2._SP
     !!
     !do ifft=1,fft_size
     !  mod_mag=abs(magn(ifft,3))
     !  if(mod_mag==0._SP) magn_inv(ifft)=0._SP
     !  if(mod_mag/=0._SP) magn_inv(ifft)=1._SP/mod_mag
     !enddo
     !!
     !F_xc_mat(:,1,2,1,2)= cZERO
     !F_xc_mat(:,1,2,2,1)= F_xc(:,1,2) !2*Vxc_mag*magn_inv
     !F_xc_mat(:,2,1,1,2)= F_xc(:,2,1) !2*Vxc_mag*magn_inv
     !F_xc_mat(:,2,1,2,1)= cZERO
     !
     F_xc_mat(:,1,2,1,2)= F_xc(:,1,1)
     F_xc_mat(:,1,2,2,1)= F_xc(:,1,2)
     F_xc_mat(:,2,1,1,2)= F_xc(:,2,1)
     F_xc_mat(:,2,1,2,1)= F_xc(:,2,2)
     !
   endif
   !
 else if (n_spinor==2) then
   !
   magn_versor=0._SP
   do ifft=1,fft_size
     mod_mag=v_norm(magn(ifft,:))
     if(mod_mag==0._SP) magn_versor(ifft,:)=0._SP
     if(mod_mag/=0._SP) magn_versor(ifft,:)=magn(ifft,:)/mod_mag
     if(mod_mag==0._SP) magn_inv(ifft)     =0._SP
     if(mod_mag/=0._SP) magn_inv(ifft)     =1._SP/mod_mag
   enddo
   !
   Vxc_mag(:)  =(V_xc(:,1)-V_xc(:,2))/2._SP
   !
   Fxc_rho2(:) =(F_xc(:,1,1)+F_xc(:,2,2)+F_xc(:,1,2)+F_xc(:,2,1))/4._SP
   Fxc_mag2(:) =(F_xc(:,1,1)+F_xc(:,2,2)-F_xc(:,1,2)-F_xc(:,2,1))/4._SP-Vxc_mag*magn_inv*2._SP  ! This also includes the term 2*Vxc_mag/mod_magn
   Fxc_rhom(:) =(F_xc(:,1,1)-F_xc(:,2,2))/4._SP
   !
   F_xc_mat(:,1,1,1,1) = (Fxc_mag2+Vxc_mag*magn_inv)*magn_versor(:,3)**2+Vxc_mag*magn_inv+2*Fxc_rhom+Fxc_rho2   !  (opt)
   F_xc_mat(:,1,1,2,2) =-(Fxc_mag2+Vxc_mag*magn_inv)*magn_versor(:,3)**2-Vxc_mag*magn_inv           +Fxc_rho2   !  (opt)
   F_xc_mat(:,2,2,1,1) =-(Fxc_mag2+Vxc_mag*magn_inv)*magn_versor(:,3)**2-Vxc_mag*magn_inv           +Fxc_rho2   !  (opt)
   F_xc_mat(:,2,2,2,2) = (Fxc_mag2+Vxc_mag*magn_inv)*magn_versor(:,3)**2+Vxc_mag*magn_inv-2*Fxc_rhom+Fxc_rho2   !  (opt)
   !
   F_xc_mat(:,1,2,1,2) = (Fxc_mag2+Vxc_mag*magn_inv)*(magn_versor(:,1)-cI*magn_versor(:,2))**2                      !  (mag)
   F_xc_mat(:,1,2,2,1) = (Fxc_mag2+Vxc_mag*magn_inv)*(magn_versor(:,1)**2+magn_versor(:,2)**2)+2*Vxc_mag*magn_inv   !  (mag)
   F_xc_mat(:,2,1,1,2) = (Fxc_mag2+Vxc_mag*magn_inv)*(magn_versor(:,1)**2+magn_versor(:,2)**2)+2*Vxc_mag*magn_inv   !  (mag)
   F_xc_mat(:,2,1,2,1) = (Fxc_mag2+Vxc_mag*magn_inv)*(magn_versor(:,1)+cI*magn_versor(:,2))**2                      !  (mag)
   !
   ! TO DO !!
   !
   F_xc_mat(:,1,1,1,2) = cZERO     !  (mix)
   F_xc_mat(:,1,1,2,1) = cZERO     !  (mix)
   F_xc_mat(:,1,2,1,1) = cZERO     !  (mix)
   F_xc_mat(:,2,1,1,1) = cZERO     !  (mix)
   !
   F_xc_mat(:,1,2,2,2) = cZERO     !  (mix)
   F_xc_mat(:,2,1,2,2) = cZERO     !  (mix)
   F_xc_mat(:,2,2,1,2) = cZERO     !  (mix)
   F_xc_mat(:,2,2,2,1) = cZERO     !  (mix)
   !
 endif
 !
end subroutine Build_F_xc_mat
