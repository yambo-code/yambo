!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine Build_F_xc_mat(V_xc,F_xc,F_xc_mat)
 !
 use pars,           ONLY:SP,cZERO,cI
 use BS,             ONLY:l_BS_magnons
 use FFT_m,          ONLY:fft_size
 use electrons,      ONLY:n_spin,n_spinor,n_sp_pol
 use D_lattice,      ONLY:DL_vol
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
 real(SP)    :: mod_mag,mod_mag_n,magn_versor(fft_size,3),magn_inv(fft_size),&
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
     ! The following is in principle ok, but slightly less precise
     ! than the same operation done in XC_eval_LDA_kernel with DP quantities
     ! There F_xc(:,1:2,1:2) is filled and directly used here
     ! However, XC_eval_LDA_kernel works for LDA alone. The code below
     ! would work also for GGA 
     !
     ! DEBUG<
     !Vxc_mag(:)  =(V_xc(:,1)-V_xc(:,2))/2._SP
     !!
     !do ifft=1,fft_size
     !  mod_mag=abs(magn(ifft,3))
     !  !The same factor is also used in XC_eval_LDA_kernel
     !  mod_mag=mod_mag*real(fft_size,SP)/DL_vol
     !  if(mod_mag< 1.E-6_SP) magn_inv(ifft)=0._SP
     !  if(mod_mag>=1.E-6_SP) magn_inv(ifft)=1._SP/mod_mag
     !enddo
     !!
     !F_xc_mat(:,1,2,1,2)= cZERO
     !F_xc_mat(:,1,2,2,1)= 2*Vxc_mag*magn_inv
     !F_xc_mat(:,2,1,1,2)= 2*Vxc_mag*magn_inv
     !F_xc_mat(:,2,1,2,1)= cZERO
     ! DEBUG>
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
   ! At variance with the case n_sp_pol=2, here F_xc(:,1:2,1:2 from XC_eval_LDA_kernel
   ! here contains v2rho2, e.g. the second order derivative w.r.t. the density
   ! Accordingly, the term proportional to the V_xc(:,1)-V_xc(:,2) is added here
   !
   magn_versor=0._SP
   do ifft=1,fft_size
     mod_mag_n=v_norm(magn(ifft,:))
     ! The same factor is also used in XC_eval_LDA_kernel
     mod_mag=mod_mag_n*real(fft_size,SP)/DL_vol
     if(mod_mag< 1.E-6_SP) magn_versor(ifft,:)=0._SP
     if(mod_mag>=1.E-6_SP) magn_versor(ifft,:)=magn(ifft,:)/mod_mag_n
     if(mod_mag< 1.E-6_SP) magn_inv(ifft)     =0._SP
     if(mod_mag>=1.E-6_SP) magn_inv(ifft)     =1._SP/mod_mag
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
   ! DEBUG<
   !F_xc_mat(:,1,1,1,1)= F_xc(:,1,1)          ! (opt)
   !F_xc_mat(:,1,1,2,2)= F_xc(:,1,2)          ! (opt)
   !F_xc_mat(:,2,2,1,1)= F_xc(:,2,1)          ! (opt)
   !F_xc_mat(:,2,2,2,2)= F_xc(:,2,2)          ! (opt)
   !!
   !F_xc_mat(:,1,2,1,2)= cZERO                ! (mag)
   !F_xc_mat(:,1,2,2,1)= 2*Vxc_mag*magn_inv   ! (mag)
   !F_xc_mat(:,2,1,1,2)= 2*Vxc_mag*magn_inv   ! (mag)
   !F_xc_mat(:,2,1,2,1)= cZERO                ! (mag)
   ! DEBUG>
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
 ! DEBUG<
 !write(*,*) 2*Vxc_mag(1:10)*magn_inv(1:10)
 !!
 !write(*,*) real(F_xc_mat(1:10,1,1,1,1),SP)
 !write(*,*) real(F_xc_mat(1:10,1,1,2,2),SP)
 !write(*,*) real(F_xc_mat(1:10,2,2,1,1),SP)
 !write(*,*) real(F_xc_mat(1:10,2,2,2,2),SP)
 !!
 !write(*,*) real(F_xc_mat(1:10,1,2,1,2),SP)
 !write(*,*) real(F_xc_mat(1:10,1,2,2,1),SP)
 !write(*,*) real(F_xc_mat(1:10,2,1,1,2),SP)
 !write(*,*) real(F_xc_mat(1:10,2,1,2,1),SP)
 ! DEBUG>
 !
end subroutine Build_F_xc_mat
