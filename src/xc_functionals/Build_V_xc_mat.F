!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine Build_V_xc_mat(V_xc,V_xc_mat)
 !
 use pars,           ONLY:SP,cZERO,cI,rZERO,tiny_dfl
 use FFT_m,          ONLY:fft_size
 use electrons,      ONLY:n_spin,n_spinor,n_sp_pol
 use xc_functionals, ONLY:magn
 use vec_operate,    ONLY:v_norm
 !
 implicit none
 !
 real(SP),    intent(in)  :: V_xc(fft_size,n_spin)
 complex(SP), intent(out) :: V_xc_mat(fft_size,n_spin,n_spin)
 !
 !  Work space
 !
 integer     :: ifft
 real(SP)    :: mod_mag,magn_versor(fft_size,3),vxc_plus(fft_size),vxc_minus(fft_size)
 !
 V_xc_mat=cZERO
 !
 ! V_xc_mat ( 1 , 1 ) = up up
 ! V_xc_mat ( 1 , 2 ) = up dn
 ! V_xc_mat ( 2 , 1 ) = dn up
 ! V_xc_mat ( 2 , 2 ) = dn dn
 !
 ! Magnetization, V_xc + and -
 !
 if (n_spin==1) then
   !
   V_xc_mat(:,1,1)=V_xc(:,1)
   !
 else if (n_sp_pol==2) then
   !
   V_xc_mat(:,1,1)= V_xc(:,1)
   V_xc_mat(:,2,2)= V_xc(:,2)
   !
 else if (n_spinor==2) then
   !
   magn_versor=rZERO
   do ifft=1,fft_size
     mod_mag=v_norm(magn(ifft,:))
     if(mod_mag<tiny_dfl) magn_versor(ifft,:)=rZERO
     if(mod_mag>=tiny_dfl) magn_versor(ifft,:)=magn(ifft,:)/mod_mag
   enddo
   !
   ! Vxc^+=(Vxc_up+Vxc_down)/2
   ! Vxc^+=(Vxc_up-Vxc_down)/2
   vxc_plus(:) =(V_xc(:,1)+V_xc(:,2))/2._SP
   vxc_minus(:)=(V_xc(:,1)-V_xc(:,2))/2._SP
   !
   V_xc_mat(:,1,1)= (vxc_plus(:)+vxc_minus(:)*magn_versor(:,3))
   V_xc_mat(:,2,2)= (vxc_plus(:)-vxc_minus(:)*magn_versor(:,3))
   !
   V_xc_mat(:,1,2)= vxc_minus(:)*(magn_versor(:,1)-cI*magn_versor(:,2))
   V_xc_mat(:,2,1)= vxc_minus(:)*(magn_versor(:,1)+cI*magn_versor(:,2))
   !
 endif
 !
end subroutine Build_V_xc_mat
