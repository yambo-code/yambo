!
! Copyright (C) 2000-2013 M. Gruning and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine XC_libxc_driver(en,Xk,KIND,FUNCTIONAL,ORDER)
 !
 ! After libxc is distributed together with Yambo (rev 1180)  
 ! this routine evaluates the xc functional in yambo 
 ! (03/05/12: LDA OK, GGA OK => EXTENSIVE TESTS NEEDED) 
 !
 use pars,          ONLY:SP,DP,PI
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:DL_vol, alat
 use com,           ONLY:msg,warning
#if defined _RT
 use SC,            ONLY:rho_n
#endif
 use electrons,     ONLY:levels,n_spin,n_spinor
 use FFT_m,         ONLY:fft_size
 use wrapper,        ONLY:real_V_dot_V
 use xc_functionals,ONLY:E_xc,F_xc,V_xc,magn,&
&                        xc_string,XC_LDA_C_KP,xc_fnctl_t,xc_setup_fnctl
 use xc_f90_lib_m   ,  ONLY:XC_FAMILY_LDA,XC_FAMILY_GGA
 implicit none
 type(levels) ::en
 type(bz_samp)::Xk
 integer      ::KIND,FUNCTIONAL,ORDER
 !
 ! Work Space
 !
 integer      :: i1,i2,is,ic,is1,is2,ixc,ifft
 real(SP)  :: mod_magn
 !
 ! XC routines arrays
 !
 real(SP) :: spin_rho_SP(fft_size,n_spin),rspts(fft_size),rho(fft_size)
 real(DP) :: spin_rho_DP(n_spin,fft_size)
 real(SP),allocatable:: drho(:,:,:)
 real(DP),allocatable:: sigma(:,:)
 !
 ! note that multidimensional dvxc & vxc is allowed only in xcspol
 !
 real(DP) :: tmp_dvxc(fft_size),tmp_vxc(n_spin,fft_size),tmp_exc(fft_size)
 real(SP),allocatable :: vdrho(:,:)
 real(DP),allocatable:: vsigma(:,:)
 !
 ! Functional and Functional infos
 !
 type(xc_fnctl_t) :: fnctl(2) 
 !
 !=============================== 
 ! Convert and Initialize
 !===============================
 !
 tmp_exc=0._DP
 tmp_vxc=0._DP
 tmp_dvxc=0._DP
 !
 call xc_setup_fnctl(fnctl,FUNCTIONAL,n_spin)
 !
 !==========================
 ! Electronic/spin density 
 !==========================
 !
#if defined _RT
 do i1=1,n_spin
   spin_rho_SP(:,i1)=rho_n(:)
 enddo
#else
 !
 call el_density(en,Xk,rho,.false.)
 !
 if(n_spin==1) spin_rho_SP(:,1)=rho(:)
 if(n_spin> 1) then
   call el_magnetization(en,Xk)
   if (n_spinor==1) then
     do ifft=1,fft_size
       spin_rho_SP(ifft,1)=0.5*(rho(ifft)+magn(ifft,3))
       spin_rho_SP(ifft,2)=0.5*(rho(ifft)-magn(ifft,3))
     enddo
   else
     do ifft=1,fft_size
       mod_magn=sqrt(magn(ifft,1)**2+magn(ifft,2)**2+magn(ifft,3)**2)
       spin_rho_SP(ifft,1)=0.5*(rho(ifft)+mod_magn)
       spin_rho_SP(ifft,2)=0.5*(rho(ifft)-mod_magn)
     enddo
   endif
 endif
#endif
 !
 forall(i1=1:fft_size,i2=1:n_spin)  spin_rho_DP(i2,i1)=spin_rho_SP(i1,i2)*real(fft_size,DP)/real(DL_vol,DP)
 !
 !=============================
 ! Electronic density gradient
 !=============================
 !
 if (any(fnctl(:)%family==XC_FAMILY_GGA)) then 
   allocate(drho(3,n_spin,fft_size),sigma(n_spin*(n_spin+1)/2,fft_size))
   if (ORDER==1) allocate(vdrho(3,fft_size),vsigma(n_spin*(n_spin+1)/2,fft_size))
   !
   do is1 = 1,n_spin
     call eval_Gradient(spin_rho_SP(:,is1),drho(:,is1,:))
     forall (i1=1:fft_size) drho(:,is1,i1) = drho(:,is1,i1)*real(fft_size,DP)/real(DL_vol,DP)*2._SP*PI/alat(:)
   end do
   !
   do i1 = 1,fft_size
     do is1 = 1,n_spin
       do is2 = is1,n_spin
         is = is2*(is1+1)/2
         sigma(is,i1) = real(real_V_dot_V(3,drho(:,is1,i1),drho(:,is2,i1)),DP)
       end do
     end do
   end do
   !
 endif
 !
 !===========================
 ! Evaluate the xc functional
 !===========================
 !
 if (order==0) E_xc=0.
 if (order==1) V_xc=0.
 if (order==2) F_xc=0.
 !
 ! GPL_EXCLUDE_START
 !
 if (FUNCTIONAL == XC_LDA_C_KP) then ! Special case, XC_LDA_C_KP not available from libxc
   forall (i1=1:fft_size) rspts(i1)=(3._DP/4._DP/pi/spin_rho_SP(i1,1)/DL_vol)**(1._DP/3._DP)
   call xc_rpa_kp(rspts,tmp_exc)
   E_xc = tmp_exc 
   return
 end if
 !
 ! GPL_EXCLUDE_END
 !
 do ixc = 1,2
   if (fnctl(ixc)%id == 0) cycle
   if (fnctl(ixc)%family==XC_FAMILY_LDA) then 
     select case(ORDER)
       case(0)
         call xc_f90_lda_exc(fnctl(ixc)%conf, fft_size, spin_rho_DP(1,1), tmp_exc(1))
         E_xc = E_xc + tmp_exc    
       case(1)
         call xc_f90_lda_vxc(fnctl(ixc)%conf, fft_size, spin_rho_DP(1,1), tmp_vxc(1,1))
         forall(i1=1:fft_size,i2=1:n_spin) V_xc(i1,i2) = V_xc(i1,i2) + tmp_vxc(i2,i1)         
       case(2) 
         call xc_f90_lda_fxc(fnctl(ixc)%conf, fft_size, spin_rho_DP(1,1), tmp_dvxc(1))
         F_xc = F_xc + tmp_dvxc
     end select
   elseif (fnctl(ixc)%family==XC_FAMILY_GGA) then
     select case(ORDER)
       case(0)
         call xc_f90_gga_exc(fnctl(ixc)%conf, fft_size, spin_rho_DP(1,1), sigma(1,1), tmp_exc(1))
         E_xc = E_xc + tmp_exc    
       case(1)
         call xc_f90_gga_vxc(fnctl(ixc)%conf, fft_size, spin_rho_DP(1,1), sigma(1,1), tmp_vxc(1,1), vsigma(1,1))
         do is1 = 1,n_spin
           vdrho(:,:) = 0._SP 
           do is2 = 1,n_spin
             is = min(is2*(is1+1)/2,is1*(is2+1)/2)
             forall(ic=1:3,i1=1:fft_size) vdrho(ic,i1) = vdrho(ic,i1) + &
                  & 2._SP*real(vsigma(is,i1),SP)*drho(ic,is2,i1)
           end do
           call XC_eval_gga_potential(V_xc(:,is1),real(tmp_vxc(is1,:),SP),vdrho)
         end do
           !
     end select
   end if
 enddo
 !
 if (any(fnctl(:)%family==XC_FAMILY_GGA)) deallocate(sigma,drho)
 if (any((fnctl(:)%family==XC_FAMILY_GGA)).and.(ORDER==1)) deallocate (vdrho,vsigma)
 !
end subroutine XC_libxc_driver

