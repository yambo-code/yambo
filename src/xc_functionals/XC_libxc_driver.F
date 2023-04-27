!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, DS, DV
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
subroutine XC_libxc_driver(en,Xk,FUNCTIONAL,ORDER,EXX_FRACTION,EXX_SCREENING)
 !
 ! This routine evaluates the xc functional in yambo 
 !   - 03/05/12: LDA OK, GGA OK => EXTENSIVE TESTS NEEDED) 
 !   - 21/11/13: for HYB_GGA the GGA part calculated => TO TEST
 !   - 16/06/21: GGA part improved with more accurate calculation of drho for order=2 and n_spin=1
 !        To do: compute a more precise drho also in the case n_spin>1 (see comment around line 200)
 !
 use pars,           ONLY:SP,DP,IPL,pi,pi_DP,RZERO
 use com,            ONLY:msg
 use parser_m,       ONLY:parser
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:DL_vol,alat
 use drivers,        ONLY:l_real_time,l_nl_optics
#if defined _SC || defined _RT
 use hamiltonian,    ONLY:rho_n,magn_n
#endif
 use electrons,      ONLY:levels,n_spin,n_spinor
 use FFT_m,          ONLY:fft_size
 use wrapper,        ONLY:V_dot_V
 use BS,             ONLY:l_BS_magnons
 use xc_functionals, ONLY:E_xc,E_xc_val,magn,F_xc,V_xc,xc_setup_hyb,&
&                         XC_LDA_C_KP,xc_fnctl_t,xc_setup_fnctl,xc_f03_func_end, &
&                         xc_spin,xc_spin_map1, &
&                         XC_HYB_GGA_XC_GAUPBE,XC_FACTOR,&
&                         XC_FAMILY_LDA,XC_FAMILY_GGA,XC_FAMILY_HYB_GGA,&
&                         XC_GGA_X_PBE, XC_GGA_C_PBE
 use xc_f03_lib_m,   ONLY:xc_f03_lda_exc,xc_f03_lda_vxc,xc_f03_lda_fxc, &
&                         xc_f03_gga_exc,xc_f03_gga_vxc,xc_f03_gga_fxc
 use interfaces,     ONLY:el_density_and_current,el_magnetization
 use pseudo,         ONLY:pp_rho_nlcc,pp_has_nlcc,PP_nlcc_free
 !
#include<memory.h>
 !
 type(levels) ::en
 type(bz_samp)::Xk
 integer      ::FUNCTIONAL,FUNCTIONALTMP,ORDER
 real(SP), optional :: EXX_FRACTION, EXX_SCREENING
 !
 ! Work Space
 !
 logical  :: l_Fxc_from_Vxc
 integer  :: i1,i2,is_xc,is1,is2,ixc,ifft,ORDER_NOW
 integer(IPL) :: size_IPL
 real(SP) :: mod_magn
 real(SP) :: local_exx_fraction 
 real(DP) :: exsr,PERT_FAC
 !
 ! XC routines arrays
 !
 real(SP) :: spin_rho_SP(fft_size,n_spin),rho(fft_size)
 real(DP) :: spin_rho_DP(n_spin,fft_size),rspts(fft_size)
 real(SP),allocatable:: drho(:,:,:),drho_tmp(:,:),V_xc_pert(:,:)
 real(DP),allocatable:: sigma(:,:)
 complex(SP), allocatable:: drho_cmplx(:,:)
 complex(SP), allocatable:: spin_rho_SP_cmplx(:)
 !
 ! note that multidimensional fxc & vxc is allowed only in xcspol
 !
 real(DP),allocatable :: tmp_exc(:)
 real(DP),allocatable :: v1rho(:,:),v1sigma(:,:)
 real(DP),allocatable :: vx1(:,:),vx2(:,:)
 real(DP),allocatable :: v2rho2(:,:),v2rhosigma(:,:),v2sigma2(:,:)
 logical              :: EvaluateDGradient,l_Fxc_Libxc
 !
 ! Functional and Functional infos
 !
 type(xc_fnctl_t) :: fnctl(2) 
 !
 !=============================== 
 ! Convert and Initialize
 !===============================
 !
 call parser('FxcLibxc',l_Fxc_Libxc)
 l_Fxc_from_Vxc=(n_spin==1).and..not.l_Fxc_Libxc
 !
 exsr=0._DP
 FUNCTIONALTMP=0
!Set the functional as a PBE to get Exc, next calculate the GAU-PBE Exc as
!Exc=Exc(PBE)-alfa*Ex(PBE,SR) next add the HF part.
 !
 if (FUNCTIONAL==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) then 
   FUNCTIONALTMP=XC_HYB_GGA_XC_GAUPBE*XC_FACTOR
   FUNCTIONAL=XC_GGA_X_PBE*XC_FACTOR+XC_GGA_C_PBE  !101130   This is semi-local PBE  
 endif
 !
 call xc_setup_fnctl(fnctl,FUNCTIONAL,n_spin)
 if (FUNCTIONALTMP==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) FUNCTIONAL=XC_HYB_GGA_XC_GAUPBE*XC_FACTOR
 !
 local_exx_fraction = RZERO
 if (present(EXX_FRACTION).and.fnctl(1)%family==XC_FAMILY_HYB_GGA) then 
   call xc_setup_hyb(fnctl,EXX_FRACTION,EXX_SCREENING)
   local_exx_fraction = fnctl(1)%exx_fraction
 end if
 if (FUNCTIONALTMP==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) local_exx_fraction=EXX_FRACTION
 !
 EvaluateDGradient = any((fnctl(:)%family==XC_FAMILY_GGA).or.any(fnctl(:)%family==XC_FAMILY_HYB_GGA))
 !
 !==========================
 ! Electronic/spin density 
 !==========================
 !
#if defined _SC || defined _RT
 if (l_real_time.or.l_nl_optics) then !.or.l_sc_run) then
   rho=rho_n
   if(n_spin> 1) magn=magn_n
 endif
#endif
 !
 if (.not.(l_real_time.or.l_nl_optics)) then !.or.l_sc_run)) then
   call el_density_and_current(en,Xk,rho=rho)
   if (n_spin> 1) call el_magnetization(en,Xk,magn)
 endif
 !
 ! non-linear core corrections (NLCC)
 ! to be improved for yambo_rt, otherwise this is done at each time step
 !
 call XC_nlcc_setup()
 if (pp_has_nlcc) rho(:)=rho(:)+real(pp_rho_nlcc(:),SP)
 call PP_nlcc_free()
 !
 if(n_spin==1) spin_rho_SP(:,1)=rho(:)
 if(n_spin> 1) then
   if (n_spinor==1) then
!$omp parallel do default(shared), private(ifft)
     do ifft=1,fft_size
       spin_rho_SP(ifft,1)=0.5*(rho(ifft)+magn(ifft,3))
       spin_rho_SP(ifft,2)=0.5*(rho(ifft)-magn(ifft,3))
     enddo
!$omp end parallel do
   else
!$omp parallel do default(shared), private(ifft,mod_magn)
     do ifft=1,fft_size
       mod_magn=sqrt(magn(ifft,1)**2+magn(ifft,2)**2+magn(ifft,3)**2)
       spin_rho_SP(ifft,1)=0.5*(rho(ifft)+mod_magn)
       spin_rho_SP(ifft,2)=0.5*(rho(ifft)-mod_magn)
     enddo
!$omp end parallel do
   endif
 endif
 !
 do i1=1,fft_size
   do i2=1,n_spin
     spin_rho_DP(i2,i1)=real(spin_rho_SP(i1,i2),DP)*real(fft_size,DP)/real(DL_vol,DP)
     ! DEBUG <
     ! This was needed for absorption using old libxc
     ! Keeping it with magnons gives numberical noise
     !if (spin_rho_DP(i2,i1)<1.E-10_DP) spin_rho_DP(i2,i1)=0._DP
     ! DEBUG >
   enddo
 enddo
 !
 !=============================
 ! Electronic density gradient
 !=============================
 if (ORDER==0) then
   YAMBO_ALLOC(tmp_exc,(fft_size))
   tmp_exc=0._SP
 endif
 if (ORDER==1.or.ORDER==2) then
   YAMBO_ALLOC(v1rho,(n_spin,fft_size))
   v1rho=0._DP
 endif
 if (ORDER==2.and..not.l_Fxc_from_Vxc) then
   YAMBO_ALLOC(v2rho2,(xc_spin(1),fft_size))
   v2rho2=0._DP
 endif
 !
 if (EvaluateDGradient) then 
   !
   YAMBO_ALLOC(drho,(fft_size,n_spin,3))
   YAMBO_ALLOC(sigma,(xc_spin(1),fft_size))
   if (ORDER==1.or.ORDER==2) then
     YAMBO_ALLOC(v1sigma,(xc_spin(1),fft_size))
     v1sigma=0._DP
   endif
   if (ORDER==2.and..not.l_Fxc_from_Vxc) then
     YAMBO_ALLOC(v2rhosigma,(xc_spin(1)*n_spin,fft_size))
     YAMBO_ALLOC(v2sigma2,(xc_spin(1)*n_spin,fft_size))
     v2rhosigma=0._DP
     v2sigma2=0._DP
   endif
   if (FUNCTIONAL==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) then
     YAMBO_ALLOC(vx1,(n_spin,fft_size))
     YAMBO_ALLOC(vx2,(xc_spin(1),fft_size))
     vx1=0._DP
     vx2=0._DP
   endif 
   !
   if (n_spin>1 .or. order<2) then
     YAMBO_ALLOC(drho_cmplx,(fft_size,3))
     YAMBO_ALLOC(spin_rho_SP_cmplx,(fft_size))
     do is1 = 1,n_spin
       spin_rho_SP_cmplx(:)=cmplx(spin_rho_SP(:,is1),0.0_SP,kind=SP)
       call eval_Gradient(spin_rho_SP_cmplx,drho_cmplx,1,"density")
       drho(:,is1,:)=real(drho_cmplx)
     end do
     YAMBO_FREE(drho_cmplx)
     YAMBO_FREE(spin_rho_SP_cmplx)
   else
     ! DS: This procedure computes a more precise drho (much less noise)
     !     This is particularly important for the case order=2
     ! To do: Extend the procedure to the case n_spin>1
     YAMBO_ALLOC(drho_tmp,(fft_size,3))
     call el_density_and_current(en,Xk,drho=drho_tmp)
     drho(:,1,:) = drho_tmp(:,:)
     YAMBO_FREE(drho_tmp)
   endif
   !
   forall (is1=1:n_spin,i1=1:fft_size) drho(i1,is1,:) = drho(i1,is1,:)*fft_size/DL_vol*2._SP*pi/alat(:)
   !
   do i1 = 1,fft_size
     do is1 = 1,n_spin
       do is2 = is1,n_spin
         is_xc = xc_spin_map1(is1,is2)
         sigma(is_xc,i1) = real(drho(i1,is1,1)*drho(i1,is2,1)+drho(i1,is1,2)*drho(i1,is2,2)+drho(i1,is1,3)*drho(i1,is2,3),DP)
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
 if (order==0) E_xc=0._SP
 if (order==1) V_xc=0._SP
 if (order==2) F_xc=0._SP
 !
 if (FUNCTIONAL == XC_LDA_C_KP) then ! Special case, XC_LDA_C_KP not available from libxc
   select case(ORDER)
     case(0)
       forall (i1=1:fft_size) rspts(i1)=(3._DP/4._DP/pi_DP/real(spin_rho_SP(i1,1)/DL_vol,DP))**(1._DP/3._DP)
       call xc_rpa_kp(rspts,v1rho)
       E_xc = real(tmp_exc,SP) 
       E_xc_val = V_dot_V(fft_size,rho,E_xc(:))
     case(1)
       call error(' Vxc not coded for LDA C KP')
     case(2)
       call error(' Fxc not coded for LDA C KP')
   end select
   return
 end if
 !
 size_IPL=int(fft_size,kind=IPL)
 !
 ORDER_NOW=ORDER
 if (l_Fxc_from_Vxc.and.ORDER==2) then
   call msg('s',' Evaluating Fxc as numerical functional derivative of Vxc')
   PERT_FAC=1.000001_DP
   ORDER_NOW=1
   YAMBO_ALLOC(V_xc_pert,(fft_size,n_spin))
   V_xc_pert=0._SP
 endif
 do ixc = 1,2
   if (fnctl(ixc)%id == 0) cycle
   if (fnctl(ixc)%family==XC_FAMILY_LDA) then 
     select case(ORDER_NOW)
       case(0)
         call xc_f03_lda_exc(fnctl(ixc)%conf, size_IPL, spin_rho_DP(1,1), tmp_exc(1))
         E_xc = E_xc + real(tmp_exc,SP)    
       case(1)
         call xc_f03_lda_vxc(fnctl(ixc)%conf, size_IPL, spin_rho_DP(1,1), v1rho(1,1))
         forall(i1=1:fft_size,i2=1:n_spin) V_xc(i1,i2) = V_xc(i1,i2) + real(v1rho(i2,i1),SP)
         if (l_Fxc_from_Vxc.and.ORDER==2) then
           spin_rho_DP=spin_rho_DP*PERT_FAC
           call xc_f03_lda_vxc(fnctl(ixc)%conf, size_IPL, spin_rho_DP(1,1), v1rho(1,1))
           forall(i1=1:fft_size,i2=1:n_spin) V_xc_pert(i1,i2) = V_xc_pert(i1,i2) + real(v1rho(i2,i1),SP)
           spin_rho_DP=spin_rho_DP/PERT_FAC
         endif
       case(2)
         if( l_BS_magnons) then
           call xc_f03_lda_vxc(fnctl(ixc)%conf, size_IPL, spin_rho_DP(1,1), v1rho(1,1))
           forall(i1=1:fft_size,i2=1:n_spin) V_xc(i1,i2) = V_xc(i1,i2) + real(v1rho(i2,i1),SP)
         endif
         ! I could avoid this, however I need it due to numerical noise
         ! See also comment in Build_F_xc_mat
         !if( (.not.l_BS_magnons) .or. n_spinor==2) then
           call xc_f03_lda_fxc(fnctl(ixc)%conf, size_IPL, spin_rho_DP(1,1), v2rho2(1,1))
           ! Yambo internal for Kxc
           call XC_eval_lda_kernel(v1rho,v2rho2)
         !endif
     end select
   elseif (fnctl(ixc)%family==XC_FAMILY_GGA.or.fnctl(ixc)%family==XC_FAMILY_HYB_GGA) then
        !GAUPBE short range x-energy  and potentials part
        if(FUNCTIONAL==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) call XC_eval_pbegaux(spin_rho_DP(1,1), sigma(1,1), exsr,vx1(1,1),vx2(1,1))
     select case(ORDER_NOW)
       case(0)
         call xc_f03_gga_exc(fnctl(ixc)%conf, size_IPL, spin_rho_DP(1,1), sigma(1,1), tmp_exc(1))
         E_xc = E_xc + real(tmp_exc,SP)    
       case(1)
         call xc_f03_gga_vxc(fnctl(ixc)%conf, size_IPL, spin_rho_DP(1,1), sigma(1,1), v1rho(1,1), v1sigma(1,1))
         if (FUNCTIONAL==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR.and.ixc==1) then
           !GAUPBE short energy part of the x potential,factor 2 in vx2 for
           !consistency qith QE
           v1rho=v1rho-EXX_FRACTION*vx1
           v1sigma=v1sigma-(EXX_FRACTION*vx2)/2._DP
         endif
         ! Yambo internal for V_xc
         call XC_eval_gga_potential(v1rho,v1sigma,drho,V_xc)
         if (l_Fxc_from_Vxc.and.ORDER==2) then
           spin_rho_DP=spin_rho_DP*PERT_FAC
           sigma      =sigma      *PERT_FAC
           call xc_f03_gga_vxc(fnctl(ixc)%conf, size_IPL, spin_rho_DP(1,1), sigma(1,1), v1rho(1,1), v1sigma(1,1))
           call XC_eval_gga_potential(v1rho,v1sigma,drho,V_xc_pert)
           spin_rho_DP=spin_rho_DP/PERT_FAC
           sigma      =sigma      /PERT_FAC
         endif
       case(2)
         if (FUNCTIONAL==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) call error(' Fxc not coded for GAU-PBEP')
         if( l_BS_magnons) then
           call xc_f03_gga_vxc(fnctl(ixc)%conf, size_IPL, spin_rho_DP(1,1), sigma(1,1), v1rho(1,1), v1sigma(1,1))
           ! Yambo internal for V_xc
           call XC_eval_gga_potential(v1rho,v1sigma,drho,V_xc)
         endif
         ! I could avoid this, however I need it due to numerical noise
         ! See also comment in Build_F_xc_mat
         !if( (.not.l_BS_magnons) .or. n_spinor==2) then
           call xc_f03_gga_fxc(fnctl(ixc)%conf, size_IPL, spin_rho_DP(1,1), sigma(1,1), v2rho2(1,1), v2rhosigma(1,1), v2sigma2(1,1))
           ! Yambo internal for Kxc
           call XC_eval_gga_kernel(v1rho,v1sigma,v2rho2,v2rhosigma,v2sigma2)
         !endif
     end select
   end if
   call xc_f03_func_end(fnctl(ixc)%conf)
 enddo
 !
 if (order==0) then
   E_xc_val = V_dot_V(fft_size,rho,E_xc(:))
   if (FUNCTIONAL==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR)  E_xc_val= E_xc_val-EXX_FRACTION*real(exsr,SP)
 endif
 !
 if (l_Fxc_from_Vxc.and.ORDER==2) then
   do ifft=1,fft_size
     if (abs(spin_rho_DP(1,ifft))<1.E-7_DP) cycle
     F_xc(ifft,1,1)=real( real(V_xc_pert(ifft,1)-V_xc(ifft,1),DP)/(spin_rho_DP(1,ifft)*(PERT_FAC-1._DP)),SP)
   enddo
 endif
 !
 if (EvaluateDGradient) then
   YAMBO_FREE(sigma)
   YAMBO_FREE(drho)
 endif
 if (ORDER==1) then
   YAMBO_FREE(tmp_exc)
 endif
 if (EvaluateDGradient.and.(ORDER==1)) then
   YAMBO_FREE(v1rho)
   YAMBO_FREE(v1sigma)
 endif
 if (EvaluateDGradient.and.(ORDER==2)) then
   YAMBO_FREE(v2rho2)
   YAMBO_FREE(v2rhosigma)
   YAMBO_FREE(v2sigma2)
 endif
 if (EvaluateDGradient.and.(FUNCTIONAL==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR)) then
   YAMBO_FREE(vx1)
   YAMBO_FREE(vx2)
 endif
 !
 if (present(EXX_FRACTION)) EXX_FRACTION = local_exx_fraction 
 !
end subroutine XC_libxc_driver
