!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
#if defined _RT
subroutine xc_lda_driver(en,Xk,KIND,FUNCTIONAL,ORDER,G_lesser)
#else
subroutine xc_lda_driver(en,Xk,KIND,FUNCTIONAL,ORDER)
#endif
 !
 ! These sources have been slightly adpated from Abinit 4.6.5.
 !
 ! xcspol: XC_LDA_C_PW
 ! LDA or LSD, Teter Pade parametrization (4/93, published in S. Goedecker, 
 ! M. Teter, J. Huetter, Phys.Rev.B54, 1703 (1996)), 
 ! which reproduces Perdew-Wang (which reproduces Ceperley-Alder!).
 !
 ! xcpzca: XC_LDA_C_PZ
 ! LDA, Perdew-Zunger-Ceperley-Alder (no spin-polarization)
 !
 ! xcwign: XC_LDA_C_WIGNER
 ! LDA, Wigner functional (no spin-polarization)
 !
 ! ! xchelu: XC_LDA_C_HL
 ! LDA, Hedin-Lundqvist functional (no spin-polarization)
 !
 ! xcxalp: XC_LDA_C_XALPHA
 ! LDA, "X-alpha" functional (no spin-polarization)
 ! 
 ! Note that, in order to get LDA exchange and correlation separated
 !
 ! Exx = -efac*rsm1
 ! Vxx = -vfac*rsm1
 !
 ! in xcpzca xcwign xchelu xcxalp. While in xcspol xctetr there is not
 ! such a type of distinction.
 !
 ! Andrea (9/8/06)
 !
 ! In case of spin polarized systems *REMEMBER* that:
 !
 !  whatever(1) == whatever UP
 !  whatever(2) == whatever DOWN
 !
 ! Yambo have been considering the wrong order up to revision 335 (November 2008)
 !
 use pars,          ONLY:SP,DP,pi
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:DL_vol
 use com,           ONLY:msg,warning
 use electrons,     ONLY:levels,n_spin
 use FFT_m,         ONLY:fft_size
#if defined _RT
 use SC,            ONLY:SC_bands
#endif
 use xc_functionals,ONLY:XC_EXCHANGE,XC_EXCHANGE_CORRELATION,XC_CORRELATION,&
&                        E_xc,F_xc,V_xc,XC_LDA_X,&
&                        XC_LDA_C_PW,xc_string,XC_LDA_C_PZ,&
&                        XC_LDA_C_WIGNER,XC_LDA_C_HL,XC_LDA_C_XALPHA,&
&                        XC_LDA_C_KP,two_spin_density,NONE
 implicit none
 type(levels) ::en
 type(bz_samp)::Xk
 integer      ::KIND,FUNCTIONAL,ORDER
#if defined _RT
 complex(SP), intent(in) :: G_lesser(SC_bands,SC_bands,en%nk)
#endif
 !
 ! Work Space
 !
 integer  :: i1
 !
 ! XC routines arrays
 !
 real(SP) :: rho_sp(fft_size)
 real(DP) :: rspts(fft_size),zeta(fft_size),rho(fft_size)
 !
 ! note that multidimensional dvxc & vxc is allowed only in xcspol
 !
 real(DP) :: dvxc(fft_size,3),exc(fft_size),vxc(fft_size,n_spin)
 real(DP) :: dvx(fft_size,3), ex(fft_size), vx(fft_size,n_spin)
 !
 ! Init
 !
 zeta=0._DP
 exc=0._DP
 vxc=0._DP
 dvxc=0._DP
 ex=0._DP
 vx=0._DP
 dvx=0._DP
 !
 ! COM
 !
 if (KIND>0.and.FUNCTIONAL>0) then
   call msg('rsn','[xc] Functional ',xc_string(KIND,FUNCTIONAL))
 else if (KIND<0.or.FUNCTIONAL<0) then
   KIND=XC_EXCHANGE_CORRELATION
   FUNCTIONAL=XC_LDA_C_PW
   call msg('rsn','[xc] Functional unknown. Used ',xc_string(KIND,FUNCTIONAL))
 endif
 !
 ! Electronic density & Rs
 !
#if defined _RT
 call el_density(G_lesser,Xk,rho_sp,.false.)
#else
 call el_density(en,Xk,rho_sp,.false.)
#endif
 rho=rho_sp*real(fft_size,DP)/real(DL_vol,DP)
 forall (i1=1:fft_size) rspts(i1)=(3._DP/4._DP/pi/rho(i1))**(1._DP/3._DP)
 !
 ! GPL_EXCLUDE_START SPIN_IGNORE
 !
 if (n_spin==2) then
   !
   ! Electronic spin density & Zeta
   !
   call spin_density(en,Xk)
   !
   zeta(:)=sqrt(two_spin_density(:,1)**2+two_spin_density(:,2)**2+two_spin_density(:,3)**2)/rho_sp
   !
 endif
 !
 ! GPL_EXCLUDE_END SPIN_IGNORE
 !
 ! Hartree
 !
 if (KIND==NONE) then
   if (order==0) E_xc=0.
   if (order==1) V_xc=0.
   if (order==2) F_xc=0.
   return
 endif
 !
 ! EXX = X_alpha when alpha=3/4. HF when alpha=1.
 !
 if (KIND/=XC_EXCHANGE_CORRELATION.or.FUNCTIONAL==XC_LDA_X) then
   call xcxalp(1._DP,ex,fft_size,n_spin,ORDER,rspts,zeta,vx,dvx)
   if (KIND==XC_EXCHANGE.or.FUNCTIONAL==XC_LDA_X) then
     exc=ex
     vxc=vx
     dvxc=dvx
   endif
 endif
 !
 ! CORRELATION
 !
 if (KIND/=XC_EXCHANGE) then
   !
   select case (FUNCTIONAL)
     !
     case (XC_LDA_C_PW)
       call xcspol(dvxc,exc,fft_size,n_spin,ORDER,rspts,vxc,zeta)
       !
     case (XC_LDA_C_PZ)
       call xcpzca(dvxc(:,1),exc,fft_size,ORDER,rho,rspts,vxc(:,1))
       !
     case (XC_LDA_C_WIGNER)
       call xcwign(dvxc(:,1),exc,fft_size,ORDER,rho,rspts,vxc(:,1))
       !
     case (XC_LDA_C_HL)
       call xchelu(dvxc(:,1),exc,fft_size,ORDER,rspts,vxc(:,1))
       !
     case (XC_LDA_C_XALPHA)
       call xcxalp(3._DP/2._DP,exc,fft_size,n_spin,ORDER,rspts,zeta,vxc,dvxc)
       !
     case (XC_LDA_C_KP)
       call xc_rpa_kp(rspts,exc)
       !
   end select
   !
   ! GPL_EXCLUDE_START SPIN_IGNORE
   !
   !
   ! Only XC_LDA_C_PW supports spin
   ! 
   if (FUNCTIONAL/=XC_LDA_C_PW.and.FUNCTIONAL/=XC_LDA_C_XALPHA.and.n_spin==2) then
     call warning('Used functional is NOT spin-polarized')
     vxc(:,2)=vxc(:,1)
     dvxc(:,2)=0._DP
     dvxc(:,3)=dvxc(:,1)
   endif
   !
   ! GPL_EXCLUDE_END SPIN_IGNORE
   !
 endif
 !
 ! CORRELATION ONLY
 !
 if (KIND==XC_CORRELATION) then
   exc =exc-ex
   vxc =vxc-vx
   dvxc=dvxc-dvx
 endif
 !
 ! Allocation and Transfer
 !
 select case (ORDER)
   case(0)
     E_xc=exc
   case(1)
     V_xc=vxc
   case(2)
     F_xc=dvxc(:,1)
 end select
 !
end subroutine
