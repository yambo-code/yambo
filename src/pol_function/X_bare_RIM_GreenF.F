!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine X_bare_RIM_GreenF(iq,transitions,fr,Xw,Xen,Xk,GreenF,ordering)
 !
 use pars,       ONLY:SP
 ! GPL_EXCLUDE_START
#if defined _ELPH
 use units,      ONLY:pi
#endif
 ! GPL_EXCLUDE_END
 use X_m,        ONLY:use_X_RIM
 use BS,         ONLY:BSS_uses_RIM
 use electrons,  ONLY:levels,BZ_RIM_table,BZ_RIM_nkpt,spin_occ,BZ_RIM_nbands,&
                      BZ_RIM_ipol_weight
 use R_lattice,  ONLY:qindx_X,bz_samp
 use frequency,  ONLY:w_samp
 !
 implicit none
 integer      :: iq,transitions(4),fr(2)
 type(w_samp) :: Xw
 type(levels) :: Xen
 type(bz_samp):: Xk
 complex(SP)  :: GreenF(Xw%n(2))
 character(*) :: ordering
 !
 !Work Space
 !
 integer      ::i1,i2,iw,ikbz,ikpbz,iv,ic,i_spin,ik_rand,ikp_rand,ik,ikp
 real(SP)     ::ffac,f0fac,W_,s_ffac
 complex(SP)  ::Z_,pole
 ! GPL_EXCLUDE_START
#if defined _ELPH
 complex(SP), allocatable  ::Green_F_convoluted(:)
 real(SP)                  ::W_step
#endif
 ! GPL_EXCLUDE_END
 !
 GreenF  = cmplx(0.,0.,SP)
 !
 ! Transitions
 !
 ikbz   = transitions(1)     
 ikpbz  = qindx_X(iq,ikbz,1) 
 ik     = Xk%sstar(ikbz,1)
 ikp    = Xk%sstar(ikpbz,1)
 iv     = transitions(2)
 ic     = transitions(3)
 i_spin = transitions(4)
 !
 ! GPL_EXCLUDE_START
#if defined _ELPH
 !
 ! Green Functions convolution
 !
 if (associated(Xen%GreenF)) then 
   ! 
   allocate(Green_F_convoluted(Xen%GreenF_n_E_steps))
   !
   W_step=real( Xen%GreenF_W(ic,ik,i_spin,2) - Xen%GreenF_W(ic,ik,i_spin,1))
   !
   call Convolve(real(Xen%GreenF(ic,ik,i_spin,:)), real(Xen%GreenF(iv,ikp,i_spin,:)),&
&                Green_F_convoluted,Xen%GreenF_n_E_steps,W_step,real( Xen%GreenF_W(ic,ik,i_spin,1)) )
   !
   ! Green_F_convoluted is now integrated to Zc*Zv*pi*pi. As I want the spectral function only
   ! to pass to the KK transformation I need to rescale by pi^2
   !
   Green_F_convoluted=Green_F_convoluted/pi**2.
   !
   GreenF=(0.,0.)
   !
   ! Resonant Part
   !
   if (ordering/="a") &
&    call Kramers_Kronig(Green_F_convoluted,real(Xen%GreenF_W(iv,ikp,i_spin,:)),Xen%GreenF_n_E_steps,&
&                        GreenF,            Xw%p,                          Xw%n(1), cmplx(0.,0.,SP))
   if (ordering=="c") then
     call Kramers_Kronig(Green_F_convoluted,real(Xen%GreenF_W(iv,ikp,i_spin,:)),Xen%GreenF_n_E_steps,&
&                        GreenF,            -Xw%p,                      Xw%n(1), cmplx(0.,0.,SP))
   else if (ordering=="a".or.ordering=="t") then
     call Kramers_Kronig(Green_F_convoluted,real(Xen%GreenF_W(iv,ikp,i_spin,:)),Xen%GreenF_n_E_steps,&
&                        GreenF,            -conjg(Xw%p),               Xw%n(1), cmplx(0.,0.,SP))
   endif
   !
   deallocate(Green_F_convoluted)
   !
   return
   !
 endif
 !
#endif
 ! GPL_EXCLUDE_END
 !
 W_=0.
 Z_=(1.,0.)
 if (associated(Xen%W)) W_=abs(Xen%W(ic,ik,i_spin))+abs(Xen%W(iv,ikp,i_spin))
 if (associated(Xen%Z)) Z_=Xen%Z(ic,ik,i_spin)*Xen%Z(iv,ikp,i_spin)
 !
 if ((.not.use_X_RIM.or.ic>BZ_RIM_nbands).and.(.not.BSS_uses_RIM)) then
   !
   ! Using QP corrected states
   !
   pole=Xen%E(ic,ik,i_spin)-Xen%E(iv,ikp,i_spin)-(0.,1.)*W_
   !
   do iw=fr(1),fr(2)
     if (ordering=="t" ) GreenF(iw-fr(1)+1)=Z_*(1./(Xw%p(iw)-pole)-1./(conjg(Xw%p(iw))+pole))
     if (ordering=="c" ) GreenF(iw-fr(1)+1)=Z_*(1./(Xw%p(iw)-pole)-1./(Xw%p(iw)+conjg(pole)))
     if (ordering=="r" ) GreenF(iw-fr(1)+1)=Z_/    (Xw%p(iw)-pole)
     if (ordering=="a" ) GreenF(iw-fr(1)+1)=                      -Z_/(conjg(Xw%p(iw))+pole)
   enddo
   !
   return
   !
 endif
 !
 ! In the X_o loop the GreenF is multiplied by 
 !
 !  f0fac/spin_occ/real(Xk%nbz)/DL_vol
 !
 ! so that I have to divide/multiply to correctly renormalize 
 !
 f0fac=Xen%f(iv,ikp,i_spin)*(spin_occ-Xen%f(ic,ik,i_spin))
 !
 if (iq==1) then
   !
   do i1=1,BZ_RIM_nkpt(ikbz)
     ! 
     ik_rand=BZ_RIM_table(ikbz,i1) 
     !
     pole=Xen%E_RIM(ic,ik_rand,i_spin)-Xen%E_RIM(iv,ik_rand,i_spin)-(0.,1.)*W_
     !
     s_ffac=Z_*Xen%f_RIM(iv,ik_rand,i_spin)*(spin_occ-Xen%f_RIM(ic,ik_rand,i_spin))*&
&           BZ_RIM_ipol_weight(i1)/real(BZ_RIM_nkpt(ikbz))
     ! 
     ffac=s_ffac/f0fac
     !
     do iw=fr(1),fr(2)
       !
       if (ordering=="t" ) GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)&
&                                             +ffac/(Xw%p(iw)-pole)-ffac/(conjg(Xw%p(iw))+pole)
       if (ordering=="c" ) GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)&
&                                             +ffac/(Xw%p(iw)-pole)-ffac/(Xw%p(iw)+pole)
       if (ordering=="r" ) GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)&
&                                             +ffac/(Xw%p(iw)-pole)
       if (ordering=="a" ) GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)&
&                                             -ffac/(conjg(Xw%p(iw))+pole)
       if (ordering=="ia") GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)&
&                                             -ffac/(Xw%p(iw)+pole)
       if (ordering=="sr") GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)&
&                                             +s_ffac/(Xw%p(iw)-pole)/spin_occ**2.
       if (ordering=="sa") GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)&
&                                             -s_ffac/(Xw%p(iw)+pole)/spin_occ**2.
     enddo
     !
   enddo
   !
 else
   !
   do i1=1,BZ_RIM_nkpt(ikbz)
     ik_rand=BZ_RIM_table(ikbz,i1) 
     !
     do i2=1,BZ_RIM_nkpt(ikpbz)
       ikp_rand=BZ_RIM_table(ikpbz,i2) 
       !
       pole=Xen%E_RIM(ic,ik_rand,i_spin)-Xen%E_RIM(iv,ikp_rand,i_spin)+(0.,1.)*W_
       !
       ffac=Z_*Xen%f_RIM(iv,ikp_rand,i_spin)*(spin_occ-Xen%f_RIM(ic,ik_rand,i_spin))*&
&         BZ_RIM_ipol_weight(i1)*BZ_RIM_ipol_weight(i2)/real(BZ_RIM_nkpt(ikbz))/real(BZ_RIM_nkpt(ikpbz))/f0fac
       !
       do iw=fr(1),fr(2)
         if (ordering=="t") GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)+&
&                           ffac/(Xw%p(iw)-pole)-ffac/(conjg(Xw%p(iw))+pole)
         if (ordering=="c") GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)+&
&                           ffac/(Xw%p(iw)-pole)-ffac/(Xw%p(iw)+pole)
         if (ordering=="r") GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)+&
&                           ffac/(Xw%p(iw)-pole)
         if (ordering=="a") GreenF(iw-fr(1)+1)=GreenF(iw-fr(1)+1)&
&                                                  -ffac/(conjg(Xw%p(iw))+pole)
       enddo
       !
     enddo
     !
   enddo
   !
 endif
 !
end subroutine
