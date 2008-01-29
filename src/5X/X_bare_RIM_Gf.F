!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine X_bare_RIM_Gf(iq,transitions,fr,Xw,Xen,Xk,Gf,ordering)
 !
 use pars,       ONLY:SP
 use X_m,        ONLY:use_X_RIM
 use BS,         ONLY:BSS_uses_RIM
 use electrons,  ONLY:levels,BZ_RIM_table,BZ_RIM_nkpt,spin_occ
 use R_lattice,  ONLY:qindx_X,bz_samp
 use frequency,  ONLY:w_samp
 !
 implicit none
 integer      :: iq,transitions(4),fr(2)
 type(w_samp) :: Xw
 type(levels) :: Xen
 type(bz_samp):: Xk
 complex(SP)  :: Gf(Xw%n(2))
 character(*) :: ordering
 !
 !Work Space
 !
 integer      ::i1,i2,iw,ikbz,ikpbz,iv,ic,i_spin,ik_rand,ikp_rand,ik,ikp
 real(SP)     ::ffac,f0fac,W_
 complex(SP)  ::Z_,pole
 !
 Gf  = cmplx(0.,0.,SP)
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
 W_=0.
 Z_=(1.,0.)
 if (associated(Xen%W)) W_=Xen%W(ic,ik,i_spin)-Xen%W(iv,ikp,i_spin)
 if (associated(Xen%Z)) Z_=Xen%Z(ic,ik,i_spin)*Xen%Z(iv,ikp,i_spin)
 !
 if ((.not.use_X_RIM).and.(.not.BSS_uses_RIM)) then
   !
   ! Using QP corrected states
   !
   pole=Xen%E(ic,ik,i_spin)-Xen%E(iv,ikp,i_spin)+(0.,1.)*W_
   !
   do iw=fr(1),fr(2)
     if (ordering=="t") Gf(iw-fr(1)+1)=Z_*(1./(Xw%p(iw)-pole)-1./(conjg(Xw%p(iw))+pole))
     if (ordering=="c") Gf(iw-fr(1)+1)=Z_*(1./(Xw%p(iw)-pole)-1./(Xw%p(iw)+pole))
     if (ordering=="r") Gf(iw-fr(1)+1)=Z_/(Xw%p(iw)-pole)
     if (ordering=="a") Gf(iw-fr(1)+1)=           -Z_/(conjg(Xw%p(iw))+pole)
   enddo
   !
   return
   !
 endif
 !
 ! GPL_END
 !
 ! In the X_o loop the Gf is multiplied by 
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
     pole=Xen%E_RIM(ic,ik_rand,i_spin)-Xen%E_RIM(iv,ik_rand,i_spin)+(0.,1.)*W_
     !
     ffac=Z_*Xen%f_RIM(iv,ik_rand,i_spin)*(spin_occ-Xen%f_RIM(ic,ik_rand,i_spin))/&
&         real(BZ_RIM_nkpt(ikbz))/f0fac
     !
     do iw=fr(1),fr(2)
       !
       if (ordering=="t") Gf(iw-fr(1)+1)=Gf(iw-fr(1)+1)+&
&                         ffac/(Xw%p(iw)-pole)-ffac/(conjg(Xw%p(iw))+pole)
       if (ordering=="c") Gf(iw-fr(1)+1)=Gf(iw-fr(1)+1)+&
&                         ffac/(Xw%p(iw)-pole)-ffac/(Xw%p(iw)+pole)
       if (ordering=="r") Gf(iw-fr(1)+1)=Gf(iw-fr(1)+1)+&
&                         ffac/(Xw%p(iw)-pole)
       if (ordering=="a") Gf(iw-fr(1)+1)=Gf(iw-fr(1)+1)&
&                                                -ffac/(conjg(Xw%p(iw))+pole)
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
       ffac=Z_*Xen%f_RIM(iv,ikp_rand,i_spin)*(spin_occ-Xen%f_RIM(ic,ik_rand,i_spin))/&
&         real(BZ_RIM_nkpt(ikbz))/real(BZ_RIM_nkpt(ikpbz))/f0fac
       !
       do iw=fr(1),fr(2)
         if (ordering=="t") Gf(iw-fr(1)+1)=Gf(iw-fr(1)+1)+&
&                           ffac/(Xw%p(iw)-pole)-ffac/(conjg(Xw%p(iw))+pole)
         if (ordering=="c") Gf(iw-fr(1)+1)=Gf(iw-fr(1)+1)+&
&                           ffac/(Xw%p(iw)-pole)-ffac/(Xw%p(iw)+pole)
         if (ordering=="r") Gf(iw-fr(1)+1)=Gf(iw-fr(1)+1)+&
&                           ffac/(Xw%p(iw)-pole)
         if (ordering=="a") Gf(iw-fr(1)+1)=Gf(iw-fr(1)+1)&
&                                                  -ffac/(conjg(Xw%p(iw))+pole)
       enddo
       !
     enddo
     !
   enddo
   !
 endif
 !
 ! GPL_START
 !
end subroutine
