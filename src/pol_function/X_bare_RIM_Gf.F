!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
 !
end subroutine
