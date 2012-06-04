!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team
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
subroutine Drude(iq,fr,Xen,Xk,Xw,X,drude_GreenF)
 !
 use pars,          ONLY:SP,cZERO
 use units,         ONLY:HA2EV
 use com,           ONLY:msg
 use BS,            ONLY:BS_drude_dim
 use frequency,     ONLY:bg_npts,cg_pt,cg_npts
 use stderr,        ONLY:intc
 use drivers,       ONLY:l_bss
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ
 use R_lattice,     ONLY:bz_samp,q_norm 
 use D_lattice,     ONLY:DL_vol
 use X_m,           ONLY:X_t
 !
 implicit none
 !
 type(levels)         :: Xen
 type(bz_samp)        :: Xk
 type(X_t)            :: X
 type(w_samp)         :: Xw
 !
 integer              :: iq,fr(2)
 complex(SP)          :: drude_GreenF(Xw%n(2))
 !
 real(SP)             :: drude_factor
 logical              :: Drude_term
 integer              :: iw,Drude_dim
 !
 drude_GreenF = cZERO
 !
 ! Drude Contrib. if all of following conditions hold
 !===============
 !  . Non zero Drude Freq.
 !  . Metallic system 
 !  . Optical response @ q = Gamma
 !
 Drude_term=all((/real(X%Wd)>0.,aimag(X%Wd)>0.,Xen%kf>0,Xen%nbf/=Xen%nbm,iq==1/))
 !
 if(.not.Drude_term) return
 !
 Drude_dim=0
 if(l_bss) then
   Drude_dim=BS_drude_dim
 else
   if(cg_pt(1)==0)  Drude_dim=bg_npts(1)
 endif
 !
 call msg('nrs','[X] Drude replaces '//trim(intc(Drude_dim))//' poles @[ev]:',(/real(X%Wd),aimag(X%Wd)/)*HA2EV)
 !
 ! drude_factor is the eh_occ factor calculated in the Xo loop
 ! evaluated at the fermi state
 !
 drude_factor=Xen%f(Xen%bf,Xen%kf,Xen%sf)*&
&             (spin_occ-Xen%f(Xen%bf,Xen%kf,Xen%sf))/ &
&             (spin_occ*real(Xk%nbz)*DL_vol)
 !
 do iw=fr(1),fr(2)
   drude_GreenF(iw-fr(1)+1)=X_drude(real(Xw%p(iw)),X%Wd,q_norm(1))/drude_factor
 enddo
 !
 contains
   !
   function X_drude(W,Wd,q0_norm)
   !
   !Jellium Xo propagator with a given plasma frequency
   !The propagator is T-ordered. 
   !See R.D. Mattuck "A guide to Feynmann diagrams in the Many-Body
   !                  problem", pag. 197.
   !  
   use pars,  ONLY:SP,pi
   implicit none
   real(SP)    :: q0_norm,W
   complex(SP) :: Wd
   !
   ! Work Space
   !
   complex(SP) :: xi,X_drude
   real(SP)    :: Kf,rxi,fac
   !
   Kf =((3.*pi*real(Wd)**2.)/4.)**(1./3.)
   fac=Kf*q0_norm
   xi =W+(0.,1.)*aimag(Wd)*fac
   rxi=real(xi)
   !
   if (abs(W)<=10*fac) then
     X_drude=-1./(2.*pi**2.*q0_norm)*(2.*fac+xi*log((xi-fac)/xi)-&
&            conjg(xi)*log((conjg(xi)+fac)/conjg(xi)))
   else
     X_drude=-Kf*fac/(2.*pi**2.)*(-1./(2.*xi)+1./(2.*conjg(xi))-&
&            fac/(3.*rxi**2.)-fac/(3.*rxi**2.))
   endif
   !
  end function X_drude
  !
  !
end subroutine
