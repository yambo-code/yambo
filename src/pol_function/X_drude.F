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
&          conjg(xi)*log((conjg(xi)+fac)/conjg(xi)))
 else
   X_drude=-Kf*fac/(2.*pi**2.)*(-1./(2.*xi)+1./(2.*conjg(xi))-&
&          fac/(3.*rxi**2.)-fac/(3.*rxi**2.))
 endif
 !
end function
