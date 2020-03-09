!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,DS
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
subroutine X_Drude(iq,Xen,Xk,Xw,Wd,drude_GreenF)
 !
 use pars,          ONLY:SP,cZERO
 use units,         ONLY:HA2EV
 use com,           ONLY:msg
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ
 use R_lattice,     ONLY:bz_samp,q_norm
 use D_lattice,     ONLY:DL_vol
 use X_m,           ONLY:use_X_DbGd,l_drude
 !
 implicit none
 !
 type(levels) ,intent(in)   :: Xen
 type(bz_samp),intent(in)   :: Xk
 type(w_samp) ,intent(in)   :: Xw
 !
 integer      ,intent(in)   :: iq
 complex(SP)  ,intent(inout):: Wd
 complex(SP)  ,intent(out)  :: drude_GreenF(Xw%n_freqs)
 !
 real(SP)             :: drude_factor,f_e,f_h
 logical              :: Drude_term(4)
 integer              :: iw
 !
 drude_GreenF = cZERO
 !
 ! Drude Contrib. if all of following conditions hold
 !===============
 !  . Non zero Drude Freq.
 !  . Metallic system 
 !  . Optical response @ q = Gamma
 !
 Drude_term(1) = real(Wd)>0._SP .and. aimag(Wd)>0._SP
 Drude_term(2) = Xen%nbf/=Xen%nbm
 Drude_term(3) = Xen%kf>0
 Drude_term(4) = (iq==1)
 !
 ! Checks
 !
 if (.not.Drude_term(4)) then
   call warning(' Not q=0. I should not be here!')
 endif
 !
 if (.not.Drude_term(1).and. all((/Drude_term(2:3)/)) .and..not.use_X_DbGd ) then
   call warning(' The system is a metal but Drude term not included.')
 endif
 !
 if(      Drude_term(1) .and. .not.Drude_term(2) ) then
   call warning(' System is not a metal. Drude term not included')
   Wd=cZERO
 endif
 !
 if(      Drude_term(1) .and.      Drude_term(2) .and. .not.Drude_term(3) ) then
   call warning(' Too low ElecTemp. Drude term not included.')
   Wd=cZERO
 endif
 !
 l_drude=all(Drude_term)
 !
 if(.not.all(Drude_term)) return
 !
 call msg('nrs','[X/K] Drude contribution @[ev]',(/real(Wd),aimag(Wd)/)*HA2EV)
 !
 ! drude_factor is the eh_occ factor calculated in the Xo loop
 ! evaluated at the fermi state
 !
 f_e=          Xen%f(Xen%bf,Xen%kf,Xen%sf) 
 f_h=(spin_occ-Xen%f(Xen%bf,Xen%kf,Xen%sf))
 drude_factor=f_e*f_h/(real(Xk%nbz,SP)*DL_vol*spin_occ)
 !
 do iw=1,Xw%n_freqs
   drude_GreenF(iw)=Jellium_drude(real(Xw%p(iw)),Wd,q_norm(1))/drude_factor
 enddo
 !
 contains
   !
   function Jellium_drude(W,Wd,q0_norm)
   !
   !See R.D. Mattuck "A guide to Feynmann diagrams in the Many-Body problem", pag. 197.
   !Jellium Xo propagator with a given plasma frequency.
   !The propagator is T-ordered.
   !Davide: If Drude is used at finite temperature / with fractional occupations
   !        I would need the retarded propagator
   !
   !  
   use pars,  ONLY:SP,pi,cI
   !
   implicit none
   !
   real(SP)    :: q0_norm,W
   complex(SP) :: Wd
   !
   ! Work Space
   !
   complex(SP) :: xi,Jellium_drude
   real(SP)    :: Kf,rxi,fac
   !
   Kf =((3._SP*pi*real(Wd)**2)/4._SP)**(1._SP/3._SP)
   fac=Kf*q0_norm
   xi =W+cI*aimag(Wd)*fac
   rxi=real(xi)
   !
   if (abs(W)<=10*fac) then
     Jellium_drude=-1._SP/(2._SP*pi**2*q0_norm)*(2._SP*fac+xi*log((xi-fac)/xi)-&
&                  conjg(xi)*log((conjg(xi)+fac)/conjg(xi)))
   else
     Jellium_drude=-Kf*fac/(2._SP*pi**2)*(-1._SP/(2._SP*xi)+1._SP/(2._SP*conjg(xi))-&
&                  fac/(3._SP*rxi**2)-fac/(3._SP*rxi**2))
   endif
   !
  end function Jellium_drude
  !
end subroutine
