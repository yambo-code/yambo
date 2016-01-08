!
! Copyright (C) 2000-2016 A. Marini and the YAMBO team
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
subroutine X_Drude(iq,Xen,Xk,Xw,Wd,drude_GreenF,drude_ordering)
 !
 use pars,          ONLY:SP,cZERO,schlen
 use units,         ONLY:HA2EV
 use com,           ONLY:msg
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ
 use R_lattice,     ONLY:bz_samp,q_norm 
 use D_lattice,     ONLY:DL_vol
 use X_m,           ONLY:iw_ref
 use com,           ONLY:warning
 !
 implicit none
 !
 type(levels)         :: Xen
 type(bz_samp)        :: Xk
 complex(SP)          :: Wd
 type(w_samp)         :: Xw
 !
 integer              :: iq
 complex(SP)          :: drude_GreenF(Xw%n(2))
 character(1)         :: drude_ordering
 !
 real(SP)             :: drude_factor
 logical              :: Drude_term
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
 Drude_term=all((/real(Wd)>0.,aimag(Wd)>0.,Xen%kf>0,Xen%nbf/=Xen%nbm,iq==1/))
 !
 if(.not.Drude_term .and. iq==1) then
   if(Xen%nbf==Xen%nbm.and.all((/real(Wd)>0.,aimag(Wd)>0./)) ) then
     call warning(' System is not a metal. Drude term not included')
     Wd=(0._SP,0._SP)
   endif
   if(Xen%nbf/=Xen%nbm) then
     if(real(Wd)>0. .and. aimag(Wd)>0.)   call warning(' Too low ElecTemp. Drude term not included.')
     if(real(Wd)==0. .or. aimag(Wd)==0.)  call warning(' The system is a metal but Drude term not included.')
   endif
 endif
 !
 if(.not.Drude_term) return
 !
 call msg('nrs','[X/K] Drude contribution @[ev]:',(/real(Wd),aimag(Wd)/)*HA2EV)
 !
 ! drude_factor is the eh_occ factor calculated in the Xo loop
 ! evaluated at the fermi state
 !
 drude_factor=Xen%f(Xen%bf,Xen%kf,Xen%sf)*&
&             (spin_occ-Xen%f(Xen%bf,Xen%kf,Xen%sf))/ &
&             (spin_occ*real(Xk%nbz)*DL_vol)
 !
 do iw=1,Xw%n(1)
   drude_GreenF(iw)=Jellium_drude(real(Xw%p(iw)),Wd,q_norm(1))/drude_factor
 enddo
 !
 if(drude_ordering/='c') return
 !
 call msg('nsr','[X/K] Drude reference freq. for w=0 is [eV]:',real(Xw%p(iw_ref))*HA2EV )
 !
 do iw=1,iw_ref-1
   drude_GreenF(iw_ref-iw)=cmplx(real(Drude_GreenF(iw_ref-iw)),-aimag(Drude_GreenF(iw_ref+iw)))
 enddo
 !
 contains
   !
   function Jellium_drude(W,Wd,q0_norm)
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
   complex(SP) :: xi,Jellium_drude
   real(SP)    :: Kf,rxi,fac
   !
   Kf =((3.*pi*real(Wd)**2.)/4.)**(1./3.)
   fac=Kf*q0_norm
   xi =W+(0.,1.)*aimag(Wd)*fac
   rxi=real(xi)
   !
   if (abs(W)<=10*fac) then
     Jellium_drude=-1./(2.*pi**2.*q0_norm)*(2.*fac+xi*log((xi-fac)/xi)-&
&                  conjg(xi)*log((conjg(xi)+fac)/conjg(xi)))
   else
     Jellium_drude=-Kf*fac/(2.*pi**2.)*(-1./(2.*xi)+1./(2.*conjg(xi))-&
&                  fac/(3.*rxi**2.)-fac/(3.*rxi**2.))
   endif
   !
  end function Jellium_drude
  !
  !
end subroutine
