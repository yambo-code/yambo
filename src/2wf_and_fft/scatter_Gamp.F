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
 subroutine scatterGamp(isc,mode)
!================================
!
!mode is 'x'/'c'. When 'c' isc%gamp(1,1) refers
!to the qpg_anisotropy (if db.qpgint exists!)
!Wen mode=='x' the standard Coulomb integral is used.
!
 use pars,          ONLY:SP,pi
 use drivers,       ONLY:l_col_cut
 use collision,     ONLY:ggwinfo
 use R_lattice,     ONLY:d3q_factor,RIM_ng,RIM_qpg,RIM_is_diagonal,&
&                        bare_qpg,RIM_anisotropy,RIM_n_rand_pts
 implicit none
 type(ggwinfo)::isc
 character(1) ::mode
!ws
 integer :: ng(2),ig1,ig2,iq,ig
 logical :: lRIM
 real(SP):: aqpg,Co
!
 Co=d3q_factor/(2.*pi)**3. ! = 1./(DL_vol*q%nbz)
!--------------------------------------------------------------------
!Note that <1/q^2> = CONSTANT (Omega_RL/NQBZ)^-2/3 
!                  = CONSTANT d3q_factor^-2/3
!where
!CONSTAN = 7.7956 (spherical region)
!CONSTAN = 7.44   ("Godby" region)
!
!Now |q+G| = 1./sqrt(CONSTANT) d3q_factor^1/3
!----------------------------------------------------------------
 aqpg=1./sqrt(7.44)*d3q_factor**(1./3.)
!----------------------------------------------------------------
 iq=isc%qs(2)
 isc%iqref=iq
 if (.not.allocated(RIM_qpg)) then
   RIM_ng=0
   RIM_n_rand_pts=0
 endif
 ng=shape(isc%gamp)
 do ig=1,ng(1)
   do ig2=1,ng(2)
     ig1=ig
     if (ng(1)==1) ig1=ig2
     lRIM=all((/ig1<=RIM_ng,ig2<=RIM_ng/))
     if (RIM_is_diagonal.and.lRIM) lRIM=(ig1==ig2)
     if (lRIM) isc%gamp(ig,ig2)=RIM_qpg(iq,ig1,ig2)/2.
     if (.not.lRIM) then 
       isc%gamp(ig,ig2)=Co/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)
!
! When using the CUTOFF the (1,1) component of the Col potential
! is already regolarized.
!
       if (.not.l_col_cut) then
         if (iq==1.and.ig1==1) isc%gamp(ig,ig2)=isc%gamp(ig,ig2)*bare_qpg(iq,ig1)/aqpg
         if (iq==1.and.ig2==1) isc%gamp(ig,ig2)=isc%gamp(ig,ig2)*bare_qpg(iq,ig2)/aqpg
       endif
     endif
   enddo
 enddo
!
 if (all((/RIM_ng>0,mode=='c',iq==1,RIM_anisotropy/=0/))) isc%gamp(1,1)=RIM_anisotropy/2.
 end subroutine
!
