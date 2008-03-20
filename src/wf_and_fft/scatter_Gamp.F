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
 !
 ! isc%gamp(G,G') = \int_p (region around q) 1/|p+G|/|p+G'|/(2*pi)**3
 !
 ! When mode=='x' the standard Coulomb integral is used. When mode='c' the 
 ! isc%gamp are used multiplied by eps^-1 so, if present, the anisotropy
 ! is incuded.
 !
 use pars,          ONLY:SP,pi
 use drivers,       ONLY:l_col_cut
 use collision,     ONLY:ggwinfo
 use R_lattice,     ONLY:d3q_factor,RIM_ng,RIM_qpg,RIM_is_diagonal,&
&                        bare_qpg,RIM_anisotropy,RIM_n_rand_pts
 implicit none
 type(ggwinfo)::isc
 character(1) ::mode
 !
 !Working Space
 !
 integer :: ng(2),ig1,ig2,iq,i1,i2
 logical :: l_RIM
 real(SP):: reg_q_m2,q_weight,R_sphere
 !
 ! q_weight = 1./(DL_vol*q%nbz)
 !
 q_weight=d3q_factor/(2.*pi)**3. 
 !
 ! Note that
 !
 ! \int_q (region) 1/q^2 /(2*pi)**3 = CONSTANT / (2 pi)**3 (Omega_RL/NQBZ)^1/3 
 !                                  = CONSTANT / (2 pi)**3 d3q_factor^1/3
 ! where
 !
 ! CONSTANT = 7.7956 (spherical region)
 ! CONSTANT = 7.44   ("Godby" region)
 !
 ! reg_q_m2 = \int_q (region) 1/q^2 /(2*pi)**3
 !
 reg_q_m2=7.44/(2.*pi)**3.*d3q_factor**(1./3.)
 !
 ! In the case of a spherical region the radius is
 !
 R_sphere=(3./4./pi)**(1./3.)*d3q_factor**(1./3.)
 !
 iq=isc%qs(2)
 isc%iqref=iq
 !
 ! RIM support ?
 !
 if (.not.allocated(RIM_qpg)) then
   RIM_ng=0
   RIM_n_rand_pts=0
 else
   reg_q_m2=RIM_qpg(iq,1,1)/2.
 endif
 !
 ng=shape(isc%gamp)
 !
 do i1=1,ng(1)
   do i2=1,ng(2)
     ig1=i1
     ig2=i2
     if (ng(1)==1) ig1=ig2
     if (ng(2)==1) ig2=ig1
     !
     ! RIM support (Both components)
     !
     l_RIM=all((/ig1<=RIM_ng,ig2<=RIM_ng/))
     if (RIM_is_diagonal.and.l_RIM) l_RIM=(ig1==ig2)
     !
     if (l_RIM.and..not.l_col_cut) then
       isc%gamp(i1,i2)=RIM_qpg(iq,ig1,ig2)/2.
       cycle
     else
       isc%gamp(i1,i2)=q_weight/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)
     endif
     !
     ! Head and wings point special treatment (with no RIM only Gamma is possible)
     !
     if ( (RIM_ng==0.and.iq>1) ) cycle
     !
     ! When using the CUTOFF all the components of the Col potential are 
     ! already regolarized.
     !
     if (l_col_cut) cycle
     !
     ! Wings (0,G) & (G,0) components using the Sphere approx for the region around Gamma
     ! and the square root approx for other components:
     !
     ! \int_q (region) 1/q /(2*pi)**3 = R_sphere/2. * reg_q_m2 
     ! 
     ! for q = 0 
     ! 
     ! \int_q (region) 1/q /(2*pi)**3 \sim 
     !                 sqrt(\int_q (region) 1/q^2 /(2*pi)**3 ) / 
     ! 
     ! for q != 0 
     !
     if (ig1==1.and.(ig2>RIM_ng.or.(RIM_is_diagonal.and.ig2>1))) then
       !
       if (iq==1) then 
         isc%gamp(i1,i2)=R_sphere/2.*reg_q_m2/bare_qpg(iq,ig2)
       else
         !
         ! the sqrt(q_weight) is needed as the sqrt(reg_q_m2) contains
         ! implicitly the q_weight
         ! 
         isc%gamp(i1,i2)=isc%gamp(i1,i2)*& 
&                         bare_qpg(iq,ig1)*sqrt(reg_q_m2)/sqrt(q_weight)
       endif
     endif
     if (ig2==1.and.(ig1>RIM_ng.or.(RIM_is_diagonal.and.ig1>1))) then
       if (iq==1) then
         isc%gamp(i1,i2)=R_sphere/2.*reg_q_m2/bare_qpg(iq,ig1)
       else 
         isc%gamp(i1,i2)=isc%gamp(i1,i2)*& 
&                         bare_qpg(iq,ig2)*sqrt(reg_q_m2)/sqrt(q_weight)
       endif
     endif
     !
     ! head component.
     !
     if (ig1==1.and.ig2==1) isc%gamp(i1,i2)=reg_q_m2
     !
   enddo
 enddo
 !
 ! Anisotropy correction
 !
 if (.not.l_col_cut) then
   if (all((/iq==1,RIM_ng>0,mode=='c',RIM_anisotropy/=0./))) isc%gamp(1,1)=RIM_anisotropy/2.
 endif
 !
end subroutine
