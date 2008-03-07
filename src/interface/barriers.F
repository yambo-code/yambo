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
integer function barriers()
 !
 ! SPIN => no  GW real axis, LIFETIMES, ACFDT, EL-PH
 !
 !
 !
 !
 !
 !
 use drivers
 use electrons,  ONLY:n_spin
 implicit none
 !
 barriers=0
 !
 ! SELF-CONSISTENT 
 ! - no spin support
 !
 if (l_sc_run) then
   if (n_spin/=1) barriers=-1
 endif
 !
 ! GPL barriers
 !
 if (gpl_version.and.any((/&
&          l_life,l_acfdt,l_bs_fxc,l_lrc_fxc,&
#if defined _RAS
&          lras,lreels,&
#endif
&          l_gw0.and..not.l_ppa&
&         /))) then
   barriers=-1
 endif
 !
 ! SPIN barriers
 !
 if (n_spin==2.and.any((/&
&          l_life,l_acfdt,l_lrc_fxc,l_sc_run,&
&          l_gw0.and..not.l_ppa,l_ph_corr&
&         /))) then
   barriers=-1
 endif
 !
end function
