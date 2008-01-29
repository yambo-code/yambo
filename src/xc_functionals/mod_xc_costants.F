!
! Copyright (C) 1998-2005 ABINIT group (DCA, XG, GMR)
! For the initials of contributors, see ~ABINIT/Infos/contributors
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
module xc_constants
 !
 use pars,     ONLY:DP
 implicit none
 !
 ! Real constants
 !
 real(DP), parameter :: zero=0._DP
 real(DP), parameter :: one=1._DP
 real(DP), parameter :: two=2._DP
 real(DP), parameter :: three=3._DP
 real(DP), parameter :: four=4._DP
 real(DP), parameter :: five=5._DP 
 real(DP), parameter :: six=6._DP
 real(DP), parameter :: seven=7._DP
 real(DP), parameter :: eight=8._DP
 real(DP), parameter :: nine=9._DP
 real(DP), parameter :: ten=10._DP
 !
 ! Fractionary real constants
 !
 real(DP), parameter :: half=0.50_DP
 real(DP), parameter :: third=one/three
 real(DP), parameter :: quarter=0.25_DP
 real(DP), parameter :: fifth=0.20_DP
 real(DP), parameter :: sixth=one/six
 real(DP), parameter :: seventh=one/seven
 real(DP), parameter :: eighth=0.125_DP
 real(DP), parameter :: ninth=one/nine
 real(DP), parameter :: two_thirds=two*third
 real(DP), parameter :: four_thirds=four*third
 real(DP), parameter :: three_quarters=0.75_DP
 !
end module 
