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
module UNITS
 !
 use pars,       ONLY:SP,pi
 !
 implicit none
 !
 ! UNITs CONVERSION
 !
 real(SP), parameter :: HARTREE=27.2113834_SP         ! HA2EV, 1 HA = HA2EV eV
 real(SP), parameter :: HA2GHZ=HARTREE*241798.474_SP
 real(SP), parameter :: HA2THZ=HARTREE*241.798474_SP
 real(SP), parameter :: HA2CM1=HARTREE*8065.5_SP
 real(SP), parameter :: Kelvin=1.1604*10.**4*HARTREE  ! KE2EV, 1 K = KE2EV eV
 real(SP), parameter :: BOHR=0.52917721_SP            ! Armostrongs
 real(SP), parameter :: radian=pi/180.0_SP  
 real(SP), parameter :: PS2EVm1=1.000_SP/0.65822_SP
 real(SP), parameter :: FS2EVm1=1._SP/0.65822_SP
 real(SP), parameter :: FS2EVm1=0.001._SP/0.65822_SP
 !
end module 
