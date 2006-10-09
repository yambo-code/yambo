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
 function dFfermi(E,T)
!---------------------
! This is the energy derivative of the Fermi-Dirac function.
! It can be considered and approximation to the delta function.
! 
! Note that the function defined here is integrated to 1.
!
 use pars
 implicit none
 real(SP) :: E,T,dFfermi
 real(SP),parameter :: tresh=0.
 dFfermi=0.
 if (T/=0.) dFfermi=1./(2.+exp(E/T)+exp(-E/T))/T
 if (dFfermi<tresh)  dFfermi=0.
 end function
!
