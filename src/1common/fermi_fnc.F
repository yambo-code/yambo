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
 function Ffermi(E,T)
!--------------------
 use pars
 implicit none
 real(SP) :: E,T,Ffermi
 real(SP),parameter :: tresh=1.E-5
 Ffermi=1.
 if (T==0..and.E>0.) Ffermi=0.
 if (T/=0.) Ffermi=1./(1.+exp(E/T))
 if (Ffermi<tresh)  Ffermi=0.
 end function
