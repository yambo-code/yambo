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
 function X_Gf(W,pole,ordering)
!==============================
 use pars, ONLY:SP,pi
 implicit none
 real(SP)    ::pole
 complex(SP) ::W,X_Gf
 character(*)::ordering
!
 if (ordering=="t") X_Gf=1./(W-pole)-1./(conjg(W)+pole)
 if (ordering=="c") X_Gf=1./(W-pole)-1./(W+pole)       
 if (ordering=="r") X_Gf=1./(W-pole)                   
 if (ordering=="a") X_Gf=           -1./(conjg(W)+pole)

 end function

