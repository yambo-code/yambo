!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
function X_simple_GreenF(W,pole,ordering)
 !
 use pars, ONLY:SP
 implicit none
 real(SP)     ::pole
 complex(SP)  ::W,X_simple_GreenF
 character(*) ::ordering
 !
 if (trim(ordering)=="t")  X_simple_GreenF=1./(W-pole)-1./(conjg(W)+pole)
 if (trim(ordering)=="c")  X_simple_GreenF=1./(W-pole)-1./(W+pole)       
 if (trim(ordering)=="r")  X_simple_GreenF=1./(W-pole)                   
 if (trim(ordering)=="a")  X_simple_GreenF=           -1./(conjg(W)+pole)
 if (trim(ordering)=="ca") X_simple_GreenF=           -1./(W+pole)
 !
end function
