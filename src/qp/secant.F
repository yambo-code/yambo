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
integer function secant(x1,f1,x2,f2,acc)
 !
 use pars, ONLY:SP
 !
 implicit none
 real(SP)     :: f1,f2,x1,x2,acc
 !
 ! Work Space
 !
 real(SP):: Dx
 !
 secant=0
 if (abs(f1)<=acc) secant=-1
 if (abs(f2)<=acc) secant=-2
 if (abs(f1)<=acc.or.abs(f2)<=acc) return
 if (abs(f1).lt.abs(f2)) then
   secant=2
   Dx=-f2*(x1-x2)/(f1-f2)
   x2=x2+Dx
 else if (abs(f1).gt.abs(f2)) then
   secant=1
   Dx=-f1*(x1-x2)/(f1-f2)
   x1=x1+Dx
 endif
 !
end function
