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
 function damping(er,dr,E)
!-------------------------
 use pars,  ONLY: SP
 real(SP):: damping,E,er(2),dr(2)
!ws
 integer :: isign
 real(SP):: e1,e2,d1,d2
 isign=-2
 if (er(1)>=0..and.er(2)>0.) isign=1
 if (er(1)<0..and.er(2)<=0.) isign=-1
 if (er(1)<0..and.er(2)>0.) isign=0
 e1=er(1)
 e2=er(2)
 d1=dr(1)
 d2=dr(2)
 select case(isign)
   case(1)
     d1=dr(1)
     d2=dr(2)
   case(-1)
     d1=dr(2)
     d2=dr(1)
   case(0)
    if (E>=0.) then
      d1=dr(1)
      d2=dr(2)
      e1=0.
      e2=er(2)
    else
      d1=dr(2)
      d2=dr(1)
      e1=er(1)
      e2=0.
    endif
 end select
 damping=d1+(d2-d1)/(e2-e1)*(E-e1)
 end function
