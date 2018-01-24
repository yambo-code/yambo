!
!        Copyright (C) 2000-2018 the YAMBO team
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
integer function DYSON_bracket(x1,f1,x2,f2)
 !
 use pars,ONLY: SP
 !
 implicit none
 real(SP)    :: f1,f2,x1,x2
 !
 ! Work Space
 !
 real(SP),parameter :: precision=1.E-5,factor=1.6
 !
 DYSON_bracket=0
 if (f1*f2<precision) return
 if (abs(f1)<abs(f2)) then
   x1=x1+factor*(x1-x2)
   DYSON_bracket=1
 else
   x2=x2+factor*(x2-x1)
   DYSON_bracket=2
 endif
 !
end function
