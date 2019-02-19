!
!        Copyright (C) 2000-2019 the YAMBO team
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
integer function QP_secant(x1,f1,x2,f2,acc,nstep)
 !
 use pars, ONLY:SP
 !
 implicit none
 real(SP)     :: f1,f2,x1,x2,acc
 integer      :: nstep
 !
 ! Work Space
 !
 real(SP):: Dx
 !
 QP_secant=0
 if (abs(f1)<=acc) QP_secant=-1
 if (abs(f2)<=acc) QP_secant=-2
 if (abs(f1)<=acc.or.abs(f2)<=acc) return
 if (abs(f1)<abs(f2)) then
   QP_secant=2
   Dx=-f2*(x1-x2)/(f1-f2)
   Dx=Dx/(1+nstep/20)
   x2=x2+Dx
 else if (abs(f1)>abs(f2)) then
   QP_secant=1
   Dx=-f1*(x1-x2)/(f1-f2)
   Dx=Dx/(1+nstep/20)
   x1=x1+Dx
 else if (abs(f1)==abs(f2)) then
   QP_secant=2
   x2=x2+abs(f1)/(2+nstep/20)
 endif
 !
end function
