!
!        Copyright (C) 2000-2017 the YAMBO team
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
function Laplace_transform(F,N,dT,Eo)
 !
 use pars,    ONLY:SP,cI
 implicit none
 integer,  intent(in)  :: N
 real(SP), intent(in)  :: dT,Eo
 real(SP), intent(in)  :: F(N)
 real(SP)              :: Laplace_transform
 !
 ! Work Space
 !
 integer   :: i_t
 !
 Laplace_transform=0._SP
 !
 if (abs(Eo)>1.E-5) then
   do i_t=1,N-1
     Laplace_transform=Laplace_transform+aimag(F(i_t)*exp(cI*i_t*dT*Eo)/Eo*(exp(cI*dT*Eo)-1))
   enddo
 else
   do i_t=1,N
     Laplace_transform=Laplace_transform+F(i_t)*dT
   enddo
 endif
 !
end function
