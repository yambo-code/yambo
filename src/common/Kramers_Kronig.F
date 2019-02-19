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
subroutine Kramers_Kronig(fi,wi,ni,fo,wo,no,y0)
 !
 ! Calculates Kramers-Kronig transformation:
 !
 ! fo(i) = Int_{wi(1)}^{wi(last)} fi(y)/(wo(i)-y-y0) dy
 !
 ! Remember wo MUST HAVE A NON ZERO IMAGINARY PART.
 !
 ! wi must be real
 !
 ! fo is not INITIALIZED to 0. !
 !
 use pars,    ONLY:SP
 implicit none
 integer,     intent(in)    :: ni,no
 real(SP),    intent(in)    :: wi(ni)
 complex(SP), intent(in)    :: fi(ni),wo(no),y0
 complex(SP), intent(inout) :: fo(no)
 !
 ! Work Space
 !
 integer      :: i1,i2
 complex(SP)  :: m(ni-1),q(ni-1)
 !
 do i1=1,ni-1
   m(i1)=(fi(i1)-fi(i1+1))/(wi(i1)-wi(i1+1))
   q(i1)=fi(i1)-m(i1)*wi(i1)
 enddo
 !
 do i1=1,no
   do i2=1,ni-1
     fo(i1)=fo(i1)+m(i2)*(wi(i2)-wi(i2+1))+&
&           (q(i2)+m(i2)*(wo(i1)-y0))*&
&           log((wi(i2)-wo(i1)+y0)/(wi(i2+1)-wo(i1)+y0))
   enddo
 enddo
 !
end subroutine
