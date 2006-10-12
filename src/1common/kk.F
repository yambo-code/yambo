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
 subroutine kk(fi,wi,ni,fo,wo,no,y0)
!===================================
!
!Calculates Kramers-Kronig transformation:
!
!fo(i) = Int_{wi(1)}^{wi(last)} fi(y)/(wo(i)-y-y0) dy
!
!Remember wo MUST HAVE A NON ZERO IMAGINARY PART.
!
!wi must be real
!
!fo is not INITIALIZED to 0. !
!
!-------------------------------------------------
 use pars,    ONLY:SP
 implicit none
 integer      :: ni,no
 real(SP)     :: wi(ni)
 complex(SP)  :: fi(ni),fo(no),wo(no),y0
!ws
 integer      :: i1,i2
 complex(SP)  :: m(ni-1),q(ni-1)

 do i1=1,ni-1
   m(i1)=(fi(i1)-fi(i1+1))/(wi(i1)-wi(i1+1))
   q(i1)=fi(i1)-m(i1)*wi(i1)
 enddo
 do i1=1,no
   do i2=1,ni-1
     fo(i1)=fo(i1)+m(i2)*(wi(i2)-wi(i2+1))+&
&           (q(i2)+m(i2)*(wo(i1)-y0))*&
&           log((wi(i2)-wo(i1)+y0)/(wi(i2+1)-wo(i1)+y0))
   enddo
 enddo

 end subroutine

