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
subroutine gauss_leg_grid(x1,x2,x,w,n)
 !-----------------------------------------------------------------
 ! Setup of a Gauss-Legendre grid (taken from Numerical Recipees)
 ! Inputs:
 !        x1: lower limit of the grid (real)
 !        x2: upper limit of the grid (real)
 !         n: number of grid points   (integer)
 ! Outputs:
 !        x(n):GL grid (real)
 !        w(n):GL integration weigths (real)
 !
 ! NOTE:  w(n) includes the factor 1/(2*pi)
 !-----------------------------------------------------------------
 use pars, ONLY:SP,DP,pi
 implicit none
 integer:: n
 real(SP):: x1,x2,x(n),w(n)
 ! 
 ! Work Space
 !
 real(DP),parameter:: eps=3.d-14
 integer ::i,j,m
 real(DP)::p1,p2,p3,pp,xl,xm,z,z1
 !
 m=(n+1)/2
 xm=0.5d0*(x2+x1)
 xl=0.5d0*(x2-x1)
 do i=1,m
   z=dcos(pi*(i-.25d0)/(n+.5d0))
1  continue
   p1=1.d0
   p2=0.d0
   do j=1,n
     p3=p2
     p2=p1
     p1=((2.d0*j-1.d0)*z*p2-(j-1.d0)*p3)/j
   end do
   pp=n*(z*p1-p2)/(z*z-1.d0)
   z1=z
   z=z1-p1/pp
   if(abs(z-z1).gt.eps)goto 1
   x(i)=xm-xl*z
   x(n+1-i)=xm+xl*z
   w(i)=xl/((1.d0-z*z)*pp*pp*pi)
   w(n+1-i)=w(i)
 end do
 !
end subroutine
