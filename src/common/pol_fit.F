!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine pol_fit(N,X,Y,order,Tt,EPS,t_zero)
 use pars, ONLY:SP
 implicit none
 !
 integer  :: N,order
 real(SP) :: X(N),Y(N),Tt(order+1),EPS,t_zero
 ! 
 ! Work Space
 !
 integer  :: NDEG
 real(SP) :: W(N),R(N),IERR,wk(3*N+3*order+3)
 !
 W(:)=1./sum(Y(:)**2.)
 !
 if (N<=1) then
   EPS=-1._SP
   return
 endif
 !
 ! Simple linear fit on 2 points
 !
 if (N==2.and.order==1) then
   Tt(2) = ( Y(1)-Y(2) ) / ( X(1) - X(2) )
   Tt(1) = Y(1) - Tt(2)*X(1)
   return
 endif
 !
 ! General fit on N points
 !
 Tt=0.
 EPS=-1._SP
#if defined _DOUBLE
 call DPOLFT (N,X,Y,W,order,NDEG,EPS,R,IERR,wk)
 order=NDEG 
 call DPCOEF (order,t_zero,Tt(:order+1),wk)
#else
 call POLFIT (N,X,Y,W,order,NDEG,EPS,R,IERR,wk)
 order=NDEG 
 call PCOEF (order,t_zero,Tt(:order+1),wk)
#endif
 !
end subroutine
