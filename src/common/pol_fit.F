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
subroutine pol_fit(N,X,Y,order,Tt,EPS,t_zero)
 !
 use pars, ONLY:SP
 !
 implicit none
 !
 integer, intent(in)    :: N
 integer, intent(inout) :: order
 real(SP),intent(in)    :: X(N),Y(N),t_zero
 real(SP),intent(out)   :: Tt(order+1),EPS
 ! 
 ! Work Space
 !
 integer  :: NDEG,IERR
 real(SP) :: W(N),R(N),wk(3*N+3*order+3)
 !
 W(:)=1._SP/sum(Y(:)**2)
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
 Tt = 0._SP
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
