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
 subroutine polfit(N,X,Y,order,Tt,Ferr,t_zero)
!=============================================
 use pars
 implicit none
 integer  :: N,order
 real(DP):: X(N),Y(N),Tt(order+1),Ferr,t_zero
!ws
 integer :: NDEG
 real(DP) :: W(N),R(N),IERR,wk(3*N+3*order+3)
!
 Ferr=0.
 if (N<order) return
 W(:)=1./sum(Y(:)**2.)
 Ferr=-1.
 call DPOLFT (N,X,Y,W,order,NDEG,Ferr,R,IERR,wk)
 if (NDEG<order) then
  Ferr=-1.
  return
 endif
 call DPCOEF (order,t_zero,Tt,wk)

 end subroutine
