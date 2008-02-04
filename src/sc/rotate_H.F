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
subroutine rotate_H(R,H,N,direction)
 !
 ! Transform H in the new/old basis defined by R
 !
 ! new basis |n'>
 ! old basis |n>
 !
 ! R(i,j)=<i|j'>
 !
 ! direction = +1 => <n'|H|k'>=sum_{m,p} <n'|m> H(m,p) <p|k'> =
 !                               sum_{m,p} R_(m,n')^* H(m,p) R(p,k) = 
 !                               sum_{m,p} R^+_(n',m) H(m,p) R(p,k)
 !
 ! direction = -1 => <n|H|k>=sum_{m,p} <n|m'> H(m',p') <p'|k> =
 !                               sum_{m,p} R_(n,m) H(m',p') R^*(k,p) = 
 !                               sum_{m,p} R_(n,m) H(m',p') R^+(p,k)
 !
 use pars,           ONLY:SP
 implicit none
 !
 integer     :: direction,N
 complex(SP) :: H(N,N),R(N,N)
 !
 ! Work Space
 !
 integer     :: i,j
 complex(SP) :: H_rot(N,N),R_(N,N)
 !
 if (direction == 1 ) R_=R
 if (direction == -1) forall(i=1:N,j=1:N) R_(i,j)=conjg(R(j,i))
 !
 ! [1] H_rot = (R_^{\dagger}) H
 !     H     = H_rot
 !
#if defined _DOUBLE
 call zgemm('c','n',N,N,N,(1._SP,0._SP),R_,N,H,N,(0._SP,0._SP),H_rot,N)
#else
 call cgemm('c','n',N,N,N,(1._SP,0._SP),R_,N,H,N,(0._SP,0._SP),H_rot,N)
#endif
 H = H_rot
 !
 ! [2] H_rot = H R_
 !     H     = H_rot
 !
#if defined _DOUBLE
 call zgemm('n','n',N,N,N,(1._SP,0._SP),H,N,R_,N,(0._SP,0._SP),H_rot,N)
#else
 call cgemm('n','n',N,N,N,(1._SP,0._SP),H,N,R_,N,(0._SP,0._SP),H_rot,N)
#endif
 H = H_rot
 !
end subroutine 
