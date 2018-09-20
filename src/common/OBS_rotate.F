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
subroutine OBS_rotate(R,O,N,direction)
 !
 ! Transform O in the new/old basis defined by R
 !
 ! new basis |n'>
 ! old basis |n>
 !
 ! R(i,j)=<i|j'>
 !
 ! direction = +1 => <n'|O|k'>=sum_{m,p} <n'|m> O(m,p) <p|k'> =
 !                               sum_{m,p} R_(m,n')^* O(m,p) R(p,k) = 
 !                               sum_{m,p} R^+_(n',m) O(m,p) R(p,k)
 !
 ! direction = -1 => <n|O|k>=sum_{m,p} <n|m'> O(m',p') <p'|k> =
 !                               sum_{m,p} R_(n,m) O(m',p') R^*(k,p) = 
 !                               sum_{m,p} R_(n,m) O(m',p') R^+(p,k)
 !
 use pars,           ONLY:SP
 use wrapper,        ONLY:M_by_M
 !
 implicit none
 !
 integer     :: direction,N
 complex(SP) :: O(N,N),R(N,N)
 !
 ! Work Space
 !
 integer     :: i,j
 complex(SP) :: O_rot(N,N),R_(N,N)
 !
 if (direction ==  1) R_=R
 if (direction == -1) forall(i=1:N,j=1:N) R_(i,j)=conjg(R(j,i))
 !
 ! [1] O_rot = (R_^{\dagger}) O
 !     O     = O_rot
 !
 call M_by_M('c','n',N,R_,O,O_rot)
 O = O_rot
 !
 ! [2] O_rot = O R_
 !     O     = O_rot
 !
 call M_by_M('n','n',N,O,R_,O_rot)
 O = O_rot
 !
end subroutine 
