!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
 integer,     intent(in)    :: direction,N
 complex(SP), intent(in)    :: R(N,N)
 complex(SP), intent(inout) :: O(N,N)
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
 !
 call M_by_M('c','n',N,R_,O,O_rot)
 !
 ! [2] O     = O_rot R_
 !
 call M_by_M('n','n',N,O_rot,R_,O)
 !
end subroutine 
