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
 integer function qp_state_extract(state)
!========================================
 use pars
 use QP_m,      ONLY:QP_state,QP_nb,QP_nk
 implicit none
 integer           :: state(4)
!ws
 integer :: i1,i2,i3,ik1,ik2,ib1,ib2,proposed_state(4)
 logical :: flag

 qp_state_extract=1

! state(1) = ik1
! state(2) = ik2
! state(3) = ib1
! state(4) = ib2

3 continue

 do i1=state(1),QP_nk
   i3=1
   if (i1==state(1)) i3=state(4)+1
   do i2=i3,QP_nb
     if (QP_state(i2,i1)) goto 1
   enddo
 enddo
 qp_state_extract=-1
 return
 
1 ik1=i1 
  ib1=i2

 if (ib1==state(4).and.ik1==state(1)) then
   qp_state_extract=-1
   return
 endif

 do i1=ib1,QP_nb
   if (.not.QP_state(i1,ik1)) goto 2
 enddo

2 ib2=i1-1

 ik2=ik1
 do i1=ik1+1,QP_nk
   flag=all((/QP_state(ib1:ib2,i1)/))
   if (flag.and.ib1/=1) flag=.not.QP_state(ib1-1,i1)
   if (flag.and.ib2/=QP_nb) flag=.not.QP_state(ib2+1,i1)
   if (flag) ik2=i1
 enddo

 proposed_state=(/ik1,ik2,ib1,ib2/)

 do i1=ik1-1,1,-1
   flag=all((/QP_state(ib1:ib2,i1)/))
   if (flag.and.ib1/=1) flag=.not.QP_state(ib1-1,i1)
   if (flag.and.ib2/=QP_nb) flag=.not.QP_state(ib2+1,i1)
   if (flag) then
     state=proposed_state
     goto 3
   endif
 enddo

 if (all((/state==proposed_state/))) then
   qp_state_extract=-1
 else
   state=proposed_state
 endif

 end function

