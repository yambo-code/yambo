!
!        Copyright (C) 2000-2020 the YAMBO team
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
integer function QP_state_extract(state)
 !
 ! Parser of the QP_state logical array
 !
 use QP_m,      ONLY:QP_state,QP_nb,QP_nk
 implicit none
 integer           :: state(4)
 ! 
 ! Work Space
 !
 integer :: i1,i2,i3,ik1,ik2,ib1,ib2,proposed_state(4)
 logical :: flag
 !
 QP_state_extract=1
 !
 ! state(1) = ik1
 ! state(2) = ik2
 ! state(3) = ib1
 ! state(4) = ib2
 !
3 continue
 !
 do i1=state(1),QP_nk
   i3=1
   if (i1==state(1)) i3=state(4)+1
   do i2=i3,QP_nb
     if (QP_state(i2,i1)) goto 1
   enddo
 enddo
 QP_state_extract=-1
 !
 return
 !
1 ik1=i1 
 ib1=i2
 !
 if (ib1==state(4).and.ik1==state(1)) then
   QP_state_extract=-1
   return
 endif
 !
 do i1=ib1,QP_nb
   if (.not.QP_state(i1,ik1)) goto 2
 enddo
 !
2 ib2=i1-1
 !
 ! Check FORWARD for a set of contiguous K-points with same bands range
 !
 ik2=ik1
 do i1=ik1+1,QP_nk
   flag=.true.
   do i2=ib1,ib2
     do i3=ik1+1,i1
       if (.not.QP_state(i2,i3)) flag=.false.
     enddo
   enddo
   if (flag.and.ib1/=1    ) flag=.not.QP_state(ib1-1,i1)
   if (flag.and.ib2/=QP_nb) flag=.not.QP_state(ib2+1,i1)
   if (flag) ik2=i1
 enddo
 !
 proposed_state=(/ik1,ik2,ib1,ib2/)
 !
 ! Check BACKWARD if ALL K-points have same bands range
 !
 do i1=ik1-1,1,-1
   flag=.true.
   do i2=ib1,ib2
     do i3=i1,ik1-1
       if (.not.QP_state(i2,i3)) flag=.false.
     enddo
   enddo
   if (flag.and.ib1/=1    ) flag=.not.QP_state(ib1-1,i1)
   if (flag.and.ib2/=QP_nb) flag=.not.QP_state(ib2+1,i1)
   if (flag) then
     state=proposed_state
     goto 3
   endif
 enddo
 !
 if (all((/state==proposed_state/))) then
   QP_state_extract=-1
 else
   state=proposed_state
 endif
 !
end function
