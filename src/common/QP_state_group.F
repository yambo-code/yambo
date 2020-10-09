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
subroutine QP_state_group(N,states)
 !
 ! Parser of the QP_state logical array and creation of group of indexes
 ! to be used in the input file and in the report
 !
 use QP_m,      ONLY:QP_state,QP_nb,QP_nk
 !
 implicit none
 !
 integer, intent(inout):: N
 integer, intent(inout):: states(iabs(N),4)
 ! 
 ! Work Space
 !
 integer :: ib,ik,i_scan,state_ws(4),i_last
 logical :: fill,QP_state_cpy(QP_nb,QP_nk)
 !
 ! states(i_s,1) = ik1
 ! states(i_s,2) = ik2
 ! states(i_s,3) = ib1
 ! states(i_s,4) = ib2
 !
 fill=N>0
 !
 N=0
 !
 QP_state_cpy=QP_state
 !
 do ik=1,QP_nk
   do ib=1,QP_nb
     !
     if (.not.QP_state_cpy(ib,ik)) cycle
     !
     QP_state_cpy(ib,ik)=.FALSE.
     !
     N=N+1
     state_ws=(/ik,ik,ib,ib/)
     !
     ! step 1: find the largest square box
     !
     BOX_LOOP: do i_scan=1,max(QP_nb,QP_nk)
       if (ik+i_scan>QP_nk.or.ib+i_scan>QP_nb)             exit BOX_LOOP
       if (.not.all(QP_state_cpy(ib:ib+i_scan,ik+i_scan))) exit BOX_LOOP
       if (.not.all(QP_state_cpy(ib+i_scan,ik:ik+i_scan))) exit BOX_LOOP
       QP_state_cpy(ib:ib+i_scan,ik+i_scan)    =.FALSE.
       QP_state_cpy(ib+i_scan,ik:ik+i_scan)    =.FALSE.
       state_ws(2)=ik+i_scan
       state_ws(4)=ib+i_scan
     enddo BOX_LOOP
     !
     ! step 2: try extending the band range
     !
     i_last=state_ws(4)
     B_LOOP: do i_scan=i_last+1,QP_nb
       if (.not.all(QP_state_cpy(i_scan,state_ws(1):state_ws(2)))) exit B_LOOP
       QP_state_cpy(i_scan,state_ws(1):state_ws(2))=.FALSE.
       state_ws(4)=i_scan
     enddo B_LOOP
     !
     ! step 3: try extending the k range
     !
     i_last=state_ws(2)
     K_LOOP: do i_scan=i_last+1,QP_nk
       if (.not.all(QP_state_cpy(state_ws(3):state_ws(4),i_scan))) exit K_LOOP
       QP_state_cpy(state_ws(3):state_ws(4),i_scan)=.FALSE.
       state_ws(2)=i_scan
     enddo K_LOOP
     !
     if (fill) states(N,:)=state_ws
     !
   enddo
 enddo
 !
end subroutine
