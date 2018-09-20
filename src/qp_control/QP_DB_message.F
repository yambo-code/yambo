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
subroutine QP_DB_message( i_ACTION, qp )
 !
 use pars,     ONLY:lchlen
 use QP_m,     ONLY:QP_t
 use com,      ONLY:msg
 use QP_CTL_m, ONLY:QP_action,QP_ctl_msg_fmt
 !
 implicit none
 !
 integer    :: i_ACTION
 type(QP_t) :: qp
 !
 ! Work Space
 !
 integer, parameter :: n_fields=7
 integer            :: if,c_pos,i_msg,i1
 character(lchlen)  :: field(n_fields,2)
 !
 field(1,1)="X G`s            [used]"
 field(1,2)="XG"
 field(2,1)="X bands"
 field(2,2)="Xb"
 field(3,1)="Sc/G bands"
 field(3,2)="Scb"
 field(4,1)="Sc. G bands"
 field(4,2)="Sphb"
 field(5,1)="PPA"
 field(5,2)="E-E PPA"
 field(6,1)="El-Ph Self"
 field(6,2)="E-P"
 field(7,1)="Real Axis GW"
 field(7,2)="E-E"
 !
 ! Here I build the %desc char on the basis of the qp%description lines.
 ! This to give a compact description of the QP details.
 !
 i_msg=1
 if (QP_action(i_ACTION)%plan(1)) then
   QP_action(i_ACTION)%desc(i_msg:i_msg)="E"
   i_msg=i_msg+1
 endif
 if (QP_action(i_ACTION)%plan(2)) then
   QP_action(i_ACTION)%desc(i_msg:i_msg)="W"
   i_msg=i_msg+1
 endif
 if (QP_action(i_ACTION)%plan(3)) then
   QP_action(i_ACTION)%desc(i_msg:i_msg)="Z"
   i_msg=i_msg+1
 endif
 !
 QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//'<'//trim(QP_action(i_ACTION)%CTL(1)%db)//'['
 i_msg=i_msg+2+len_trim(QP_action(i_ACTION)%CTL(1)%db)
 !
 do i1=1,qp%n_descs
   do if=1,n_fields
     if (index(qp%description(i1),trim(field(if,1)))/=0) then
       c_pos=index(qp%description(i1),":")+1
       QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//' '//trim(field(if,2))
       if (c_pos>1) call CHappend(":"//qp%description(i1)(c_pos:))
     endif
   enddo
 enddo
 !
 QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//']'
 !
 call msg("n"//trim(QP_ctl_msg_fmt),trim(QP_action(i_ACTION)%desc))
 ! 
 contains
  !
  subroutine CHappend(c)
    character(*)::c
    integer     ::ic,ic_start,ic_end
    !
    ic_start=-1    
    ic_end=-1    
    do ic=1,len(c)
      if (c(ic:ic)/=' '.and.ic_start<0) ic_start=ic
      if (c(ic:ic)==' '.and.ic_start>0) ic_end=ic
    enddo
    !
    QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//c(ic_start:ic_end)
  end subroutine
  !
end subroutine
     !
