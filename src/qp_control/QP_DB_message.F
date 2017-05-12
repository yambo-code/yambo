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
subroutine QP_DB_message( )
 !
 use pars, ONLY:lchlen
 !
 integer, parameter :: n_fields=4
 integer            :: if,c_pos
 character(lchlen)  :: field(n_fields,2),qp_msg
 !
 field(1,1)="X G`s            [used]"
 field(1,2)="XG"
 field(2,1)="X bands"
 field(2,2)="Xb"
 field(3,1)="Sc/G bands"
 field(3,2)="Scb"
 field(4,1)="Sc. G bands"
 field(4,2)="Sphb"
 !
 ! Here I build the qp_msg char on the basis of the qp%description lines.
 ! This to give a compact description of the QP details.
 ! To be used in the DB headers.
 !  
! if (do_EWZ_using_Ik_IE(1)) then
!   qp_msg(i_msg:i_msg)="E"
!   i_msg=i_msg+1
! endif
! if (do_EWZ_using_Ik_IE(2)) then
!   qp_msg(i_msg:i_msg)="W"
!   i_msg=i_msg+1
! endif
! if (do_EWZ_using_Ik_IE(3)) then
!   qp_msg(i_msg:i_msg)="Z"
!   i_msg=i_msg+1
! endif
! if (GFs_From_DB) then
!   qp_msg(i_msg:i_msg)="G"
!   i_msg=i_msg+1
! endif
 !
! qp_msg=trim(qp_msg)//'<'//trim(QP_db_name(i_db))//'['
! i_msg=i_msg+2+len_trim(QP_db_name(i_db))
 !
! do i1=1,qp%n_descs
!   do if=1,n_fields
!     if (index(qp%description(i1),trim(field(if,1)))/=0) then
!       c_pos=index(qp%description(i1),":")+1
!       qp_msg=trim(qp_msg)//' '//trim(field(if,2))//':'
!       call CHappend(qp%description(i1)(c_pos:))
!     endif
!   enddo
!   if (index(qp%description(i1),"PPA")/=0) qp_msg=trim(qp_msg)//' PPA '
! enddo
! !
! qp_msg=trim(qp_msg)//']'
!
! do i_sp=1,n_sp_pol
!   QP_ctl_from_DB(i_sp)%short_descr=trim(QP_ctl_from_DB(i_sp)%short_descr)//trim(qp_msg)
!   call msg(msg_fmt,trim(QP_ctl_from_DB(i_sp)%short_descr))
! enddo
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
    qp_msg=trim(qp_msg)//c(ic_start:ic_end)
  end subroutine
  !
end subroutine
     !
