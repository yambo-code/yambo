!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QP_DB_message( i_ACTION, qp )
 !
 use pars,     ONLY:lchlen
 use stderr,   ONLY:real2ch,intc
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
 integer            :: if,i_msg,i1
 character(lchlen)  :: field(n_fields,3)
 logical            :: am_i_first
 !
 field=" "
 field(1,1)="X G`s"
 field(1,2)="XG" 
 field(1,3)="used" 
 field(2,1)="X bands"
 field(2,2)="Xb"
 field(3,1)="Sc/G bands"
 field(3,2)="Scb"
 field(4,1)="Sc. G bands"
 field(4,2)="Sphb"
 field(5,1)="PPA imaginary"
 field(5,2)="PPA@E"
 field(6,1)="Fan"
 field(6,2)="E-P"
 field(7,1)="Real Axis"
 field(7,2)="E-E"
 !
 am_i_first=.TRUE.
 !
 ! Here I build the %desc char on the basis of the qp%desc lines.
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
 do i1=1,qp%desc%n
   do if=1,n_fields
     if (index(qp%desc%str(i1),trim(field(if,1)))/=0.or.index(qp%desc%sval(i1),trim(field(if,1)))/=0) then
       if (len_trim(field(if,3))>0 .and. index(qp%desc%term(i1),trim(field(if,3)))==0) cycle
       if (.not.am_i_first) then
         QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//' * '//trim(field(if,2))
       else
         QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//' '//trim(field(if,2))
       endif
       am_i_first=.FALSE.
       if(qp%desc%kind(i1)=='s') then
         QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//' '//trim(qp%desc%sval(i1))
       endif
       if(qp%desc%kind(i1)=='r') then
         if(qp%desc%size(i1)> 0) QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//' '//trim(real2ch(qp%desc%rval(1,i1)))
         if(qp%desc%size(i1)> 1) QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//'-'//trim(real2ch(qp%desc%rval(2,i1)))
       endif
       if(qp%desc%kind(i1)=='i') then
         if(qp%desc%size(i1)> 0) QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//' '//trim(intc(qp%desc%ival(1,i1)))
         if(qp%desc%size(i1)> 1) QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//'-'//trim(intc(qp%desc%ival(2,i1)))
       endif
     endif
   enddo
 enddo
 !
 QP_action(i_ACTION)%desc=trim(QP_action(i_ACTION)%desc)//']'
 !
 call msg("n"//trim(QP_ctl_msg_fmt),trim(QP_action(i_ACTION)%desc))
 ! 
end subroutine
