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
subroutine QP_apply_dump_user_CTLs( E, object )
 !
 use pars,          ONLY:schlen
 use electrons,     ONLY:levels,n_sp_pol
 use parser_m,      ONLY:parser
 use QP_CTL_m,      ONLY:QP_action,n_QP_actions,QP_ctl_what,QP_ctl_user,&
&                        QP_ctl_interp_neigh,QP_ctl_db,QP_ctl_ID,QP_CTL_is_active,QP_CTL_fill
 use stderr,        ONLY:string_split
 !
#include<memory.h>
 !
 type (levels) ::E
 character(1)  ::object
 !
 ! Work Space
 !
 logical          ::HAVE_QP_dbs
 integer          ::i_p,i2
 integer,parameter:: n_pieces=30
 character(schlen):: db_piece(n_pieces)
 !
 if (object=="X") QP_ctl_ID=1
 if (object=="K") QP_ctl_ID=2
 if (object=="G") QP_ctl_ID=3
 !
 if (QP_ctl_ID==1) QP_ctl_what='X'
 if (QP_ctl_ID==2) QP_ctl_what='K'
 if (QP_ctl_ID==3) QP_ctl_what='G'
 !
 call parser(trim(QP_ctl_what)//'fnQPdb',HAVE_QP_dbs)
 !
 ! User defined QP database
 !
 if (HAVE_QP_dbs) then
   call string_split(QP_ctl_db(QP_ctl_ID),db_piece)
   do i_p=1,n_pieces
     if (trim(db_piece(i_p))=="<") then
       n_QP_actions=n_QP_actions+1
       QP_action(n_QP_actions)%active=.TRUE.
       QP_action(n_QP_actions)%CTL%db=db_piece(i_p+1)
       QP_action(n_QP_actions)%CTL%interp_neigh=QP_ctl_interp_neigh(QP_ctl_ID)
       do i2=max(i_p-3,1),i_p-1
         call FLAG_the_logical(QP_action(n_QP_actions)%plan(1),"E")
         call FLAG_the_logical(QP_action(n_QP_actions)%plan(2),"W")
         call FLAG_the_logical(QP_action(n_QP_actions)%plan(3),"Z")
       enddo
       do i2=i_p,min(i_p+2,n_pieces)
         call FLAG_the_logical(QP_action(n_QP_actions)%plan(4),"ui")
         call FLAG_the_logical(QP_action(n_QP_actions)%plan(4),"uki")
         call FLAG_the_logical(QP_action(n_QP_actions)%plan(4),"UKI")
         call FLAG_the_logical(QP_action(n_QP_actions)%plan(5),"uei")
         call FLAG_the_logical(QP_action(n_QP_actions)%plan(5),"UEI")
       enddo
       if (.not.any((/QP_action(n_QP_actions)%plan(4:5)/))) QP_action(n_QP_actions)%plan(4)=.TRUE.
       !
       if (QP_action(n_QP_actions)%plan(1)) then
         QP_action(n_QP_actions)%what=trim(QP_action(n_QP_actions)%what)//" E"
       endif
       if (QP_action(n_QP_actions)%plan(2)) then
         QP_action(n_QP_actions)%what=trim(QP_action(n_QP_actions)%what)//" W"
       endif
       if (QP_action(n_QP_actions)%plan(3)) then
         QP_action(n_QP_actions)%what=trim(QP_action(n_QP_actions)%what)//" Z"
       endif
       !
       if (any( QP_action(n_QP_actions)%plan(1:3))) then
         QP_action(n_QP_actions)%what=trim(QP_action(n_QP_actions)%what)//" < "//trim(QP_action(n_QP_actions)%CTL(1)%db)
       else
         QP_action(n_QP_actions)%active=.FALSE.
       endif
       !
     endif
   enddo
 endif
 !
 ! User defined parameters
 !  
 if (n_sp_pol==2) then
   !
   if (QP_CTL_is_active(QP_ctl_ID,2).or.QP_CTL_is_active(QP_ctl_ID,3)) then
     !
     call USER_action_active( )
     !
     call QP_CTL_fill(n_QP_actions,1,QP_ctl_ID,2,E%E_CBm,E%E_VBM)
     call QP_CTL_fill(n_QP_actions,2,QP_ctl_ID,3,E%E_CBm,E%E_VBM)
     !
   else if (QP_CTL_is_active(QP_ctl_ID,1)) then
     !
     call USER_action_active( )
     !
     call QP_CTL_fill(n_QP_actions,1,QP_ctl_ID,1,E%E_CBm,E%E_VBM)
     call QP_CTL_fill(n_QP_actions,2,QP_ctl_ID,1,E%E_CBm,E%E_VBM)
     !
   endif
   !
 else
   !
   if (QP_CTL_is_active(QP_ctl_ID,1)) then
     !
     call USER_action_active( )
     !
     call QP_CTL_fill(n_QP_actions,1,QP_ctl_ID,1,E%E_CBm,E%E_VBM)
     !
   endif
   !
 endif
 !
 contains
   !
   subroutine USER_action_active()
     n_QP_actions=n_QP_actions+1
     QP_action(n_QP_actions)%active=.TRUE.
   end subroutine
   !
   subroutine FLAG_the_logical(FLAG,FIELD)
     logical      :: FLAG
     character(*) :: FIELD
     if (FLAG) return
     FLAG=trim(db_piece(i2))==FIELD
   end subroutine
   !
end subroutine
