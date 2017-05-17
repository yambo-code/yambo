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
 use pars,          ONLY:rZERO,schlen,rONE
 use electrons,     ONLY:levels,n_sp_pol
 use parser_m,      ONLY:parser
 use QP_CTL_m,      ONLY:QP_ctl_Wc,QP_ctl_Wv,QP_ctl_E,QP_action,n_QP_actions,QP_ctl_what,&
&                        QP_ctl_interp_neigh,QP_ctl_db,QP_ctl_ID,QP_CTL_fill
 use stderr,        ONLY:string_split
 !
#include<memory.h>
 !
 type (levels) ::E
 character(1)  ::object
 !
 ! Work Space
 !
 character(schlen)::spin_ch(2)
 logical          ::E_def(3),W_def(2),HAVE_QP_dbs
 integer          ::i_spin,i_p,i2
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
 call parser(trim(QP_ctl_what)//'fnQP_E',E_def(1))
 call parser(trim(QP_ctl_what)//'fnQP_up_E',E_def(2))
 call parser(trim(QP_ctl_what)//'fnQP_dn_E',E_def(3))
 call parser(trim(QP_ctl_what)//'fnQPdb',HAVE_QP_dbs)
 call parser(trim(QP_ctl_what)//'fnQP_Wv',W_def(1))
 call parser(trim(QP_ctl_what)//'fnQP_Wc',W_def(2))
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
         call FLAG_the_logical(QP_action(n_QP_actions)%plan(6),"dos")
         call FLAG_the_logical(QP_action(n_QP_actions)%plan(6),"DOS")
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
 if (.not.any((/E_def,W_def/))) return
 !
 do i_spin=1,n_sp_pol
   !
   if (i_spin==1) n_QP_actions=n_QP_actions+1
   !
   if (any(E_def)) then
     if (QP_ctl_E(QP_ctl_ID,1,i_spin)==rZERO.and.all((/QP_ctl_E(QP_ctl_ID,2:3,i_spin)==rONE/))) cycle
   endif
   if (any(W_def)) then
     if (all((/QP_ctl_Wc(QP_ctl_ID,:,i_spin)==rZERO/)).and.all((/QP_ctl_Wv(QP_ctl_ID,:,i_spin)==rZERO/))) cycle
   endif
   !
   spin_ch=(/'',''/)
   if (n_sp_pol==2) spin_ch=(/'  (spin up)','(spin down)'/)
   QP_action(n_QP_actions)%what="User defined "//trim(QP_action(n_QP_actions)%what)//trim(spin_ch(i_spin))
   !
   ! Now I move to the input file provided stretching and generalized QP parameters. 
   ! Those are contained in the QP_ctl* fields.
   !
   call QP_CTL_fill(n_QP_actions,i_spin,QP_ctl_ID,+E%E_CBm,E%E_VBM)
   !
 enddo
 !
 contains
   !
   subroutine FLAG_the_logical(FLAG,FIELD)
     logical      :: FLAG
     character(*) :: FIELD
     if (FLAG) return
     FLAG=trim(db_piece(i2))==FIELD
   end subroutine
   !
end subroutine
