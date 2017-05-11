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
subroutine QP_merge_DBs(n_dbs,qp2merge,plan_EWZ,n_GROUNDS,qp,OP,PRE_FAC)
 !
 ! This routine merges DBs that respect the two following statements:
 !
 ! 1. They are different (e-e vs e-p) but done on the same grid of k-pts
 ! 2. They are relative to the SAME calculation but on different subsets of k-pts
 !
 use pars,           ONLY:SP,cI
 use QP_m,           ONLY:QP_t,QP_copy
 use com,            ONLY:msg
 use stderr,         ONLY:intc
 use LIVE_t,         ONLY:what_is_running
 use vec_operate,    ONLY:v_is_zero
 !
#include<memory.h>
 !
 integer              :: n_dbs,n_GROUNDS
 type(QP_t)           :: qp2merge(n_dbs)
 logical              :: plan_EWZ(n_dbs,3)
 type(QP_t),  optional:: qp(n_GROUNDS)
 character(*),optional:: OP(n_dbs)
 real(SP)    ,optional:: PRE_FAC(n_dbs)
 !
 ! Work Space
 !
 integer      :: i_db,i_db_p,GROUND(n_dbs)
 logical      :: MERGED(n_dbs),SAME_k,SAME_desc,just_COUNT,CONDITION
 character(1) :: WHAT
 !
 ! Affinity check
 !================
 !
 just_COUNT=n_GROUNDS < 0
 MERGED    =.FALSE.
 GROUND    =0
 n_GROUNDS =0
 !
 do i_db=1,n_dbs
   !
   if (MERGED(i_db  )) cycle
   !
   n_GROUNDS=n_GROUNDS+1
   GROUND(i_db)=n_GROUNDS
   if (.not.just_COUNT) then
     if (plan_EWZ(i_db,1)) WHAT="E"
     if (plan_EWZ(i_db,2)) WHAT="W"
     if (plan_EWZ(i_db,3)) WHAT="Z"
     call QP_copy(qp2merge(i_db), qp( n_GROUNDS ), WHAT, trim(OP(i_db)),PRE_FAC(i_db) )
     if (WHAT/="E") qp( n_GROUNDS )%E=qp2merge(i_db)%E_bare+cI*aimag( qp( n_GROUNDS )%E )
   endif
   !
   do i_db_p=i_db+1,n_dbs
     !
     if (MERGED(i_db_p)) cycle
     !
     SAME_k   =have_same_k(qp2merge(i_db),qp2merge(i_db_p))
     SAME_desc=have_same_desc(qp2merge(i_db),qp2merge(i_db_p))
     !
     ! Merging scheme:
     !
     !            SAME_k => Merge if different plan
     ! SAME_desc-< 
     !            .not.SAME_k => add only different kpts
     !
     !                 SAME_k => add ALL
     ! .not.SAME_desc <
     !                 .not.SAME_k => No merge
     !
     !
     CONDITION= (SAME_desc.and..not.SAME_k) .or. (.not.SAME_desc.and.SAME_k) .or. &
&               (SAME_desc.and.SAME_k.and..not.all(plan_EWZ(i_db,:).eqv.plan_EWZ(i_db_p,:)))
     !
     if (CONDITION) then
       MERGED(i_db_p)=.TRUE.
       GROUND(i_db_p)=GROUND(i_db)
       if (.not.just_COUNT) then
         call QP_DBs_add_me(qp( GROUND(i_db_p) ),qp2merge(i_db_p) ,plan_EWZ(i_db_p,:), &
&                           SAME_k, SAME_desc,trim(OP(i_db_p)),PRE_FAC(i_db_p))
       endif
     endif
   enddo
   MERGED(i_db)=.TRUE.
 enddo
 !
 if (index(what_is_running,"YPP")>0.and.just_COUNT) then
   !
   call msg('l','detected '//trim(intc(n_GROUNDS))//' family(ies) of QP databases...')
   !
 endif
 !
 contains 
   !
   logical function have_same_k(qp1,qp2)
     type(QP_t)::qp1,qp2
     integer ik,ik_p
     logical l_ik_found
     have_same_k=qp1%nk==qp2%nk
     do ik=1,qp1%nk
       l_ik_found=.FALSE.
       do ik_p=1,qp2%nk
         if (v_is_zero(qp1%k(ik,:)-qp2%k(ik_p,:))) l_ik_found=.TRUE.
       enddo
       if (.not.l_ik_found) have_same_k=.FALSE.
     enddo
   end function
   !
   logical function have_same_desc(qp1,qp2)
     type(QP_t)::qp1,qp2
     integer i_d
     have_same_desc=.TRUE.
     if (qp1%n_descs/=qp2%n_descs) have_same_desc=.FALSE.
     do i_d=1,qp1%n_descs
       if (index(qp2%description(i_d),"QP @ K")==0) then
         if ( trim(qp2%description(i_d)) /= trim(qp1%description(i_d)) )  have_same_desc=.FALSE.
       endif
     enddo
   end function
   !
end subroutine
