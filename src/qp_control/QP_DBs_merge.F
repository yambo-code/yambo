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
subroutine QP_DBs_merge(n_dbs,qp2merge,plan_EWZG,n_GROUNDS,qp,OP,PRE_FAC,MAP)
 !
 use pars,           ONLY:SP
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
 logical              :: plan_EWZG(n_dbs,4)
 type(QP_t),  optional:: qp(n_GROUNDS)
 character(*),optional:: OP(n_dbs)
 real(SP)    ,optional:: PRE_FAC(n_dbs)
 integer     ,optional:: MAP(n_dbs)
 !
 ! Work Space
 !
 integer      :: i_db,i_db_p,GROUND(n_dbs),i_d
 logical      :: MERGED(n_dbs),SAME_states,SAME_desc,just_COUNT,CONDITION
 character(4) :: WHAT
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
   if (qp2merge(i_db)%n_states==0) cycle
   !
   if (MERGED(i_db  )) cycle
   !
   n_GROUNDS=n_GROUNDS+1
   GROUND(i_db)=n_GROUNDS
   if (present(MAP)) then
     MAP(i_db)=n_GROUNDS
   endif
   if (.not.just_COUNT) then
     WHAT=" "
     if (plan_EWZG(i_db,1)) WHAT=trim(WHAT)//"E"
     if (plan_EWZG(i_db,2)) WHAT=trim(WHAT)//"W"
     if (plan_EWZG(i_db,3)) WHAT=trim(WHAT)//"Z"
     if (plan_EWZG(i_db,4)) WHAT=trim(WHAT)//"G"
     !
     call QP_copy(qp2merge(i_db), qp( n_GROUNDS ), WHAT, trim(OP(i_db)),PRE_FAC(i_db) )
     !
     qp( n_GROUNDS )%n_descs=qp2merge(i_db)%n_descs+1
     qp( n_GROUNDS )%description( 1 )="--- DB#"//trim(intc(i_db))//" descriptions ---"
     do i_d=1,qp2merge(i_db)%n_descs
       qp( n_GROUNDS )%description( i_d+1 )= qp2merge(i_db)%description(i_d)
     enddo
     do i_d=1,qp( n_GROUNDS )%n_descs
     enddo
     !
   endif
   !
   do i_db_p=i_db+1,n_dbs
     !
     if (MERGED(i_db_p)) cycle
     !
     SAME_states=(have_same_k(qp2merge(i_db),qp2merge(i_db_p)).and.have_same_states(qp2merge(i_db),qp2merge(i_db_p)))
     SAME_desc  =have_same_desc(qp2merge(i_db),qp2merge(i_db_p))
     !
     ! Merging scheme:
     !
     !            SAME_states => Merge if different plan
     ! SAME_desc-< 
     !            .not.SAME_states => add only different states
     !
     !                 SAME_states => add ALL
     ! .not.SAME_desc <
     !                 .not.SAME_states => No merge
     !
     CONDITION= (SAME_desc.and..not.SAME_states) .or. (.not.SAME_desc.and.SAME_states)
     if (SAME_desc.and.SAME_states) then
       CONDITION=.FALSE.
       if (.not.all(plan_EWZG(i_db,:).eqv.plan_EWZG(i_db_p,:)).or.OP(i_db)/=OP(i_db_p).or.&
&          PRE_FAC(i_db)/=PRE_FAC(i_db_p)) CONDITION=.TRUE.
     endif
     !
     if (plan_EWZG(i_db,4)) then
       CONDITION=qp2merge(i_db)%GreenF_n_steps==qp2merge(i_db_p)%GreenF_n_steps
     endif
     !
     if (CONDITION) then
       if (present(MAP)) then
         MAP(i_db_p)=GROUND(i_db)
       endif
       MERGED(i_db_p)=.TRUE.
       GROUND(i_db_p)=GROUND(i_db)
       if (.not.just_COUNT) then
         !
         if (.not.SAME_desc) then
           qp( GROUND(i_db_p) )%n_descs=qp( GROUND(i_db_p) )%n_descs+1
           qp( GROUND(i_db_p) )%description( qp( GROUND(i_db_p) )%n_descs )="--- DB#"//trim(intc(i_db_p))//" descriptions ---"
         endif
         !
         call QP_DBs_add_me(qp( GROUND(i_db_p) ),qp2merge(i_db_p) ,plan_EWZG(i_db_p,:), &
&                           SAME_states, SAME_desc,trim(OP(i_db_p)),PRE_FAC(i_db_p))
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
   logical function have_same_states(qp1,qp2)
     type(QP_t)::qp1,qp2
     integer iqp1,iqp2,ib1,ib2,ik1,ik2
     logical l_state_found
     do iqp1=1,qp1%n_states
       l_state_found=.FALSE.
       ib1=qp1%table(iqp1,1)
       ik1=qp1%table(iqp1,3)
       do iqp2=1,qp2%n_states
         ib2=qp2%table(iqp2,1)
         ik2=qp2%table(iqp2,3)
         if ( (ib1==ib2).and.(ik1==ik2) ) l_state_found=.TRUE.
       enddo
       if (.not.l_state_found) have_same_states=.FALSE.
     enddo
     !
     have_same_states=l_state_found
     !
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
