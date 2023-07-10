!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine QP_DBs_merge(n_dbs,qp2merge,plan_EWZG,n_GROUNDS,qp,OP,PRE_FAC,FILES)
 !
 use pars,           ONLY:SP
 use QP_m,           ONLY:QP_t,QP_copy
 use com,            ONLY:msg
 use stderr,         ONLY:intc,real2ch
 use C_driver,       ONLY:code_bin
 use vec_operate,    ONLY:v_is_zero
 use descriptors,    ONLY:IO_desc_duplicate
 !
#include<memory.h>
 !
 integer              :: n_dbs,n_GROUNDS
 type(QP_t)           :: qp2merge(n_dbs)
 logical              :: plan_EWZG(n_dbs,4)
 type(QP_t),  optional:: qp(n_GROUNDS)
 character(*),optional:: OP(n_dbs)
 real(SP)    ,optional:: PRE_FAC(n_dbs)
 character(*),optional:: FILES(n_dbs)
 !
 ! Work Space
 !
 integer      :: i_db,i_db_p,GROUND(n_dbs),i_d
 logical      :: MERGED(n_dbs),SAME_states,SAME_desc,just_COUNT,CONDITION
 character(4) :: WHAT
 !
 ! Affinity check
 !================
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
   !
   if (.not.just_COUNT) then
     call WHAT_update(i_db)
     call QP_copy(qp2merge(i_db), qp( n_GROUNDS ), WHAT, trim(OP(i_db)),PRE_FAC(i_db) )
     call ADD_the_DB_desc( N_GROUNDS, i_db,"[DB #"//trim(intc(i_db))//"]")
     if(i_db == 1) then
       call IO_desc_duplicate(qp2merge(i_db)%desc,qp( n_GROUNDS )%desc,offset=2,PREPEND="")
     else
       call IO_desc_duplicate(qp2merge(i_db)%desc,qp( n_GROUNDS )%desc,offset=2,PREPEND="[DB #"//trim(intc(i_db))//"]")
     endif
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
       MERGED(i_db_p)=.TRUE.
       GROUND(i_db_p)=GROUND(i_db)
       if (.not.just_COUNT) then
         call WHAT_update( i_db_p)
         call ADD_the_DB_desc( GROUND(i_db_p), i_db_p,"[DB #"//trim(intc(i_db_p))//"]")
         call PREAPPEND_the_DB_desc( qp2merge(i_db_p), i_db_p )
         call QP_DBs_add_me(qp( GROUND(i_db_p) ),qp2merge(i_db_p) ,plan_EWZG(i_db_p,:), &
&                           SAME_states, SAME_desc,trim(OP(i_db_p)),PRE_FAC(i_db_p))
       endif
     endif
   enddo
   MERGED(i_db)=.TRUE.
 enddo
 !
 if (index(code_bin,"ypp")>0.and.just_COUNT) then
   !
   call msg('l','detected '//trim(intc(n_GROUNDS))//' family(ies) of QP databases...')
   !
 endif
 !
 contains 
   !
   subroutine WHAT_update( j_db )
     integer :: j_db
     WHAT=" "
     if (plan_EWZG(j_db,1)) WHAT=trim(WHAT)//"E"
     if (plan_EWZG(j_db,2)) WHAT=trim(WHAT)//"W"
     if (plan_EWZG(j_db,3)) WHAT=trim(WHAT)//"Z"
     if (plan_EWZG(j_db,4)) WHAT=trim(WHAT)//"G"
   end subroutine
   !
   subroutine PREAPPEND_the_DB_desc( qp_in, current_db )
     type(QP_t)::qp_in
     integer   ::i_desc,current_db
     do i_desc=1,qp_in%desc%n
       qp_in%desc%str(i_desc)="[DB #"//trim(intc(current_db))//"] "//trim(qp_in%desc%str(i_desc))
     enddo
   end subroutine
   !
   subroutine ADD_the_DB_desc( j_db,  current_db, PREPEND )
     integer      :: j_db,current_db
     character(*) :: PREPEND
     qp( j_db )%desc%str ( qp( j_db)%desc%n+1)=PREPEND//"Database"
     qp( j_db )%desc%sval( qp( j_db)%desc%n+1)=trim(FILES(current_db))
     qp( j_db )%desc%size( qp( j_db)%desc%n+1)= 1
     qp( j_db )%desc%kind( qp( j_db)%desc%n+1)="s"
     qp( j_db )%desc%str ( qp( j_db)%desc%n+2)=PREPEND//"Database Operation"
     qp( j_db )%desc%sval( qp( j_db)%desc%n+2)=trim(OP(current_db))//trim(real2ch(PRE_FAC(current_db)))//"x"//WHAT
     qp( j_db )%desc%size( qp( j_db)%desc%n+2)= 1
     qp( j_db )%desc%kind( qp( j_db)%desc%n+2)="s"
     qp( j_db )%desc%n=qp( j_db )%desc%n+2
   end subroutine
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
     !      
     use pars,           ONLY:lchlen
     !
     type(QP_t)::qp1,qp2
     integer i_d1,i_d2,wrong_i_d
     character(lchlen) :: wrong_val
     !
     have_same_desc=.TRUE.
     !
     if (qp1%desc%n/=qp2%desc%n) call warning('Differenent number of descriptions in the QP DBs '//NEW_LINE('A'))
     !
     do i_d1=1,qp1%desc%n
       wrong_i_d=0
       if (index(qp1%desc%str(i_d1),"QP @ state")/=0) cycle  ! Skip the QP range
       do i_d2=1,qp2%desc%n
         if (index(qp2%desc%str(i_d2),"QP @ state")/=0) cycle  ! Skip the QP range
         !
         if(trim(qp1%desc%str(i_d1))==trim(qp2%desc%str(i_d2)).and. &
&           trim(qp1%desc%term(i_d1))==trim(qp2%desc%term(i_d2))) then  ! Compare all other descriptors
           !
           if(qp2%desc%kind(i_d2)/=qp1%desc%kind(i_d1)) call error('Internal error in types in DBs merging ')     
           !
           if(qp1%desc%kind(i_d1)=='s'.and.(trim(qp2%desc%sval(i_d2)) /= trim(qp1%desc%sval(i_d1))) ) then
              write(wrong_Val,*) trim(qp2%desc%sval(i_d2))//'  != '//trim(qp1%desc%sval(i_d1))
              wrong_i_d=i_d1 
           endif
           !
           if(qp1%desc%kind(i_d1)=='l'.and.(qp2%desc%lval(i_d2).neqv.qp1%desc%lval(i_d1)))   then
              write(wrong_val,*) qp2%desc%lval(i_d2),' != ',qp2%desc%lval(i_d1)
              wrong_i_d=i_d1 
           endif
           !
           if(qp1%desc%kind(i_d1)=='c'.and.(qp2%desc%cval(i_d2)/= qp1%desc%cval(i_d1)) )  then
             write(wrong_val,*) qp2%desc%cval(i_d2),' != ',qp2%desc%cval(i_d1)
             wrong_i_d=i_d1 
           endif
           !
           if(qp1%desc%kind(i_d1)=='d'.and.(qp2%desc%dval(i_d2)/= qp1%desc%dval(i_d1)) )  then
             write(wrong_val,*) qp2%desc%dval(i_d2),' != ',qp2%desc%dval(i_d1)
             wrong_i_d=i_d1 
           endif
           !
           if(qp1%desc%kind(i_d1)=='r'.or.qp1%desc%kind(i_d1)=='i') then
             if ( qp2%desc%size(i_d2)/= qp1%desc%size(i_d1) ) then
                wrong_i_d=i_d1 
                write(wrong_val,*) 'size -> ',qp2%desc%size(i_d2),' != ',qp2%desc%size(i_d1)
             endif
           endif
           !
           if(qp1%desc%kind(i_d1)=='r'.and.(any(qp2%desc%rval(:,i_d2) /= qp1%desc%rval(:,i_d1)))) then
              wrong_i_d=i_d1
              write(wrong_val,*) qp2%desc%rval(:3,i_d2),'... != ',qp2%desc%rval(:3,i_d1),'...'
           endif
           !
           if(qp1%desc%kind(i_d1)=='i'.and.(any(qp2%desc%ival(:,i_d2) /= qp1%desc%ival(:,i_d1)))) then
              wrong_i_d=i_d1
              write(wrong_val,*) qp2%desc%ival(:3,i_d2),'... != ',qp2%desc%ival(:3,i_d1),'...'
           endif
           !
           if(wrong_i_d/=0) then
             have_same_desc=.false.
             call msg('s','QP DBs with different  "'//trim(qp1%desc%str(wrong_i_d))//'"')
             call msg('s','Different values: '//wrong_val)
             call msg('s','') 
            endif
            !
         endif
       enddo
     enddo
     !
   end function
   !
end subroutine
