!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QP_DBs_manipulate( E )
 !
 use pars,          ONLY:schlen,msg_len
 use YPPm,          ONLY:QP_db_operation,QP_db_prefactor,n_QP_dbs,QP_db,QP_db_what,QP_db_name,QP_db_input_control
 use stderr,        ONLY:intc,real2ch
 use com,           ONLY:msg
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_WR_CL,REP,DUMP,NONE,OP_RD_CL
 use QP_m,          ONLY:QP_t,QP_reset,QP_solver,QP_table,QP_n_states
 use interfaces,    ONLY:QP_DBs_merge,DESC_write
 use electrons,     ONLY:levels,n_sp_pol
 !
#include<memory.h>
 !
 type(levels)     ::E
 !
 ! Work Space
 !
 integer                 :: i_qp,i_db,i_d,i_err,ID,n_GROUNDS,i_c,i_loop
 integer,    external    :: io_QP_and_GF
 character(schlen)       :: QP_merged_name,QP_db_kind,ACTION,kind_to_add
 logical,           allocatable :: plan_EWZG(:,:)
 type(QP_t),        allocatable :: qp(:)
 !
 ! Setup
 !--------
 do i_c=1,2
   n_QP_dbs=0
   do i_qp=1,size(QP_db_input_control,1)
     if (len_trim(QP_db_input_control(i_qp,1))==0) exit
     n_QP_dbs=n_QP_dbs+1
     if (i_c==2) then
       call QP_reset(QP_db(n_QP_dbs))
       QP_db_what(n_QP_dbs)=QP_db_input_control(i_qp,1)
       QP_db_operation(n_QP_dbs)=QP_db_input_control(i_qp,2)
       read (QP_db_input_control(i_qp,3),*) QP_db_prefactor(n_QP_dbs)
       QP_db_name(n_QP_dbs)=QP_db_input_control(i_qp,4)
       call io_control(ACTION=OP_RD_CL,SEC=(/1,2,3/),COM=NONE,MODE=DUMP,ID=ID)
       i_err=io_QP_and_GF(trim(QP_db_input_control(i_qp,4)),QP_db(n_QP_dbs),ID)
       if (i_err/=0) then
         call warning(" Error in reading QP DB "//trim(QP_db_input_control(i_qp,4))//" not found")
         call QP_reset(QP_db(n_QP_dbs))
         n_QP_dbs=n_QP_dbs-1
       endif
     endif
   enddo
   if (i_c==1) then
     allocate(QP_db_what(n_QP_dbs))
     allocate(QP_db(n_QP_dbs))
     allocate(QP_db_name(n_QP_dbs))
     allocate(QP_db_operation(n_QP_dbs))
     allocate(QP_db_prefactor(n_QP_dbs))
     QP_db_operation="+"
     QP_db_prefactor=1.
   endif
   !
 enddo
 !
 if (n_QP_dbs==0) return
 !
 YAMBO_ALLOC(plan_EWZG,(n_QP_dbs,4))
 plan_EWZG=.FALSE.
 !
 call section("=",'QP databases mergering')
 !=========================================
 do i_db=1,n_QP_dbs
   !
   ACTION=trim(QP_db_operation(i_db))//trim(real2ch(QP_db_prefactor(i_db)))//" x "//trim(QP_db_what(i_db))
   !
   call msg('s',repeat("-",msg_len))
   call msg('s','[DB #'//trim(intc(i_db))//']'//trim(ACTION)//" < "//trim(QP_db_name(i_db)))
   call msg('s',repeat("-",msg_len))
   !
   if (index(QP_db_what(i_db),"E")>0) plan_EWZG(i_db,1)=.TRUE.
   if (index(QP_db_what(i_db),"W")>0) plan_EWZG(i_db,2)=.TRUE.
   if (index(QP_db_what(i_db),"Z")>0) plan_EWZG(i_db,3)=.TRUE.
   if (index(QP_db_what(i_db),"G")>0) plan_EWZG(i_db,4)=.TRUE.
   !
   call DESC_write('s','[DB #'//trim(intc(i_db))//']',QP_db(i_db)%desc)
   !
   call msg('s',repeat("-",msg_len))
   !
 enddo
 !
 call msg('s','Merging...')
 !==========================
 n_GROUNDS=-1
 do i_loop=1,2
   if (i_loop==2) then
     allocate(qp(n_GROUNDS))
     do i_qp=1,n_GROUNDS
       call QP_reset(qp(i_qp))
     enddo
   endif
   call QP_DBs_merge(n_QP_dbs,QP_db,plan_EWZG,n_GROUNDS,&
&                    qp=qp,OP=QP_db_operation,PRE_FAC=QP_db_prefactor,FILES=QP_db_input_control(:,4))
 enddo
 call msg('l','done')
 !
 call msg('s','I/O...')
 !=====================
 do i_qp=1,n_GROUNDS
   !
   QP_db_kind=" "
   do i_d=1,qp(i_qp)%desc%n
     kind_to_add=" "
     if (index(qp(i_qp)%desc%sval(i_d),"Real")>0)    kind_to_add="_gw_real_axis"
     if (index(qp(i_qp)%desc%sval(i_d),"Fan")>0)     kind_to_add="_el_ph"
     if (index(qp(i_qp)%desc%sval(i_d),"Lamb")>0)    kind_to_add="_el_photon"
     if (index(qp(i_qp)%desc%sval(i_d),"PPA")>0)     kind_to_add="_gw_ppa"
     if (index(qp(i_qp)%desc%sval(i_d),"COHSEX")>0)  kind_to_add="_gw_cohsex"
     if (len_trim(kind_to_add)==0) cycle
     if (index(QP_db_kind,trim(kind_to_add))==0)     QP_db_kind=trim(QP_db_kind)//trim(kind_to_add)
   enddo
   !
   QP_merged_name='QP_merged_'//trim(intc(i_qp))//trim(QP_db_kind)
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
   i_err=io_QP_and_GF(trim(QP_merged_name),qp(i_qp),ID)
   !
   QP_n_states=qp(i_qp)%n_states
   YAMBO_FREE(QP_table)
   YAMBO_ALLOC(QP_table,(QP_n_states,3+n_sp_pol-1))
   QP_table=qp(i_qp)%table
   !
   QP_solver="mixed"
   if (allocated(qp(i_qp)%GreenF)) QP_solver="g"
   call QP_of(qp(i_qp),E,0,"qp"//trim(QP_db_kind))
   !
 enddo
 call msg('l','done')
 !
 deallocate(qp)
 YAMBO_FREE(plan_EWZG)
 !
end subroutine
