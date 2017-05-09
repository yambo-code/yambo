!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine QP_DBs_manipulate( )
 !
 use YPP,           ONLY:n_QP_dbs,QP_db,QP_db_what,QP_db_name
#if defined _YPP_RT
 use YPP_real_time, ONLY:Z_input
#endif
 use stderr,        ONLY:intc
 use com,           ONLY:msg
 use IO_m,          ONLY:io_control,OP_WR_CL,REP
 use QP_m,          ONLY:QP_t,QP_reset
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer                 :: i_qp,i_d,i_err,ID,n_GROUNDS
 integer,    external    :: io_QP_and_GF
 logical,    allocatable :: plan_EWZ(:,:)
 type(QP_t), allocatable :: qp(:)
 !
 ! Resets
 !--------
 call QP_DBs_initialize()
 !
 YAMBO_ALLOC(plan_EWZ,(n_QP_dbs,3))
 plan_EWZ=.FALSE.
 !
 call section("=",'QP databases mergering')
 !=========================================
 !
 do i_qp=1,n_QP_dbs
   !
   call msg('s',repeat("-",len_trim(QP_db_name(i_qp))+13))
   call msg('s','[DB #'//trim(intc(i_qp))//']'//trim(QP_db_what(i_qp))//" < "//trim(QP_db_name(i_qp)))
   call msg('s',repeat("-",len_trim(QP_db_name(i_qp))+13))
   !
   if (index(QP_db_what(i_qp),"E")>0) plan_EWZ(i_qp,1)=.TRUE.
   if (index(QP_db_what(i_qp),"W")>0) plan_EWZ(i_qp,2)=.TRUE.
   if (index(QP_db_what(i_qp),"Z")>0) plan_EWZ(i_qp,3)=.TRUE.
   !
   do i_d=1,QP_db(i_qp)%n_descs
     call msg('s','[DB #'//trim(intc(i_qp))//']'//trim(QP_db(i_qp)%description(i_d)))
   enddo
   !
   if (i_qp==n_QP_dbs) call msg('s',repeat("-",len_trim(QP_db_name(i_qp))+13))
   !
 enddo
 !
 call msg('s','Merging...')
 !==========================
 !
 allocate(qp(1))
 n_GROUNDS=-1
 call QP_merge_DBs(n_QP_dbs,QP_db,plan_EWZ,n_GROUNDS,qp)
 deallocate(qp)
 allocate(qp(n_GROUNDS))
 do i_qp=1,n_GROUNDS
   call QP_reset(qp(i_qp))
 enddo
 call QP_merge_DBs(n_QP_dbs,QP_db,plan_EWZ,n_GROUNDS,qp)
 deallocate(qp)
 !
 call msg('l','done')
 stop
 !
!#if defined _RT
! !
! call msg('s','Post-Processing...')
! !==================================
! !
! do i_sp=1,qp%n_states
!   !
!   COHSEX_eq= real(qp%E(i_sp)-qp%E_bare(i_sp))
!   dE_Neq   = real(qp%E(i_sp)-qp%E_bare(i_sp))
!   !
!   if (index(trim(QP_db_what(i_qp)),"C")==0) COHSEX_eq=0._SP
!   if (index(trim(QP_db_what(i_qp)),"N")==0) dE_Neq   =0._SP
!   if (Z_input<=0._SP) then
!     if(index(trim(QP_db_what(i_qp)),"Z")==0) then
!     Z_local=real(QP_db(i_qp_ref)%E(i_sp)-QP_db(i_qp_ref)%E_bare(i_sp))/ &
!  &          real(QP_db(1)%E(i_sp)-QP_db(1)%E_bare(i_sp))
!   else
!     Z_local=QP_db(i_qp_ref)%Z(i_sp)
!   endif
! else
!           Z_local=Z_input
!         endif
!         QP_db(i_qp_ref)%E(i_s)=QP_db(i_qp_ref)%E(i_s)+Z_local*(dE_Neq-COHSEX_eq)
!         !
!#endif
 !
 call msg('s','I/O...')
 !=====================
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
 i_err=io_QP_and_GF('QP',qp,ID)
 call msg('l','done')
 !
end subroutine
