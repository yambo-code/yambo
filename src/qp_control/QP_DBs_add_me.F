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
subroutine QP_DBs_add_me(qp_base,qp_to_add,plan_EWZ,SAME_k,SAME_desc,QUIET)
 !
 ! This routine just adds to qp_base qp with the plan plan_EWZ
 !
 use pars,           ONLY:cZERO,rZERO,cI
 use QP_m,           ONLY:QP_t,QP_reset,QP_copy,QP_alloc
 use electrons,      ONLY:n_sp_pol
 use com,            ONLY:error,msg
 use LIVE_t,         ONLY:what_is_running
 use vec_operate,    ONLY:v_is_zero
 !
#include<memory.h>
 !
 integer            ::n_GROUNDS
 type(QP_t)         ::qp_base,qp_to_add
 logical            ::plan_EWZ(3),SAME_k,SAME_desc,QUIET
 !
 ! Work Space

 logical    ::l_QP_db,SAME_calculation,SAME_k_grid,l_ik_found
 integer    ::i_sp,n_states_merged,ik,ik_p
 !
 integer    ::N_states,i_s,i_d
 type(QP_t) ::qp
 integer, allocatable :: dummy_table(:,:)
 !
 ! Check if there is any new state to fill
 !
 YAMBO_ALLOC(dummy_table,(qp_base%n_states+qp_to_add%n_states,3+n_sp_pol-1))
 dummy_table=0
 dummy_table(:qp_base%n_states,:)=qp_base%table
 N_states=qp_base%n_states
 I_S_LOOP: do i_s=1,qp_to_add%n_states
   do i_sp=1,qp_base%n_states
     if (all(qp_to_add%table(i_s,:)==qp_base%table(i_sp,:))) cycle I_S_LOOP
   enddo
   N_states=N_states+1
   dummy_table(N_states,:)=qp_to_add%table(i_s,:)
 enddo I_S_LOOP
 write (*,*) "K",SAME_k,"D",SAME_desc
 write (*,*) N_states,qp_base%n_states,qp_to_add%n_states
 !
 if (SAME_desc) then
   !
   if (N_states==qp_base%n_states) then
     write (*,*) "Nothing to do"
     return
   endif
   !
 endif
 !
 ! Add new descs/k-points
 !
 do i_d=1,qp_to_add%n_descs
   if (SAME_k   .and.index(qp_to_add%description(i_d),"QP @ K") >  0) cycle
   if (SAME_desc.and.index(qp_to_add%description(i_d),"QP @ K") == 0) cycle
   qp_base%n_descs=qp_base%n_descs+1
   qp_base%description(qp_base%n_descs)=qp_to_add%description(i_d)
   write (*,*) trim(qp_to_add%description(i_d))
 enddo
 !
 ! Prepare the new QP type
 !
 call QP_reset(qp)
 !
 qp%nb=maxval(dummy_table(:,1))
 qp%nk=maxval(dummy_table(:,3))
 qp%n_states=N_states
 qp%n_descs=qp_base%n_descs
 qp%description=qp_base%description
 qp%GreenF_n_steps=qp_base%GreenF_n_steps
 !
 ! Is a QP/GF database?
 !
 l_QP_db=allocated(qp_base%E)
 !
 ! Allocation
 !
 call QP_alloc(qp)
 !
 ! Zeroing
 !
 qp%k =rZERO
 qp%wk=rZERO
 if (l_QP_db) then
   qp%Z=cZERO
   qp%E=cZERO
   qp%E_bare=rZERO
 endif
 !
 ! Copying from qp_base
 !
 qp%table=dummy_table(:qp%n_states,:)
 qp%k(:qp_base%nk,:)=qp_base%k
 qp%wk(:qp_base%nk)=qp_base%wk
 if (l_QP_db) then
   qp%Z(:qp_base%n_states)=qp_base%Z
   qp%E(:qp_base%n_states)=qp_base%E
   qp%E_bare(:qp_base%n_states)=qp_base%E_bare
 endif
 !
 ! Adding/Merging 
 !
 do i_s=1,qp%n_states
   do i_sp=1,qp_to_add%n_states
     if (all(qp_to_add%table(i_sp,:)==qp%table(i_s,:))) then
       if (l_QP_db) then
         if (plan_EWZ(1)) then
           qp%E(i_s)=qp%E(i_s)+real(qp_to_add%E(i_sp))-qp_to_add%E_bare(i_sp)
         endif
         qp%E_bare(i_s)=qp_to_add%E_bare(i_sp)
         if (plan_EWZ(2)) qp%E(i_s)=qp%E(i_s)+cI*aimag(qp_to_add%E(i_sp))
         if (plan_EWZ(3)) qp%Z(i_s)=qp%Z(i_s)+qp_to_add%Z(i_sp)
       else
         qp%S_total(i_s,:)=qp_to_add%S_total(i_sp,:)
         qp%GreenF(i_s,:)=qp_to_add%GreenF(i_sp,:)
         qp%GreenF_W(i_s,:)=qp_to_add%GreenF_W(i_sp,:)
       endif
       qp%k(qp%table(i_s,3),:)=qp_to_add%k( qp_to_add%table(i_sp,3),:)
       qp%wk(qp%table(i_s,3) )=qp_to_add%wk( qp_to_add%table(i_sp,3) )
     endif
   enddo
 enddo
 !
 ! Finalization
 !
 call QP_reset(qp_base)
 call QP_copy(qp,qp_base)
 call QP_reset(qp)
 write (*,*) qp_base%n_states,shape(qp_base%table)
 !
 YAMBO_FREE(dummy_table)
 !
end subroutine
