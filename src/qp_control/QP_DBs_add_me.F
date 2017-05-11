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
subroutine QP_DBs_add_me(qp_base,qp_to_add,plan_EWZ,SAME_k,SAME_desc,OPERATION,PRE_FAC)
 !
 ! This routine just adds to qp_base qp with the plan plan_EWZ
 !
 use pars,           ONLY:cZERO,rZERO,cI,SP
 use QP_m,           ONLY:QP_t,QP_reset,QP_copy,QP_alloc
 use electrons,      ONLY:n_sp_pol
 !
#include<memory.h>
 !
 type(QP_t)         ::qp_base,qp_to_add
 logical            ::plan_EWZ(3),SAME_k,SAME_desc
 character(*)       ::OPERATION
 real(SP)           ::PRE_FAC
 !
 ! Work Space

 logical    ::l_QP_db
 integer    ::N_states,i_s,i_d,i_sp
 type(QP_t) ::qp
 real(SP)   ::PRE_SIGN
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
 !
 ! Prepare the new QP type
 !
 call QP_reset(qp)
 !
 ! Add new descs/k-points
 !
 call ADD_descs(.FALSE.,qp_base)
 if (.not.SAME_desc) then
   call ADD_descs(.FALSE.,qp_to_add)
 endif
 call ADD_descs(.TRUE.,qp_base)
 if (.not.SAME_k) then
   call ADD_descs(.TRUE.,qp_to_add)
 endif
 !
 qp%nb=maxval(dummy_table(:,1))
 qp%nk=maxval(dummy_table(:,3))
 qp%n_states=N_states
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
 ! Adding/Merging |
 !
 PRE_SIGN=1.
 if (OPERATION=="-") PRE_SIGN=-1
 !
 do i_s=1,qp%n_states
   do i_sp=1,qp_to_add%n_states
     if (all(qp_to_add%table(i_sp,:)==qp%table(i_s,:))) then
       !
       if (l_QP_db) then
         if (plan_EWZ(1)) then
           qp%E(i_s)=qp%E(i_s)+PRE_SIGN*PRE_FAC*real(qp_to_add%E(i_sp)-qp_to_add%E_bare(i_sp))
         endif
         qp%E_bare(i_s)=qp_to_add%E_bare(i_sp)
         if (plan_EWZ(2)) qp%E(i_s)=qp%E(i_s)+PRE_SIGN*PRE_FAC*cI*aimag(qp_to_add%E(i_sp))
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
 call QP_copy(qp,qp_base,"ALL","+",1.)
 call QP_reset(qp)
 !
 YAMBO_FREE(dummy_table)
 !
 contains 
   !
   subroutine ADD_descs(KPTS,qp_in)
     logical    :: KPTS
     type(QP_t) :: qp_in
     do i_d=1,qp_in%n_descs
       if (KPTS     .and.index(qp_in%description(i_d),"QP @ K") == 0) cycle
       if (.not.KPTS.and.index(qp_in%description(i_d),"QP @ K") >  0) cycle
       if (len_trim(qp_in%description(i_d)) == 0) cycle
       qp%n_descs=qp%n_descs+1
       qp%description(qp%n_descs)=qp_in%description(i_d)
     enddo
   end subroutine
end subroutine
