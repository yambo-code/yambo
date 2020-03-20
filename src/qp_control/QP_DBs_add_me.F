!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine QP_DBs_add_me(qp_base,qp_to_add,plan_EWZG,SAME_states,SAME_desc,OPERATION,PRE_FAC)
 !
 ! This routine just adds to qp_base qp with the plan plan_EWZG
 !
 use pars,           ONLY:cZERO,rZERO,cI,SP,rONE
 use QP_m,           ONLY:QP_t,QP_reset,QP_copy,QP_alloc
 use electrons,      ONLY:n_sp_pol
 !
#include<memory.h>
 !
 type(QP_t)         ::qp_base,qp_to_add
 logical            ::plan_EWZG(4),SAME_states,SAME_desc
 character(*)       ::OPERATION
 real(SP)           ::PRE_FAC
 !
 ! Work Space

 logical    ::l_QP_db,CONDITION
 integer    ::N_states,i_s,i_d,i_sp
 type(QP_t) ::qp
 real(SP)   ::PRE_SIGN
 complex(SP)::dE
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
 if (.not.SAME_desc)   call ADD_descs(.FALSE.,qp_to_add)
 call ADD_descs(.TRUE.,qp_base)
 if (.not.SAME_states) call ADD_descs(.TRUE.,qp_to_add)
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
 qp%Z=rONE
 qp%E=cZERO
 qp%E_bare=rZERO
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
 else
   qp%S_total(:qp_base%n_states,:)=qp_base%S_total
   qp%GreenF(:qp_base%n_states,:)=qp_base%GreenF
   qp%GreenF_W(:qp_base%n_states,:)=qp_base%GreenF_W
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
         !
         if (i_s>qp_base%n_states) then
           qp%E_bare(i_s)=qp_to_add%E_bare(i_sp)
           qp%E(i_s)=qp_to_add%E_bare(i_sp)
         endif
         !
         if (plan_EWZG(1)) then
           dE=qp_to_add%E(i_sp)-qp_to_add%E_bare(i_sp)
           qp%E(i_s)=qp%E(i_s)+PRE_SIGN*PRE_FAC*real(dE)
         endif
         if (plan_EWZG(2)) qp%E(i_s)=qp%E(i_s)+PRE_SIGN*PRE_FAC*cI*aimag(qp_to_add%E(i_sp))
         if (plan_EWZG(3)) then
           if (OPERATION=="x") then
             dE=qp_to_add%E(i_sp)-qp_to_add%E_bare(i_sp)
             qp%E(i_s)=qp%E(i_s)-dE+qp_to_add%Z(i_sp)*dE
           endif
           qp%Z(i_s)=1.+(qp%Z(i_s)-1.)+(qp_to_add%Z(i_sp)-1.)
         endif
       else
         if (plan_EWZG(4)) then
           if (i_s>qp_base%n_states) then
             CONDITION=.TRUE.
           else
             CONDITION=abs(qp%GreenF_W(i_s,qp%GreenF_n_steps)-qp_to_add%GreenF_W(i_sp,qp%GreenF_n_steps))<1.E-5.and.&
&                      abs(qp%GreenF_W(i_s,1)-qp_to_add%GreenF_W(i_sp,1))<1.E-5
           endif
           if (CONDITION) then
             qp%S_total(i_s,:)=qp%S_total(i_s,:)+qp_to_add%S_total(i_sp,:)
             qp%GreenF(i_s,:)=qp%GreenF(i_s,:)+qp_to_add%GreenF(i_sp,:)
             qp%GreenF_W(i_s,:)=qp_to_add%GreenF_W(i_sp,:)
           endif
         endif
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
 call QP_copy(qp,qp_base,"ALL","+",1._SP)
 call QP_reset(qp)
 !
 YAMBO_FREE(dummy_table)
 !
 contains 
   !
   subroutine ADD_descs(STATES,qp_in)
     use IO_m,  ONLY:IO_desc_element_duplicate
     logical    :: STATES
     type(QP_t) :: qp_in
     do i_d=1,qp_in%desc%n
       if (     STATES.and.index(qp_in%desc%str(i_d),"QP @ state") == 0) cycle
       if (.not.STATES.and.index(qp_in%desc%str(i_d),"QP @ state") >  0) cycle
       if (len_trim(qp_in%desc%str(i_d)) == 0) cycle
       qp%desc%n=qp%desc%n+1
       call IO_desc_element_duplicate(qp_in%desc,qp%desc,i_d,qp%desc%n)
     enddo
   end subroutine
end subroutine
