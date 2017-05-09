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
subroutine QP_add_me(qp_base,qp_to_add,plan_EWZ)
 !
 ! This routine just adds to qp_base qp with the plan plan_EWZ
 !
 use pars,           ONLY:cZERO,rZERO,cI
 use QP_m,           ONLY:QP_t,QP_reset
 use electrons,      ONLY:n_sp_pol
 use com,            ONLY:error,msg
 use LIVE_t,         ONLY:what_is_running
 use vec_operate,    ONLY:v_is_zero
 !
#include<memory.h>
 !
 integer            ::n_GROUNDS
 type(QP_t)         ::qp_base,qp_to_add
 logical            ::plan_EWZ(3)
 !
 ! Work Space
 !
 logical  ::l_QP_db,SAME_calculation,SAME_k_grid,l_ik_found
 integer  ::i_db,i_desc,i_sp,n_states_merged,i_s,ik,ik_p
 type(QP_t) ::qp
 integer, allocatable :: dummy_table(:,:)
 !
 qp%n_states=qp_base%n_states+qp_to_add%n_states
 qp%n_descs =qp_base%n_descs+qp_to_add%n_descs
 !
 YAMBO_ALLOC(dummy_table,(qp%n_states,3+n_sp_pol-1))
 dummy_table=0
 dummy_table(:qp_base%n_states,:)=qp_base%table(:,:)
 qp%description(:qp_base%n_descs)=qp_base%description(:qp_base%n_descs)
 !
! do i_desc=1,qp2merge(i_db)%n_descs
!   if ( trim(qp2merge(i_db)%description(i_desc)) /= trim(qp2merge(1)%description(i_desc)) )then
!     if (index('QP @ K',qp2merge(1)%description(i_desc))==0) then
!       qp%n_descs=qp%n_descs+1
!       qp%description(qp%n_descs)=qp2merge(i_db)%description(i_desc)
!       cycle
!     endif
!     call error('Incompatible Databases. Description strings differ.')
!   endif 
! enddo
! do i_s=1,qp2merge(i_db)%n_states
!   do i_sp=1,n_states_merged
!     if (all(qp2merge(i_db)%table(i_s,:)==dummy_table(i_sp,:))) goto 1
!   enddo
!   n_states_merged=n_states_merged+1
!   dummy_table(n_states_merged,:)=qp2merge(i_db)%table(i_s,:)
!1  continue
! enddo
 !
 qp%nb=maxval(dummy_table(:,1))
 qp%nk=maxval(dummy_table(:,3))
 !qp%n_states=n_states_merged
 qp%GreenF_n_steps=qp_base%GreenF_n_steps
 !
 l_QP_db=allocated(qp_base%E)
 !
 YAMBO_ALLOC(qp%k,(qp%nk,3))
 YAMBO_ALLOC(qp%wk,(qp%nk))
 YAMBO_ALLOC(qp%table,(qp%n_states,3+n_sp_pol-1))
 !
 qp%k =rZERO
 qp%wk=rZERO
 !
 if (l_QP_db) then
   YAMBO_ALLOC(qp%Z,(qp%n_states))
   YAMBO_ALLOC(qp%E,(qp%n_states))
   YAMBO_ALLOC(qp%E_bare,(qp%n_states))
   qp%Z=cZERO
   qp%E=cZERO
   qp%E_bare=rZERO
 else
   YAMBO_ALLOC(qp%S_total,(qp%n_states,qp%GreenF_n_steps))
   YAMBO_ALLOC(qp%GreenF,(qp%n_states,qp%GreenF_n_steps))
   YAMBO_ALLOC(qp%GreenF_W,(qp%n_states,qp%GreenF_n_steps))
 endif
 !
! qp%table=dummy_table(:qp%n_states,:)
! do i_s=1,qp%n_states
!   do i_db=1,n_dbs
!     do i_sp=1,qp2merge(i_db)%n_states
!       if (all(qp2merge(i_db)%table(i_sp,:)==qp%table(i_s,:))) then
!         if (l_QP_db) then
!           if (i_db==1) qp%E_bare(i_s)=qp2merge(i_db)%E_bare(i_sp)
!           if (plan_EWZ(i_db,1)) then
!             if (i_db==1) qp%E(i_s)=qp%E(i_s)+real(qp2merge(i_db)%E(i_sp))
!             if (i_db> 1) qp%E(i_s)=qp%E(i_s)+real(qp2merge(i_db)%E(i_sp))-qp%E_bare(i_s)
!           endif
!           qp%E_bare(i_s)=qp2merge(i_db)%E_bare(i_sp)
!           if (plan_EWZ(i_db,2)) qp%E(i_s)=qp%E(i_s)+cI*aimag(qp2merge(i_db)%E(i_sp))
!           if (plan_EWZ(i_db,3)) qp%Z(i_s)=qp%Z(i_s)+qp2merge(i_db)%Z(i_sp)
!         else
!           qp%S_total(i_s,:)=qp2merge(i_db)%S_total(i_sp,:)
!           qp%GreenF(i_s,:)=qp2merge(i_db)%GreenF(i_sp,:)
!           qp%GreenF_W(i_s,:)=qp2merge(i_db)%GreenF_W(i_sp,:)
!         endif
!         qp%k(qp%table(i_s,3),:)=qp2merge(i_db)%k( qp2merge(i_db)%table(i_sp,3),:)
!         qp%wk(qp%table(i_s,3) )=qp2merge(i_db)%wk( qp2merge(i_db)%table(i_sp,3) )
!       endif
!     enddo
!   enddo
! enddo
 !
 YAMBO_FREE(dummy_table)
 !
end subroutine
