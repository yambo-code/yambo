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
subroutine QP_merge_DBs(n_dbs,qp2merge,qp,plan_EWZ)
 !
 use pars,           ONLY:cZERO,rZERO,cI
 use QP_m,           ONLY:QP_t,QP_reset
 use electrons,      ONLY:n_sp_pol
 use com,            ONLY:error
 !
#include<memory.h>
 !
 integer         ::n_dbs
 type(QP_t)      ::qp2merge(n_dbs),qp
 logical         ::plan_EWZ(n_dbs,3)
 !
 ! Work Space
 !
 integer  ::i_db,i1,i2,n_states_merged
 logical  ::l_QP_db
 integer, allocatable :: dummy_table(:,:)
 !
 n_states_merged=qp2merge(1)%n_states
 qp%n_descs=qp2merge(1)%n_descs+n_dbs-1
 !
 YAMBO_ALLOC(dummy_table,(sum(qp2merge(:)%n_states),3+n_sp_pol-1))
 dummy_table=0
 dummy_table(:n_states_merged,:)=qp2merge(1)%table(:,:)
 qp%description(:qp2merge(1)%n_descs)=qp2merge(1)%description(:qp2merge(1)%n_descs)
 !
 do i_db=2,n_dbs
   do i1=1,qp2merge(i_db)%n_descs
     if ( trim(qp2merge(i_db)%description(i1)) /= trim(qp2merge(1)%description(i1)) )then
       if (index('QP @ K',qp2merge(1)%description(i1))==0) then
         qp%description(qp2merge(1)%n_descs+i_db-1)=qp2merge(i_db)%description(i1)
         cycle
       endif
       call error('Incompatible Databases. Description strings differ.')
     endif 
   enddo
   do i1=1,qp2merge(i_db)%n_states
     do i2=1,n_states_merged
       if (all(qp2merge(i_db)%table(i1,:)==dummy_table(i2,:))) goto 1
     enddo
     n_states_merged=n_states_merged+1
     dummy_table(n_states_merged,:)=qp2merge(i_db)%table(i1,:)
1    continue
   enddo
 enddo
 !
 qp%nb=maxval(dummy_table(:,1))
 qp%nk=maxval(dummy_table(:,3))
 qp%n_states=n_states_merged
 qp%GreenF_n_steps=qp2merge(1)%GreenF_n_steps
 !
 l_QP_db=allocated(qp2merge(1)%E)
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
 qp%table=dummy_table(:qp%n_states,:)
 !
 do i1=1,qp%n_states
   do i_db=1,n_dbs
     do i2=1,qp2merge(i_db)%n_states
       if (all(qp2merge(i_db)%table(i2,:)==qp%table(i1,:))) then
         if (l_QP_db) then
           if (i_db==1) qp%E_bare(i1)=qp2merge(i_db)%E_bare(i2)
           if (plan_EWZ(i_db,1)) then
             if (i_db==1) qp%E(i1)=qp%E(i1)+real(qp2merge(i_db)%E(i2))
             if (i_db> 1) qp%E(i1)=qp%E(i1)+real(qp2merge(i_db)%E(i2))-qp%E_bare(i1)
           endif
           qp%E_bare(i1)=qp2merge(i_db)%E_bare(i2)
           if (plan_EWZ(i_db,2)) qp%E(i1)=qp%E(i1)+cI*aimag(qp2merge(i_db)%E(i2))
           if (plan_EWZ(i_db,3)) qp%Z(i1)=qp%Z(i1)+qp2merge(i_db)%Z(i2)
         else
           qp%S_total(i1,:)=qp2merge(i_db)%S_total(i2,:)
           qp%GreenF(i1,:)=qp2merge(i_db)%GreenF(i2,:)
           qp%GreenF_W(i1,:)=qp2merge(i_db)%GreenF_W(i2,:)
         endif
         qp%k(qp%table(i1,3),:)=qp2merge(i_db)%k( qp2merge(i_db)%table(i2,3),:)
         qp%wk(qp%table(i1,3) )=qp2merge(i_db)%wk( qp2merge(i_db)%table(i2,3) )
       endif
     enddo
   enddo
 enddo
 !
 do i_db=1,n_dbs
   call QP_reset(qp2merge(i_db))
 enddo
 YAMBO_FREE(dummy_table)
 !
end subroutine
