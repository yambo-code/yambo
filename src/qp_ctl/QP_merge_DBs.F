!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine QP_merge_DBs(n_dbs,qp2merge,qp)
 !
 use QP_m,           ONLY:QP_t,QP_reset
 use electrons,      ONLY:n_sp_pol
 use com,            ONLY:error
 !
 implicit none
 !
 integer         ::n_dbs
 type(QP_t)      ::qp2merge(n_dbs),qp
 !
 ! Work Space
 !
 integer  ::i_db,i1,i2,n_states_merged,nk_merged,nb_merged
 integer, allocatable :: dummy_table(:,:)
 !
 n_states_merged=qp2merge(1)%n_states
 qp%n_descs=qp2merge(1)%n_descs+n_dbs-1
 !
 allocate(dummy_table(sum(qp2merge(:)%n_states),3+n_sp_pol-1))
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
 !
 allocate(qp%Z(qp%n_states),qp%E(qp%n_states),qp%E_bare(qp%n_states))
 allocate(qp%k(qp%nk,3),qp%table(qp%n_states,3+n_sp_pol-1))
 !
 qp%table=dummy_table(:qp%n_states,:)
 !
 do i1=1,qp%n_states
   do i_db=1,n_dbs
     do i2=1,qp2merge(i_db)%n_states
       if (all(qp2merge(i_db)%table(i2,:)==qp%table(i1,:))) then
         qp%Z(i1)=qp2merge(i_db)%Z(i2)
         qp%E(i1)=qp2merge(i_db)%E(i2)
         qp%E_bare(i1)=qp2merge(i_db)%E_bare(i2)
         qp%k(qp%table(i1,3),:)=qp2merge(i_db)%k( qp2merge(i_db)%table(i2,3),:)
       endif
     enddo
   enddo
 enddo
 !
 do i_db=1,n_dbs
   call QP_reset(qp2merge(i_db))
 enddo
 deallocate(dummy_table)
 !
end subroutine
