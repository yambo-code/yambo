!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
module collision
 !
 use pars, ONLY :SP,LP
 !
 implicit none
 !
 type ggwinfo
   !
   !  is(3)=(ib,ik,is,isp) --<--:--<-- os(3)=(ob,ok,os,osp)
   !                             :
   !                           /:\ iqs=(ig0,iq,is)
   !                            :
   !  iq_is = bz(ik_is-ok_os)+Go
   !
   integer :: is(4) 
   integer :: os(4)
   integer :: qs(3)
   integer :: ngrho,iqref
   complex(SP), pointer :: gamp(:,:) => null()
   complex(SP), pointer :: rhotw(:)  => null()
 end type ggwinfo
 !
 logical :: COLLISIONS_parallel_SC_structure=.FALSE.
 !
 type COLLISIONS_element
   integer(LP), allocatable  :: table(:,:,:)
   complex(SP), allocatable  :: v(:)
   complex(SP), allocatable  :: v3(:,:,:)
 end type
 !
 type(COLLISIONS_element), allocatable :: COLLISIONS(:)
 !
 contains
   !
   subroutine free_COLLISIONS_v
     use memory_m,       ONLY:mem_est
     use stderr,         ONLY:intc
     use QP_m,           ONLY:QP_n_states
     use parallel_m,     ONLY:PAR_IND_QP,PAR_QP_index
     !
     implicit none
     !
     ! Work Space
     !
     integer :: i_qp,i_qp_mem
     !
     do i_qp=1,QP_n_states
       !
       if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
       i_qp_mem=PAR_QP_index(i_qp)
       !
       if (allocated(COLLISIONS(i_qp_mem)%table)) then
         deallocate(COLLISIONS(i_qp_mem)%table)
         call mem_est("QP_COLLISIONS_table"//trim(intc((i_qp))))
       endif
       !
       if (allocated(COLLISIONS(i_qp_mem)%v)) then
         deallocate(COLLISIONS(i_qp_mem)%v)
         call mem_est("QP_COLLISIONS_v"//trim(intc((i_qp))))
       endif
       !
       if (allocated(COLLISIONS(i_qp_mem)%v3)) then
         deallocate(COLLISIONS(i_qp_mem)%v3)
         call mem_est("QP_COLLISIONS_v3"//trim(intc((i_qp))))
       endif
       !
     enddo
     !
     deallocate(COLLISIONS)
     !
   end subroutine free_COLLISIONS_v
   !
   subroutine collision_reset(ggw)
     type(ggwinfo)::ggw
     if(associated(ggw%gamp))  deallocate(ggw%gamp)
     if(associated(ggw%rhotw)) deallocate(ggw%rhotw)
     nullify(ggw%gamp,ggw%rhotw)
   end subroutine
   !
end module collision
