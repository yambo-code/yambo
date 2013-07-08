!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
module collisions_CACHE
 !
 use pars,       ONLY:SP,LP
 implicit none
 !
 type COLLISION_cache
   integer(LP), allocatable  :: COLLISIONS_table(:,:,:)
   complex(SP), allocatable  :: COLLISIONS_v(:)
   complex(SP), allocatable  :: COLLISIONS_v3(:,:,:)
 end type
 !
 real(SP)                           :: COLLISIONS_cache_size
 real(SP)                           :: cache_memory
 integer                            :: last_QP_mem
 type(COLLISION_cache), allocatable :: CACHE(:)
 !
 contains
   !
   subroutine free_COLLISIONS_v
     use memory_m,       ONLY:mem_est
     use stderr,         ONLY:intc
     use parallel_m,     ONLY:PP_indexes,PP_indexes_reset
     use interfaces,     ONLY:PARALLEL_index       
     use QP_m,           ONLY:QP_n_states
     !
     implicit none
     !
     ! Work Space
     !
     integer :: i_qp
     type(PP_indexes)  :: px
     !
     if(COLLISIONS_cache_size==0.) return
     !
     call PP_indexes_reset(px)   
     !
     call PARALLEL_index(px,(/QP_n_states/))
     !
     do i_qp=1,last_QP_mem
       !
       if(.not.px%element_1D(i_qp)) cycle  
       !
       deallocate(CACHE(i_qp)%COLLISIONS_table,CACHE(i_qp)%COLLISIONS_v)
       call mem_est("QP_COLLISIONS_table"//trim(intc((i_qp))))
       call mem_est("QP_COLLISIONS_v"//trim(intc(i_qp)))
       !
     enddo
     !
     last_QP_mem=0
     COLLISIONS_cache_size=0.0
     !
     deallocate(CACHE)
     !
     call PP_indexes_reset(px)
     !
   end subroutine free_COLLISIONS_v
   !
end module
