!
! Copyright (C) 2000-2012 A. Marini, C. Attaccalite and the YAMBO team 
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
module collisions_IO
 !
 use pars,            ONLY:SP,LP,schlen,lchlen
 implicit none
 !
 complex(SP), allocatable :: COLLISIONS_v(:)
 integer(LP), allocatable :: COLLISIONS_table(:,:,:) 
 integer                  :: n_collisions
 integer                  :: V_kind 
 character(lchlen)        :: COLLISIONS_path
 !
 ! Cut off on exchange matrix elements
 ! Sx_cutoff=0 all, (Sx_cutoff>1 or Sx_cutoff<0)=none
 !
 real(SP)                 :: COLLISIONS_cutoff
 real(SP)                 :: COLLISIONS_max
 complex(SP), allocatable :: TEMP_COLLISIONS_v(:,:,:)
 !
 ! I/O descriptions
 !
 character(lchlen):: COLLISION_description(100)
 !
 interface
   !
   integer function io_COLLISIONS(i_qp,ID,X)
     use X_m,        ONLY:X_t
     integer, intent(inout) ::ID
     integer, intent(in)    ::i_qp
     type(X_t), optional    ::X
   end function
   !
 end interface
 !
 contains
   !
   subroutine collisions_IO_alloc(V_size_input)
     use pars,           ONLY:IP
     use QP_m,           ONLY:QP_nk,QP_nb
     use memory_m,       ONLY:mem_est
     implicit none
     !
     integer, intent(in), optional :: V_size_input
     !
     ! Work Space
     !
     integer :: V_size
     !
     if(present(V_size_input)) then
       V_size=V_size_input
     else
       V_size=QP_nb**2*QP_nk
     endif
     !
     allocate(COLLISIONS_v(V_size))
     call mem_est("COLLISIONS_v",(/size(COLLISIONS_v)/),(/2*SP/))
     allocate(COLLISIONS_table(QP_nb,QP_nb,QP_nk))
     call mem_est("COLLISIONS_table",(/size(COLLISIONS_table)/),(/LP/))
     !
   end subroutine
   !
   subroutine collisions_IO_free()
     use memory_m,       ONLY:mem_est
     implicit none
     if(allocated(COLLISIONS_v)) then
       deallocate(COLLISIONS_v)
       call mem_est("COLLISIONS_v")
     endif
     if(allocated(COLLISIONS_table)) then
       deallocate(COLLISIONS_table)
       call mem_est("COLLISIONS_table")
     endif
   end subroutine
   !
end module
