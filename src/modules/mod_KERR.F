!
! Copyright (C) 2000-2010 D. Sangalli, A. Marini and the YAMBO team 
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
module KERR
 !
 use pars,  ONLY:SP
 !
 implicit none
 !
 complex(SP), allocatable :: X_epsilon_xy(:,:)
 complex(SP), allocatable :: X_Kerr_par(:,:)
 complex(SP), allocatable :: DIP_q_dot_iR_kerr(:,:,:,:)
 complex(SP), allocatable :: X_mat_wing_right(:,:)
 complex(SP), allocatable :: X_mat_wing_left(:,:)
 !
 contains
   !
   subroutine KERR_alloc(what,d)
     !
     use electrons, ONLY:n_sp_pol
     use memory_m,  ONLY:mem_est
     character(*)      :: what
     integer, optional :: d(:)
     !
     ! Work Space
     !
     integer :: err
     !
     if (present(d)) then
       select case(what)
       case('X')
         if (allocated(X_mat_wing_right)) return
         allocate(X_mat_wing_right(d(1),d(2)),stat=err)
         allocate(X_mat_wing_left(d(1),d(2)),stat=err)
         call mem_est(what,(/2*product(d)/),errors=(/err/))
       case('DIP_q_dot_iR')
         if (allocated(DIP_q_dot_iR_kerr)) return
         allocate(DIP_q_dot_iR_kerr(d(1),d(2),d(3),n_sp_pol),stat=err)
         call mem_est(what,(/product(d)*n_sp_pol/),errors=(/err/))
       end select
     else
       select case(what)
       case('X')
         if (.not.allocated(X_mat_wing_right)) return
         deallocate(X_mat_wing_right)
         deallocate(X_mat_wing_left)
       case('DIP_q_dot_iR')
         if (.not.allocated(DIP_q_dot_iR_kerr)) return
         deallocate(DIP_q_dot_iR_kerr)
       end select
       call mem_est(what)
     endif
   end subroutine
   !
end module
