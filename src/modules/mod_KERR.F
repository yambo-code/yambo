!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
 complex(SP)              :: B_Hall(2)
 complex(SP), allocatable :: BSS_rhoq0_kerr(:)
 complex(SP), allocatable :: DIP_q_dot_iR_kerr(:,:,:,:)
 complex(SP), allocatable :: DIP_P_symm(:,:,:,:,:)
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
       case('DIP_q_dot_iR')
         if (allocated(DIP_q_dot_iR_kerr)) return
         allocate(DIP_q_dot_iR_kerr(d(1),d(2),d(3),n_sp_pol),stat=err)
         call mem_est(what,(/product(d)*n_sp_pol/),errors=(/err/))
       case('DIP_P_symm')
         if (allocated(DIP_P_symm)) return
         allocate(DIP_P_symm(d(1),d(2),d(3),d(4),n_sp_pol),stat=err)
         call mem_est(what,(/product(d)*n_sp_pol/),errors=(/err/))
       end select
     else
       select case(what)
       case('DIP_q_dot_iR')
         if (.not.allocated(DIP_q_dot_iR_kerr)) return
         deallocate(DIP_q_dot_iR_kerr)
       case('DIP_P_symm')
         if (.not.allocated(DIP_P_symm)) return
         deallocate(DIP_P_symm)
       end select
       call mem_est(what)
     endif
   end subroutine
   !
   !
   !
end module
