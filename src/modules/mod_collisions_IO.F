!
! Copyright (C) 2000-2009 A. Marini, C. Attaccalite and the YAMBO team 
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
 use pars,            ONLY:SP,LP,schlen
 implicit none
 !
 complex(SP), allocatable :: RHO_VX_RHO(:)
 integer(LP), allocatable :: RHO_table(:,:,:) 
 integer                  :: n_rho_states
 integer                  :: V_kind 
 !
 ! Cut off on exchange matrix elements
 ! Sx_cutoff=0 all, (Sx_cutoff>1 or Sx_cutoff<0)=none
 !
 real(SP)                 :: Sx_cutoff
 real(SP)                 :: sx_max
 complex(SP), allocatable :: tmp_rho_vx(:,:,:)
 logical, allocatable     :: QP_DB_done(:)
 !
 interface
   !
   integer function io_COLLISIONS(i_qp,ID,QP_DB_done)
     use QP_m,      ONLY:QP_n_states
     integer           :: i_qp,ID
     logical, optional :: QP_DB_done(QP_n_states)
   end function 
   !
 end interface
 !
 contains
   !
   subroutine collisions_IO_alloc(rho_VX_input_size)
     use pars,           ONLY:IP
     use QP_m,           ONLY:QP_nk,QP_nb
     use memory_m,       ONLY:mem_est
     implicit none
     !
     integer, intent(in), optional :: rho_VX_input_size
     !
     ! Work Space
     !
     integer :: rho_VX_size
     !
     if(present(rho_VX_input_size)) then
       rho_VX_size=rho_VX_input_size
     else
       rho_VX_size=QP_nb**2*QP_nk
     endif
     !
     call mem_est("RHO_VX_RHO",(/size(RHO_VX_RHO)/),(/2*SP/))
     allocate(RHO_VX_RHO(rho_VX_size))
     allocate(RHO_table(QP_nb,QP_nb,QP_nk))
     call mem_est("RHO_table",(/size(RHO_table)/),(/LP/))
     !
   end subroutine
   !
   subroutine collisions_IO_free()
     use memory_m,       ONLY:mem_est
     implicit none
     if(allocated(RHO_VX_RHO)) then
       deallocate(RHO_VX_RHO)
       call mem_est("RHO_VX_RHO")
     endif
     if(allocated(RHO_table)) then
       deallocate(RHO_table)
       call mem_est("RHO_table")
     endif
   end subroutine
   !
end module
