!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
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
module oscillators_IO
 !
 use pars,            ONLY:SP,schlen
 implicit none
 !
 complex(SP), allocatable :: rho_VX_rho(:)
 integer,     allocatable :: rho_table(:,:) 
 integer                  :: n_rho_states
 integer                  :: V_kind 
 !
 interface
   !
   integer function ioRHO_RHO_VX(i_qp,ID,QP_DB_done)
     use QP_m,      ONLY:QP_n_states
     integer           :: i_qp,ID
     logical, optional :: QP_DB_done(QP_n_states)
   end function 
   !
 end interface
 !
 contains
   !
   subroutine oscillators_IO_alloc()
     use pars,           ONLY:IP
     use QP_m,           ONLY:QP_nk
     use SC,             ONLY:SC_bands
     use memory_m,       ONLY:mem_est
     if (n_rho_states==0) then
       allocate(rho_VX_rho(SC_bands*SC_bands*QP_nk))
       call mem_est("rho_VX_rho",(/size(rho_VX_rho)/),(/2*SP/))
       allocate(rho_table(3,SC_bands*SC_bands*QP_nk))
       call mem_est("rho_table",(/size(rho_table)/),(/IP/))
     endif
   end subroutine
   !
   subroutine oscillators_IO_free()
     use memory_m,       ONLY:mem_est
     deallocate(rho_VX_rho)
     deallocate(rho_table)
     call mem_est("rho_VX_rho  rho_table")
  end subroutine
 !
end module
