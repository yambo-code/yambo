!
! Copyright (C) 2000-2012 D. Sangalli, A. Marini and the YAMBO team 
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
 logical                  :: chi_nn
 logical                  :: chi_jj
 integer                  :: iw_ref
 !
 ! BSE via X_nn
 complex(SP), allocatable :: DIP_q_dot_iR_kerr(:,:,:,:)
 !
 ! BSE via X_jj
 complex(SP), allocatable :: DIP_P_symm(:,:,:,:,:)
 !
 ! X
 !
 complex(SP), allocatable :: Xo_head(:,:)
 complex(SP), allocatable :: Xo_wing_R(:,:,:)
 complex(SP), allocatable :: Xo_wing_L(:,:,:)
 !
 complex(SP), allocatable :: X_head(:,:)
 !
 ! BSE
 !
 complex(SP),allocatable  :: BSS_rhoq0_kerr(:)
 !
 interface
   !
   subroutine KERR_shadow(what,l_flag,n1,n2,c1)
     use pars,  ONLY:SP
     character(*)         :: what
     integer,optional     :: n1,n2
     complex(SP),optional :: c1(:)
     logical,optional     :: l_flag
   end subroutine
   !
 end interface
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
         if (allocated(Xo_head)) return
         allocate(Xo_head(9,d(2)),stat=err)
         allocate(X_head(9,d(2)) ,stat=err)
         if(d(1)>1) then
           allocate(Xo_wing_R(3,d(1)-1,d(2)),stat=err)
           allocate(Xo_wing_L(3,d(1)-1,d(2)),stat=err)
           call mem_est(what,(/2*product(d)-d(1)/),errors=(/err/))
         endif
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
       case('X')
         if (.not.allocated(Xo_head)) return
         deallocate(Xo_head,X_head)
         if(allocated(Xo_wing_R)) deallocate(Xo_wing_R)
         if(allocated(Xo_wing_L)) deallocate(Xo_wing_L)
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
end module
