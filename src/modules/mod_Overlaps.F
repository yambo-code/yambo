!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA
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
module Overlaps_m
 !
 use pars,  ONLY:SP
 !
 implicit none
 !
 complex(SP), allocatable :: S_det(:,:)        
 !
 interface
   !
   subroutine Build_overlaps_det(en,ik,k_map,X,S_det,Sm1_plus,Sm1_minus,V_bands)
     use electrons,      ONLY:levels,n_sp_pol
     use R_lattice,      ONLY:bz_grid
     use pars,           ONLY:SP
     use QP_m,           ONLY:QP_nk
     use SC,             ONLY:SC_bands
     use X_m,            ONLY:X_t
     type(levels),           intent(in)  :: en  
     integer,                intent(in)  :: ik  
     type(bz_grid),          intent(in)  :: k_map
     type(X_t),              intent(in)  :: X
     complex(SP),            intent(in)  :: DIP_S(X%ib(2),X%ib(2),en%nk,3,n_sp_pol)
     complex(SP),            intent(out) :: S_det(3)
     complex(SP), optional,  intent(out) :: Sm1_plus (3,en%nbf,en%nbf)
     complex(SP), optional,  intent(out) :: Sm1_minus(3,en%nbf,en%nbf)
     complex(SP), optional,  intent(in)  :: V_bands(en%nbf,SC_bands(1):SC_bands(2),QP_nk)
   end subroutine Build_overlaps_det
   !
   integer function io_Overlaps(X,Xen,ID,DIP_S)
     !
     use pars,          ONLY:SP
     use X_m,           ONLY:X_t
     use electrons,     ONLY:levels,n_sp_pol
     use R_lattice,     ONLY:nXkbz
     use matrix_operate,ONLY:mat_c2r,mat_r2c
     !
     implicit none
     type(X_t)   ::X
     type(levels)::Xen
     integer     ::ID
     complex(SP),  optional :: DIP_S(X%ib(2),X%ib(2),Xen%nk,3,n_sp_pol)
     !
   end function io_Overlaps
   !
 end interface
 !
 contains
   !
   subroutine S_alloc(Xen,Xk)
     !
     use R_lattice, ONLY:bz_samp
     use electrons, ONLY:levels
     use memory_m,  ONLY:mem_est
     use X_m,       ONLY:l_dip_cov
     use drivers,   ONLY:l_real_time
     !
     type(bz_samp), intent(in) :: Xk
     type(levels),  intent(in) :: Xen
     !
     allocate(S_det(3,Xk%nbz))
     call mem_est("S_det",(/size(S_det)/),(/2*SP/))
     !
   end subroutine S_alloc
   !
   subroutine S_dealloc()
     !
     use memory_m,  ONLY:mem_est
     use X_m,       ONLY:l_dip_cov
     use drivers,   ONLY:l_real_time
     !
     deallocate(S_det)
     call mem_est("S_det")
     !
   end subroutine S_dealloc    
   !
end module
