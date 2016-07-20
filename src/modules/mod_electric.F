! 
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG AC
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
module electric
 !
 use pars,       ONLY:SP,schlen,lchlen
 use electrons,  ONLY:levels
 !
 complex(SP)            :: NL_initial_P(3) ! in principle it can be different from zero
 !
 ! Variable for overlaps and Berry phase
 !
 complex(SP), allocatable :: All_S_det(:,:,:)
 complex(SP), allocatable :: Sm1_plus(:,:,:,:,:)
 complex(SP), allocatable :: Sm1_minus(:,:,:,:,:)
 !
 ! Electric Field operator used in SC
 !
 complex(SP), allocatable :: W_electric(:,:,:,:)
 !
 logical  :: l_force_SndOrd  ! Use second order dipoles as in the old code (default 4th order)
 !
 contains
   !
   subroutine ELECTRIC_alloc(en)
     !
     use pars,           ONLY:SP
     use memory_m,       ONLY:mem_est
     use electrons,      ONLY:levels,n_sp_pol
     use R_lattice,      ONLY:nXkbz
     !
     implicit none
     !
     type(levels), intent(in) :: en
     !
     allocate(All_S_det(6,nXkbz,n_sp_pol))
     call mem_est("All_S_det",(/size(All_S_det)/))
     allocate(Sm1_plus(en%nbf,en%nbf,6,nXkbz,n_sp_pol))
     call mem_est("Sm1_plus",(/size(Sm1_plus)/))
     allocate(Sm1_minus(en%nbf,en%nbf,6,nXkbz,n_sp_pol))
     call mem_est("Sm1_minus",(/size(Sm1_minus)/))
     !
   end subroutine ELECTRIC_alloc
   
   subroutine ELECTRIC_free
     !
     use memory_m,       ONLY:mem_est
     !
     implicit none
     !
     deallocate(All_S_det,Sm1_plus,Sm1_minus)
     call mem_est("All_S_det Sm1_plus Sm1_minus")
     !
   end subroutine ELECTRIC_free
   !
end module
