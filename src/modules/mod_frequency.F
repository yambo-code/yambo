!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module frequency
 !
 use pars,  ONLY:SP
 use units, ONLY:HA2EV
#include<memory.h>
 !
 ! Bare 2 Coarse grid
 !
 ! coarse_grid_N          ! Coarse grid points
 ! coarse_grid_Pt         ! Coarse grid point
 ! coarse_grid_index(ibg) ! Tells the index in the reordered (not coarse) 
 !                        ! grid of the ibg-th element
 !                        ! of the original (not sorted) grid
 ! ordered_grid_index(ibg)! Tells the index in the reordered (not coarse) 
 !                        ! grid of the ibg-th element
 !                        ! of the original (not sorted) grid
 ! bare_grid_N(icg)       ! Tells how many poles are linked to the POLE of the
 !                        ! non degenerate new grid (icg)
 !
 integer :: coarse_grid_N
 real(SP),allocatable :: coarse_grid_Pt(:)
 integer, allocatable :: ordered_grid_index(:)
 integer, allocatable :: coarse_grid_index(:)
 integer, allocatable :: bare_grid_N(:)
 !
 type w_samp
   integer      :: n_freqs
   real(SP)     :: er(2)
   real(SP)     :: damp_reference
   real(SP)     :: dr(2)
   real(SP)     :: per_memstps
   complex(SP),allocatable :: p(:)
 end type w_samp
 !
 contains
   !
   subroutine W_duplicate(Wi,Wo)
     type(w_samp):: Wi,Wo
     Wo%n_freqs=Wi%n_freqs
     Wo%per_memstps=Wi%per_memstps
     Wo%er=Wi%er
     Wo%dr=Wi%dr
     Wo%damp_reference=Wi%damp_reference
     if (allocated(Wi%p)) then
       YAMBO_ALLOC(Wo%p,(Wo%n_freqs))
       Wo%p=Wi%p
     endif
   end subroutine W_duplicate
   !
   subroutine W_reset(W,damping)
     type(w_samp)::W
     real(SP), optional, intent(in) :: damping
     real :: local_damping
     local_damping=0.1_SP
     if(present(damping)) local_damping=damping
     W%n_freqs=100
     W%per_memstps=100._SP
     W%er=(/0._SP,10._SP/)/HA2EV
     W%dr=(/local_damping,local_damping/)/HA2EV
     W%damp_reference=0._SP
     YAMBO_FREE(W%p)
   end subroutine W_reset
   !
end module frequency
