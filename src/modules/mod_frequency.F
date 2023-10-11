!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module frequency
 !
 use pars,  ONLY:SP
 use units, ONLY:HA2EV
 !
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
   real(SP)     :: ir(2)
   real(SP)     :: damp_reference
   real(SP)     :: dr(2)
   real(SP)     :: per_memstps
   complex(SP),allocatable :: p(:)
   character(2) :: samp_type
   character(2) :: samp_grid
   character(2) :: mpa_solver
   character(16):: grid_type
 end type w_samp
 !
 contains
   !
   subroutine W_duplicate(Wi,Wo)
     type(w_samp):: Wi,Wo
     Wo%n_freqs=Wi%n_freqs
     Wo%per_memstps=Wi%per_memstps
     Wo%er=Wi%er
     Wo%ir=Wi%ir
     Wo%dr=Wi%dr
     Wo%damp_reference=Wi%damp_reference
     if (allocated(Wi%p)) then
       YAMBO_ALLOC(Wo%p,(Wo%n_freqs))
       Wo%p=Wi%p
     endif
     Wo%samp_type=Wi%samp_type
     Wo%samp_grid=Wi%samp_grid
     Wo%mpa_solver=Wi%mpa_solver
     Wo%grid_type=Wi%grid_type
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
     W%ir=(/1._SP,1._SP/)!/HA2EV
     W%dr=(/local_damping,local_damping/)/HA2EV
     W%damp_reference=0._SP
     YAMBO_FREE(W%p)
     W%samp_type="2l"
     W%samp_grid="lP"
     W%mpa_solver="PT"
     W%grid_type="ra"
   end subroutine W_reset
   !
end module frequency
