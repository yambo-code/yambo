!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
!================
   use pars
!
! Bare 2 Coarse grid
!
! cg_npts      ! Coarse grid points
! cg_pt        ! Coarse grid point
! cg_index_bg(ibg)  ! Tells the index in the reordered (not coarse) 
!                   ! grid of the ibg-th element
!                   ! of the original (not sorted) grid
! bg_npts(icg) ! Tells how many poles are linked to the POLE of the
!              ! non degenerate new grid (icg)
!
   integer :: cg_npts
   real(SP),allocatable :: cg_pt(:)
   integer, allocatable :: rg_index_bg(:)
   integer, allocatable :: cg_index_bg(:)
   integer, allocatable :: bg_npts(:)
   type w_samp
     integer :: n(2)
     real(SP):: er(2)
     real(SP):: dr(2)
     real(SP):: per_memstps
     complex(SP),pointer :: p(:) 
   end type w_samp
!
 contains
!
 subroutine W_duplicate(Wi,Wo)
!=============================
 type(w_samp):: Wi,Wo
 Wo%n=Wi%n
 Wo%per_memstps=Wi%per_memstps
 Wo%er=Wi%er
 Wo%dr=Wi%dr
 end subroutine W_duplicate

 subroutine W_reset(W)
!=====================
 use memory_m, ONLY:mem_est
 type(w_samp)::W
 W%n=100
#if defined PJ_SC
 W%n=1
#endif
 W%per_memstps=100.
 W%er=(/0.,10./)/HARTREE
 W%dr=(/.1,.1/)/HARTREE
 nullify(W%p)
 call mem_est("W-p")
 end subroutine W_reset


 end module frequency
