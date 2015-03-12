!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine FREQUENCIES_reset(Xw,what)
 !
 ! Input
 !-------
 ! integer     :: npts
 ! real(SP)    :: bg_pt(npts),cg_percentual
 !
 ! Deallocate:  
 ! bg_pt(:), cg_pt(:), cg_index_bg(:),
 ! X_poles_tab(:), rg_index_bg(:), bg_npts(:)
 !
 use frequency,   ONLY:w_samp,bare_grid_N,coarse_grid_Pt,ordered_grid_index,coarse_grid_index
 use memory_m,    ONLY:mem_est
 use X_m,         ONLY:X_poles_tab
 implicit none
 !
 type(w_samp)  :: Xw
 character(*)  :: what
 !
 if (what=="points".or.what=="all") then
   if(associated(Xw%p)) then
     deallocate(Xw%p)
     nullify(Xw%p)
   endif
   call mem_est("W-p")
 endif
 !
 if (what=="coarse_grid".or.what=="all") then
   if (allocated(ordered_grid_index)) deallocate(ordered_grid_index)
   if (allocated(X_poles_tab)) then
     deallocate(X_poles_tab,bare_grid_N,coarse_grid_Pt,coarse_grid_index)
   endif
   call mem_est("X_poles_tab RGi BGn CGp CGi")      
 endif
 !
end subroutine
