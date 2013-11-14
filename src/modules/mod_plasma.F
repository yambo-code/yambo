!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team 
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
module plasma
 !
 use pars, ONLY :SP
 !
 implicit none
 !
 ! NOTE: Plasma_tab is on bg (big grid), plasma_tab_m1 points to cg (coarse_grid).
 !       Plasma_grid connects the two
 !
 ! plasma_tab:    (ipole,iqbz,2)=(J_bg,iq)  --> 2=(i_qp,i_pbz)       =(m,m',p)
 ! plasma_tab_m1:   (ipole)     =(J_cg)     <--   (i_qp,i_pbz,i_qpbz)=(m,m',p,iq)
 !
 ! plasma_grid: (ipole_cg,iqbz,2)=(J,iq) --> 2=(ipole1,ipole2)=extrema on the big grid
 !
 integer                   ::  max_N_plasma_poles
 real(SP)                  ::  Plasma_redux_percent
 !
 integer, allocatable      ::  N_plasma_poles(:)       ! iqbz
 integer, allocatable      ::  Plasma_tab(:,:,:)       ! iJ,iqbz,2
 integer, allocatable      ::  Plasma_tab_m1(:,:,:)    ! i_qp,i_pbz,i_qpbz
 integer, allocatable      ::  Plasma_grid(:,:,:)      ! iJ,iqbz,2
 !
 contains
   !
   subroutine Plasma_free()
     !
     use memory_m,       ONLY:mem_est
     !
     if(allocated(N_plasma_poles)) then
       deallocate(N_plasma_poles)
     endif
     !
     if(allocated(Plasma_tab)) then
       deallocate(Plasma_tab)
       call mem_est("Plasma_tab")
     endif
     !
     if(allocated(Plasma_tab_m1)) then
       deallocate(Plasma_tab_m1)
       call mem_est("Plasma_tab_m1")
     endif
     !
     if(allocated(Plasma_grid)) then
       deallocate(Plasma_grid)
       call mem_est("Plasma_grid")
     endif
     !
   end subroutine
   !
end module plasma
