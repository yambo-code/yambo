!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
#include<memory.h>
 !
 ! NOTE: Plasma_tab is on bg (big grid), plasma_tab_m1 points to cg (coarse_grid).
 !       Plasma_grid connects the two
 !
 ! plasma_tab:    (ipole,iqbz,2)=(J_bg,iq)  --> 2=(i_qp,i_pbz)       =(m,m',p)
 ! plasma_tab_m1:   (ipole)     =(J_cg)     <--   (i_qp,i_pbz,i_qpbz)=(m,m',p,iq)
 !
 ! plasma_grid: (ipole_cg,iqbz,2)=(J,iq) --> 2=(ipole1,ipole2)=extrema on the big grid
 !
 integer   :: N_plasma_poles_global = 0
 real(SP)  :: PLASMA_redux_percent
 !
 type plasma_gas
   integer                   ::  max_N_poles
   real(SP)                  ::  redux_percent
   integer, allocatable      ::  N_poles(:)       ! iqbz
   integer, allocatable      ::  tab(:,:,:)       ! iJ,iqbz,2
   integer, allocatable      ::  tab_m1(:,:,:)    ! i_qp,i_pbz,i_qpbz
   integer, allocatable      ::  grid(:,:,:)      ! iJ,iqbz,2
   !
 end type plasma_gas
 !
 type(plasma_gas) :: Eh_gas
 !
 contains
   !
   subroutine PLASMA_alloc(Gas,DIM_)
     !
     use D_lattice,      ONLY:nsym
     !
     type(plasma_gas) :: Gas
     integer          :: DIM_(3)
     !
     YAMBO_ALLOC(Gas%N_poles,(DIM_(1)))
     !
     if(.not.allocated(Gas%tab).and.Gas%max_N_poles>0) then       
       YAMBO_ALLOC(Gas%tab,(Gas%max_N_poles,DIM_(2),2))
     endif
     !
     if(.not.allocated(Gas%tab_m1)) then
       YAMBO_ALLOC(Gas%tab_m1,(DIM_(3),nsym,DIM_(2)))
     endif
     !
   end subroutine
   !
   subroutine PLASMA_free(Gas)
     !
     type(plasma_gas) :: Gas
     !
     YAMBO_FREE(Gas%N_poles)
     YAMBO_FREE(Gas%tab)
     YAMBO_FREE(Gas%tab_m1)
     YAMBO_FREE(Gas%grid)
     !
   end subroutine
   !
end module plasma
