!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP
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
subroutine PHEL_double_grid_subset(k,k_FG,k_n)
 !
 ! Select double grid subset to be used in PHEL calculations
 !
 use R_lattice,      ONLY:bz_samp,bz_fine_grid,nkDg,nk_FG_per_kpt
 use PHEL,           ONLY:l_PH_uses_E_FineGd
 use real_time,      ONLY:l_RT_uses_E_FineGd
 !
#include<memory.h>
 !
 type(bz_samp),     intent(in) :: k
 type(bz_fine_grid),intent(in) :: k_FG
 integer,           intent(in) :: k_n
 !
 ! Work Space ...
 !
 ! Select the double grid subset to use
 !=====================================
 !
 ! 1. Fine grid is used
 if (l_PH_uses_E_FineGd .or. l_RT_uses_E_FineGd) then
   ! 1.1 Sanity check
   if (k_FG%N.le.k_n) then
     nk_FG_per_kpt = -1
   else
     ! 1.2 No subset or incorrect value of nkDg provided
     if (nkDg<0.or.nkDg>k_FG%N-k_n) then
       nkDg=-1
       nk_FG_per_kpt = -1
     else
       ! 1.3 Do not use double grid at all
       if (nkDg == 0) then 
         nk_FG_per_kpt=0
       else
         ! 1.4 Use subset of fine grid
         nk_FG_per_kpt=ceiling(float(nkDg)/float(k_n))
         nkDg = nk_FG_per_kpt*k_n
       endif
     endif
   endif
 ! 2. Fine grid is not used
 else
   nkDg=-1
   nk_FG_per_kpt=-1
 endif
 !
end subroutine PHEL_double_grid_subset
 
