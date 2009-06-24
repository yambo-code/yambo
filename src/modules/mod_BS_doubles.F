!
! Copyright (C) 2000-2008 D. Sangalli and the YAMBO team 
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
module BS_doubles
 !
#if defined _DOUBLES
 !
 use pars, ONLY:SP
 implicit none
 !
 ! BS Kernel building
 !
 integer  :: BS_K_doubles_dim,BS_bands_doubles(2)
 logical  :: BS_res_K_doubles,eps2eel,doubles_corr
 integer     , allocatable ::  BS_doubles_table(:,:),BS_table_fixed_E(:,:)
 integer     , allocatable ::  tab_check2icv_zero(:),degeneracy_zero(:)
 integer     , allocatable ::  change(:,:), inv_change(:,:), tab_poles2iw(:)
 integer     , allocatable ::  static_approx(:)
 complex(SP) , allocatable ::  BS_mat_doubles(:,:,:)
 complex(SP) , allocatable ::  BS_mat_tmp(:,:,:)
 complex(SP) , allocatable ::  Eigen_singles(:,:),Eigen_doubles(:,:)
 real(SP)    , allocatable ::  BS_doubles_E(:)
 !
 ! Solvers
 !
#endif
 !
end module BS_doubles
