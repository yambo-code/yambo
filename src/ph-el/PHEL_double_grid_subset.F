!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine PHEL_double_grid_subset(k,k_FG,nk_CG)
 !
 ! Select double grid subset to be used in PHEL calculations
 !
 use R_lattice,      ONLY:bz_samp,bz_fine_grid
 use PHEL,           ONLY:l_PH_uses_E_FineGd,PH_DbGd_points
 use real_time,      ONLY:l_RT_uses_E_FineGd
 use com,            ONLY:msg
 !
 implicit none
 !
 type(bz_samp),     intent(in)    :: k
 type(bz_fine_grid),intent(inout) :: k_FG
 integer,           intent(in)    :: nk_CG
 ! 
 ! Workspace
 !
 logical :: FG_is_sane=.TRUE.
 integer :: N_FG_per_K_average,ik
 !
 if (.not.l_PH_uses_E_FineGd .and..not. l_RT_uses_E_FineGd) return
 if (PH_DbGd_points<0) return
 !
 call section("=","Electronic Fine Grid")
 !
 N_FG_per_K_average=ceiling(float(PH_DbGd_points)/float(nk_CG))
 !
 ! Sanity check
 if (PH_DbGd_points<0.or.PH_DbGd_points>k_FG%N) FG_is_sane=.FALSE.
 if (any(k_FG%N_mapped<N_FG_per_K_average))     FG_is_sane=.FALSE. 
 !
 if (.not.FG_is_sane) then
   call warning("[PHEL] Fine Grid does not match user requests. DbGd support switched off")
   PH_DbGd_points=-1
   l_PH_uses_E_FineGd=.FALSE.
   l_RT_uses_E_FineGd=.FALSE.
   return
 endif
 !
 N_FG_per_K_average=ceiling(float(PH_DbGd_points)/float(nk_CG))
 !
 ! Use subset 
 !
 PH_DbGd_points=0
 do ik=1,nk_CG
   k_FG%N_mapped(ik) =N_FG_per_K_average
   k_FG%k_range(ik,:)=k_FG%k_range(ik,1)+(/0,N_FG_per_K_average-1/)
   PH_DbGd_points=PH_DbGd_points+k_FG%N_mapped(ik)
 enddo
 !
 call msg('nrn','DbGd points used, read',(/PH_DbGd_points,k_FG%N/))   
 !
end subroutine PHEL_double_grid_subset
 
