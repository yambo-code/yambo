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
subroutine PROJECTION_perpendicular_sampling( K, versor )
 !
 use zeros,          ONLY:k_iku_zero
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use vec_operate,    ONLY:k2bz,v_is_zero
 use YPPm,           ONLY:PROJECT_depth
 use interpolate,    ONLY:GRID_k
 use com,            ONLY:msg
 !
 implicit none
 !
 type(bz_samp) :: K
 real(SP)      :: versor(3)
 !
 ! Work Space
 !
 integer :: i_k,i_k_line,i_k_versor,nK_out_of_BZ
 real(SP):: k_trial(3),k_trial_bz(3),min_max_component(2),component
 !
 GRID_k%nbz=K%nbz*PROJECT_depth
 allocate(GRID_k%pt(GRID_k%nbz,3))
 !
 if (PROJECT_depth==1) then
   GRID_k%pt=K%pt
   return
 endif
 !
 ! Step#1: find the min/max distance between the plane and the BZ border
 !
 min_max_component=(/1000.,-1./)
 do i_k_line=1,K%nbz
   do i_k_versor=1,100   
     component=2.*float(i_k_versor+1)/100.-1.
     k_trial=K%pt(i_k_line,:)+versor*component
     call k2bz(v_in=k_trial,v_out=k_trial_bz)
     if (.not.v_is_zero(k_trial-k_trial_bz,zero_=k_iku_zero)) cycle
     if (component<min_max_component(1)) min_max_component(1)=component
     if (component>min_max_component(2)) min_max_component(2)=component
   enddo
 enddo
 !
 ! Step#2: Fill the projection
 !
 nK_out_of_BZ=0
 do i_k_line=1,K%nbz
   i_k=(i_k_line-1)*PROJECT_depth+1
   GRID_k%pt(i_k,:)=K%pt(i_k_line,:)
   do i_k_versor=1,PROJECT_depth-1   
     component=min_max_component(1)+(min_max_component(2)-min_max_component(1))*i_k_versor/float(PROJECT_depth-1)
     k_trial=K%pt(i_k_line,:)+versor*component
     call k2bz(v_in=k_trial,v_out=k_trial_bz)
     if (.not.v_is_zero(k_trial-k_trial_bz,zero_=k_iku_zero)) nK_out_of_BZ=nK_out_of_BZ+1
     i_k=(i_k_line-1)*PROJECT_depth+i_k_versor
     GRID_k%pt(i_k,:)=k_trial
   enddo
 enddo 
 !
 call msg("sr","[PROJECT] Direction  _|_ circuit  [iku]:",versor)
 call msg("sr","[PROJECT] Perpendicular extension [iku]:",min_max_component)
 call msg("sr","[PROJECT] Total Points                 :",GRID_k%nbz)
 if (nK_out_of_BZ>0) call msg("sr","[PROJECT] Points out of BZ        [o/o]:",real(nK_out_of_BZ)/real(GRID_k%nbz)*100.)
 !
end subroutine
