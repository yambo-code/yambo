! 
!        Copyright (C) 2000-2023 the YAMBO team
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
 use YPPm,           ONLY:PROJECT_N_perp,PROJECT_N_par
 use interpolate,    ONLY:GRID_k
 use com,            ONLY:msg
 use LIVE_t,         ONLY:live_timing
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset,ncpu
 use parallel_int,   ONLY:PP_wait,PP_redux_wait,PARALLEL_index
 !
 implicit none
 !
 type(bz_samp) :: K
 real(SP)      :: versor(3)
 !
 ! Work Space
 !
 integer :: i_k,i_k_line,i_k_versor,nK_out_of_BZ(1)
 real(SP):: k_trial(3),k_trial_bz(3),min_max_component(ncpu,2),component
 type(PP_indexes) :: px
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/K%nbz/))
 call PP_wait()
 !
 call live_timing('[PROJECT] Perpendicular sampling: borders',px%n_of_elements(myid+1))
 !
 ! Step#1: find the min/max distance between the plane and the BZ border
 !
 min_max_component          =0._SP
 min_max_component(myid+1,:)=(/1000._SP,-1._SP/)
 component=0._SP
 do i_k_line=1,K%nbz
   if (.not.px%element_1D(i_k_line)) cycle
   do i_k_versor=1,100   
     component=2.*float(i_k_versor+1)/100._SP-1.
     k_trial=K%pt(i_k_line,:)+versor*component
     call k2bz(v_in=k_trial,v_out=k_trial_bz)
     if (.not.v_is_zero(k_trial-k_trial_bz,zero_=k_iku_zero)) cycle
     if (component<min_max_component(myid+1,1)) min_max_component(myid+1,1)=component
     if (component>min_max_component(myid+1,2)) min_max_component(myid+1,2)=component
   enddo
   call live_timing(steps=1)
 enddo
 !
 call live_timing()
 !
 call PP_redux_wait(min_max_component)
 min_max_component(1,1)=minval(min_max_component(:,1))
 min_max_component(1,2)=maxval(min_max_component(:,2))
 !
 ! Step#2: Fill the projection
 !
 call live_timing('[PROJECT] Perpendicular sampling: filling',K%nbz)
 !
 GRID_k%nbz=K%nbz*PROJECT_N_perp
 allocate(GRID_k%pt(GRID_k%nbz,3))
 GRID_k%pt=0._SP
 !
 nK_out_of_BZ(1)=0
 do i_k_line=1,K%nbz
   if (.not.px%element_1D(i_k_line)) cycle
   i_k=(i_k_line-1)*PROJECT_N_perp+1
   GRID_k%pt(i_k,:)=K%pt(i_k_line,:)
   do i_k_versor=1,PROJECT_N_perp-1   
     component=min_max_component(1,1)+(min_max_component(1,2)-min_max_component(1,1))*i_k_versor/float(PROJECT_N_perp-1)
     k_trial=K%pt(i_k_line,:)+versor*component
     call k2bz(v_in=k_trial,v_out=k_trial_bz)
     if (.not.v_is_zero(k_trial-k_trial_bz,zero_=k_iku_zero)) nK_out_of_BZ(1)=nK_out_of_BZ(1)+1
     i_k=(i_k_line-1)*PROJECT_N_perp+i_k_versor
     GRID_k%pt(i_k,:)=k_trial
   enddo
   call live_timing(steps=1)
 enddo 
 !
 call live_timing()
 !
 call PP_redux_wait(GRID_k%pt)
 call PP_redux_wait(nK_out_of_BZ)
 !
 call msg("sr","[PROJECT] Direction  _|_ circuit       :",versor,"[iku]")
 call msg("sr","[PROJECT] Perpendicular extension      :",min_max_component(1,:),"[iku]")
 call msg("sr","[PROJECT] Points                [TOTAL]:",GRID_k%nbz)
 call msg("sr","[PROJECT]                         [___]:",PROJECT_N_par)
 call msg("sr","[PROJECT]                         [_|_]:",PROJECT_N_perp)
 if (nK_out_of_BZ(1)>0) then
   call msg("sr","[PROJECT] Points ouside the BZ         :",real(nK_out_of_BZ(1))/real(GRID_k%nbz)*100._SP,"[o/o]")
 endif
 !
end subroutine
