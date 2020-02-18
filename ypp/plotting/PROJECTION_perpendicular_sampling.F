! 
!        Copyright (C) 2000-2019 the YAMBO team
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
 !
 implicit none
 !
 type(bz_samp) :: K
 real(SP)      :: versor(3)
 !
 ! Work Space
 !
 integer :: i_k,i_k_line,i_k_versor,n_versor_ticks,Nk_found
 real(SP):: k_trial(3),k_trial_bz(3)
 !
 GRID_k%nbz=K%nbz*PROJECT_depth
 allocate(GRID_k%pt(GRID_k%nbz,3))
 !
 if (PROJECT_depth==1) then
   GRID_k%pt=K%pt
   return
 endif
 !
 LINE_LOOP: do i_k_line=1,K%nbz
   Nk_found=1
   i_k=(i_k_line-1)*PROJECT_depth+1
   GRID_k%pt(i_k,:)=K%pt(i_k_line,:)
   n_versor_ticks=7
   do while(Nk_found<PROJECT_depth) 
     n_versor_ticks=n_versor_ticks*2
     do i_k_versor=1,n_versor_ticks   
       k_trial=K%pt(i_k_line,:)+versor*(2.*float(i_k_versor+1)/float(n_versor_ticks)-1.)
       call k2bz(v_in=k_trial,v_out=k_trial_bz)
       if (.not.v_is_zero(k_trial-k_trial_bz,zero_=k_iku_zero)) cycle
       Nk_found=Nk_found+1
       i_k=(i_k_line-1)*PROJECT_depth+Nk_found
       GRID_k%pt(i_k,:)=k_trial
       if (Nk_found==PROJECT_depth) then
         cycle LINE_LOOP
       endif
     enddo
   enddo
 enddo LINE_LOOP
 !
end subroutine
