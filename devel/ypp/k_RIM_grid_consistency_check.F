!
!        Copyright (C) 2000-2014 the YAMBO team
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
integer function k_RIM_grid_consistency_check(k,E,k_RIM,E_RIM)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use vec_operate,    ONLY:v_is_zero
 implicit none
 !
 type(bz_samp), intent(in)    :: k,k_RIM
 type(levels),  intent(in)    :: E
 type(levels),  intent(inout) :: E_RIM
 !
 ! Work Space
 !
 integer :: ik,ik_RIM,nb,nk
 real(SP):: shifts(2)
 !
 k_RIM_grid_consistency_check=-1
 !
 nb=min(E%nb,E_RIM%nb)
 !
 ik_loop: do ik=1,E%nk
   do ik_RIM=1,E_RIM%nk
     !
     if (v_is_zero(k_RIM%pt(ik_RIM,:)-k%pt(ik,:))) then
       !
       shifts(1)=E%E(1,ik,1)-E_RIM%E(1,ik_RIM,1)
       shifts(2)=E%E(nb,ik,1)-E_RIM%E(nb,ik_RIM,1)
       !
       exit  ik_loop
       !
     endif
     !
   enddo
 enddo ik_loop
 !
 if (abs(shifts(1)-shifts(2))>max(0.01*abs(shifts(1)),1.E-5)) then
   k_RIM_grid_consistency_check=-1
 else
   !
   k_RIM_grid_consistency_check=ik_RIM
   !
   ! Here I add E_Fermi as OCCUPATIONS_Fermi in ypp has been
   ! called without the RIM loaded so the Fermi level has not been applied to E_RIM.
   !
   E_RIM%E(:,:,:)=E_RIM%E(:,:,:)+(shifts(1)+shifts(2))/2.+E%E_Fermi
   !
 endif
 !
end function
