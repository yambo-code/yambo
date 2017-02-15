!
!        Copyright (C) 2000-2017 the YAMBO team
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
integer function k_DbGd_grid_consistency_check(k,E,k_DbGd,E_DbGd)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use vec_operate,    ONLY:v_is_zero
 implicit none
 !
 type(bz_samp), intent(in)    :: k,k_DbGd
 type(levels),  intent(in)    :: E
 type(levels),  intent(inout) :: E_DbGd
 !
 ! Work Space
 !
 integer :: ik,ik_DbGd,nb
 real(SP):: shifts(2)
 !
 k_DbGd_grid_consistency_check=-1
 !
 nb=min(E%nb,E_DbGd%nb)
 !
 ik_loop: do ik=1,E%nk
   do ik_DbGd=1,E_DbGd%nk
     !
     if (v_is_zero(k_DbGd%pt(ik_DbGd,:)-k%pt(ik,:))) then
       !
       shifts(1)=E%E(1,ik,1)-E_DbGd%E(1,ik_DbGd,1)
       shifts(2)=E%E(nb,ik,1)-E_DbGd%E(nb,ik_DbGd,1)
       !
       exit  ik_loop
       !
     endif
     !
   enddo
 enddo ik_loop
 !
 if (abs(shifts(1)-shifts(2))>max(0.01*abs(shifts(1)),1.E-5) .or. ik_DbGd==E_DbGd%nk+1 ) return
 !
 k_DbGd_grid_consistency_check=ik_DbGd
 !
 ! Here I add E_Fermi as OCCUPATIONS_Fermi in ypp has been
 ! called without the DbGd loaded so the Fermi level has not been applied to E_DbGd.
 !
 E_DbGd%E(:,:,:)=E_DbGd%E(:,:,:)+(shifts(1)+shifts(2))/2.+E%E_Fermi
 !
end function
