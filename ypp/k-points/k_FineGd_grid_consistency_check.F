!
!        Copyright (C) 2000-2018 the YAMBO team
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
integer function k_FineGd_grid_consistency_check(k,E,k_FineGd,E_FineGd)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use vec_operate,    ONLY:v_is_zero
 implicit none
 !
 type(bz_samp), intent(in)    :: k,k_FineGd
 type(levels),  intent(in)    :: E
 type(levels),  intent(inout) :: E_FineGd
 !
 ! Work Space
 !
 integer :: ik,ik_FineGd,nb
 real(SP):: shifts(2)
 !
 k_FineGd_grid_consistency_check=-1
 !
 nb=min(E%nb,E_FineGd%nb)
 !
 ik_loop: do ik=1,E%nk
   do ik_FineGd=1,E_FineGd%nk
     !
     if (v_is_zero(k_FineGd%pt(ik_FineGd,:)-k%pt(ik,:))) then
       !
       shifts(1)=E%E(1,ik,1)-E_FineGd%E(1,ik_FineGd,1)
       shifts(2)=E%E(nb,ik,1)-E_FineGd%E(nb,ik_FineGd,1)
       !
       exit  ik_loop
       !
     endif
     !
   enddo
 enddo ik_loop
 !
 if (abs(shifts(1)-shifts(2))>max(0.01*abs(shifts(1)),1.E-5) .or. ik_FineGd==E_FineGd%nk+1 ) return
 !
 k_FineGd_grid_consistency_check=ik_FineGd
 !
 ! Here I add E_Fermi as OCCUPATIONS_Fermi in ypp has been
 ! called without the DbGd loaded so the Fermi level has not been applied to E_FineGd.
 !
 E_FineGd%E(:,:,:)=E_FineGd%E(:,:,:)+(shifts(1)+shifts(2))/2._SP+E%E_Fermi
 !
end function
