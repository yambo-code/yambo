!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, AM
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
logical function INTERPOLATION_vec_in_star(vec,nstar,star_vec)
 !
 use pars,         ONLY:SP
 use vec_operate,  ONLY:v_is_zero
 implicit none
 integer,  intent(in)  :: nstar
 real(SP), intent(in)  :: vec(3),star_vec(3,nstar)
 !
 ! Work Space
 !
 integer         :: i1
 !---------------------------------------------------------------------  
 INTERPOLATION_vec_in_star=.FALSE.
 do i1=1,nstar
   if(v_is_zero(star_vec(:,i1)-vec(:))) INTERPOLATION_vec_in_star=.TRUE.
 enddo
 !
end function INTERPOLATION_vec_in_star
