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
module COLL_interfaces
 !
 implicit none
 !
 interface
   !
   subroutine SCATTERING_GW_kinematics(i_q_bz,i_m,I,k,q,TABLE,E,N,G0,Ek,Ekmq,E_diff)
     use pars,                ONLY:SP
     use electrons,           ONLY:levels
     use R_lattice,           ONLY:bz_samp
     use QP_m,                ONLY:QP_table
     use collision_el,        ONLY:elemental_collision
     integer,       intent(in) ::i_q_bz,i_m
     integer,       intent(in) ::TABLE(:)
     type(elemental_collision) ::I
     type(bz_samp)             ::k,q
     type(levels),  OPTIONAL   ::E
     integer,       OPTIONAL   ::N,G0
     real(SP),      OPTIONAL   ::Ek,Ekmq,E_diff
   end subroutine
   !
   integer function io_COLLISIONS(ID,COLL_grp,COLL_element,i_coll)
     use collision_ext,  ONLY:COLLISIONS_element,COLLISIONS_group
     integer, intent(inout)             ::ID
     type(COLLISIONS_group)             ::COLL_grp
     type(COLLISIONS_element), optional ::COLL_element
     integer, intent(in)     , optional ::i_coll
   end function
   !
 end interface
 !
end module
