!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
   integer function io_COLLISIONS(ID,COLL_grp,COLL_element,i_coll,COMM)
     use collision_ext,  ONLY:COLLISIONS_element,COLLISIONS_group
     integer, intent(inout)             ::ID
     type(COLLISIONS_group)             ::COLL_grp
     type(COLLISIONS_element), optional ::COLL_element
     integer, intent(in)     , optional ::i_coll,COMM
   end function
   !
 end interface
 !
end module
