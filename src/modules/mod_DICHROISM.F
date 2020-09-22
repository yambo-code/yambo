!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
module DICHROISM
 !
 use drivers, ONLY:l_dichroism
 use pars,    ONLY:SP
 use BS,      ONLY:BS_dip_size
 !
#include<memory.h>
 !
 complex(SP), allocatable :: BSS_dipoles_dichroism(:,:)
 !
 contains
   !
   subroutine BSS_DICHROISM_alloc( )
     use pars,       ONLY:cZERO
     use BS,         ONLY:BS_H_dim,BS_K_dim
     if (.not.l_dichroism) return
     YAMBO_ALLOC(BSS_dipoles_dichroism,(BS_dip_size,BS_H_dim))
     BSS_dipoles_dichroism = cZERO
   end subroutine
   !
   subroutine BSS_DICHROISM_free( )
     if (.not.l_dichroism) return
     YAMBO_FREE(BSS_dipoles_dichroism)
   end subroutine
   !
end module
