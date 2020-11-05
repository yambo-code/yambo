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
 use pars,    ONLY:SP
 use BS,      ONLY:l_BS_dichroism
 !
#include<memory.h>
 !
 complex(SP), allocatable :: BSS_dipoles_dich(:,:)
 !
 contains
   !
   subroutine BSS_DICH_alloc( )
     use pars,       ONLY:cZERO
     use BS,         ONLY:BS_dip_size
     use BS_solvers, ONLY:BSS_size
     if (.not.l_BS_dichroism) return
     YAMBO_ALLOC(BSS_dipoles_dich,(BS_dip_size,BSS_size))
     BSS_dipoles_dich = cZERO
   end subroutine
   !
   subroutine BSS_DICH_free( )
     if (.not.l_BS_dichroism) return
     YAMBO_FREE(BSS_dipoles_dich)
   end subroutine
   !
end module
