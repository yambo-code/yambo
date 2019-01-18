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
module MAGNONS
 !
 use drivers, ONLY:l_magnons
 use pars,    ONLY:SP
 !
#include<memory.h>
 !
 complex(SP), allocatable :: BSS_dipoles_magnons(:,:)
 complex(SP), allocatable :: DIP_Splus(:,:,:,:)
 complex(SP), allocatable :: DIP_Smins(:,:,:,:)
 !
 contains
   !
   subroutine MAGNONS_alloc(what,d)
     !
     use electrons, ONLY:n_sp_pol
     character(*)      :: what
     integer, optional :: d(:)
     !
     ! Work Space
     !
     integer :: err
     !
     if (present(d)) then
       select case(what)
       case('DIP_S')
         if (allocated(DIP_Splus)) return
         YAMBO_ALLOC(DIP_Splus,(d(1):d(2),d(3):d(4),d(5),n_sp_pol))
         YAMBO_ALLOC(DIP_Smins,(d(1):d(2),d(3):d(4),d(5),n_sp_pol))
       end select
     else
       select case(what)
       case('DIP_S')
         YAMBO_FREE(DIP_Splus)
         YAMBO_FREE(DIP_Smins)
       end select
     endif
   end subroutine
   !
   subroutine BSS_MAGNONS_alloc( )
     use pars,       ONLY:cZERO
     use BS,         ONLY:BS_H_dim,BS_K_dim
     if (.not.l_magnons) return
     YAMBO_ALLOC(BSS_dipoles_magnons,(2,BS_H_dim))
     BSS_dipoles_magnons = cZERO
   end subroutine
   !
   subroutine BSS_MAGNONS_free( )
     if (.not.l_magnons) return
     YAMBO_FREE(BSS_dipoles_magnons)
   end subroutine
   !
end module
