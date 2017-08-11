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
module ELPH_old
 !
 use pars,       ONLY:SP
 !
#include<memory.h>
 !
 real(SP),    allocatable :: elph_DW_old(:,:,:,:)
 complex(SP), allocatable :: elph_gkkp_old(:,:,:,:)
 ! 
 contains
   !
   subroutine elph_global_alloc_old(what)
     use R_lattice, ONLY:nkbz
     use ELPH,      ONLY:ph_modes,elph_nb
     character(*)      ::what
     integer           ::alloc_err
     !
     select case (trim(what))
       !
       case ('gkkp')
         !
         if (.not.allocated(elph_gkkp_old)) then
           YAMBO_ALLOC(elph_gkkp_old,(nkbz,ph_modes,elph_nb,elph_nb))
           YAMBO_ALLOC(elph_DW_old,(nkbz,ph_modes,elph_nb,elph_nb))
         endif
         !
     end select
     !
   end subroutine
   !
   subroutine elph_global_free_old()
     if (allocated(elph_gkkp_old)) then
       YAMBO_FREE(elph_gkkp_old)
       YAMBO_FREE(elph_DW_old)
     endif
   end subroutine
   !
end module ELPH_old
