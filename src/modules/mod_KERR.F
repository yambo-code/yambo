!
!        Copyright (C) 2000-2017 the YAMBO team
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
module KERR
 !
 use drivers, ONLY:l_kerr
 use pars,    ONLY:SP
#include<memory.h>
 !
 complex(SP)              :: A_drude(2)
 complex(SP)              :: B_Hall(2)
 complex(SP), allocatable :: BSS_dipoles_kerr(:)
 complex(SP), allocatable :: DIP_projected_r90(:,:,:,:)
 !
 contains
   !
   subroutine KERR_alloc(what,d)
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
       case('DIP_projected')
         if (allocated(DIP_projected_r90)) return
         YAMBO_ALLOC(DIP_projected_r90,(d(1):d(2),d(3):d(4),d(5),n_sp_pol))
       end select
     else
       select case(what)
       case('DIP_projected')
         YAMBO_FREE(DIP_projected_r90)
       end select
     endif
   end subroutine
   !
   !
   ! GPL_EXCLUDE_START
   !
   subroutine BSS_KERR_alloc( )
     use pars,       ONLY:cZERO
     use BS,         ONLY:BS_H_dim,BS_K_dim
     if (.not.l_kerr) return
     YAMBO_ALLOC(BSS_dipoles_kerr,(BS_H_dim))
     BSS_dipoles_kerr = cZERO
   end subroutine
   !
   subroutine BSS_KERR_free( )
     if (.not.l_kerr) return
     YAMBO_FREE(BSS_dipoles_kerr)
   end subroutine
   !
   ! GPL_EXCLUDE_END
   !
end module
