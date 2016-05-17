!
!        Copyright (C) 2000-2016 the YAMBO team
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
module PHOTOLUM
 !
 use drivers,    ONLY:l_photolum
 use pars,       ONLY:SP
 use memory_m,   ONLY:mem_est
 !
 implicit none
 !
 complex(SP), allocatable :: BSS_dipoles_PL(:,:)
 complex(SP), allocatable :: BSS_PL_f(:)
 !
 ! Photo Luminescence Spectrum
 !
 real(SP),    allocatable :: PL(:,:,:)
 !
 contains
   !
   subroutine BS_PL_alloc( )
     !
     use pars,       ONLY:cZERO,rZERO
     use BS,         ONLY:BS_H_dim
     !
     if (.not.l_photolum) return
     !
     allocate(BSS_dipoles_PL(BS_H_dim,3))
     call mem_est('BSS_dipoles_PL',(/size(BSS_dipoles_PL)/))
     BSS_dipoles_PL = cZERO
     !
     allocate(BSS_PL_f(BS_H_dim))
     call mem_est('BSS_PL_f',(/BS_H_dim/))
     BSS_PL_f = rZERO
     !
   end subroutine
   !
   subroutine BS_PL_free( )
     !
     if (.not.l_photolum) return
     !
     if (allocated(BSS_dipoles_PL)) then
       deallocate(BSS_dipoles_PL)
       call mem_est('BSS_dipoles_PL')
     endif
     if (allocated(BSS_PL_f)) then
       deallocate(BSS_PL_f)
       call mem_est('BSS_PL_f')
     endif
     !
   end subroutine
   !
end module PHOTOLUM
