!
! Copyright (C) 2000-2005 A. Marini and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
 module ELPH
!===========
   use pars, ONLY:SP
   implicit none
   integer   :: ph_modes
   integer   :: elph_bands(2),QP_PH_n_G_bands
   real(SP),    allocatable :: ph_freqs_sq(:,:)
   complex(SP), allocatable :: elph_dV(:,:,:,:)
   complex(SP), allocatable :: dV_bare(:,:,:)
 contains

 subroutine elph_alloc()
 use memory_m,  ONLY:mem_est
 use R_lattice, ONLY:nqibz,nkbz,ng_vec
 implicit none
 integer     ::alloc_err(3)

 if (allocated(ph_freqs_sq)) return

 allocate(ph_freqs_sq(nqibz,ph_modes),stat=alloc_err(1))
 allocate(dV_bare(nqibz,ng_vec,ph_modes),stat=alloc_err(2))
 allocate(elph_dV(nkbz,ph_modes,elph_bands(2)-elph_bands(1)+1,&
&                 elph_bands(2)-elph_bands(1)+1),stat=alloc_err(3))
 call mem_est("ph_freqs_sq elph_dV dV_bare",&
&             (/size(ph_freqs_sq),size(dV_bare),size(elph_dV)/),&
&             (/SP, 2*SP,2*SP/),errors=alloc_err)
 end subroutine

 subroutine elph_free()
 use memory_m,  ONLY:mem_est
 implicit none
 deallocate(ph_freqs_sq,elph_dV)
 call mem_est("ph_freqs_sq elph_dV")
 end subroutine

 end module ELPH
