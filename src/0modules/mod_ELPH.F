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
   integer   :: elph_nb
   integer   :: QP_PH_n_G_bands
   integer   :: dV_ngx
   integer   :: dV_nk
   real(SP),    allocatable :: ph_freqs_sq(:,:)
   complex(SP), allocatable :: elph_gkkp(:,:,:,:)
   complex(SP), allocatable :: elph_dV_psi (:,:,:)
   complex(SP), allocatable :: elph_dV(:,:,:,:)
 contains

 subroutine elph_gkkp_alloc()
!----------------------------
 use memory_m,  ONLY:mem_est
 use R_lattice, ONLY:nqibz,nkbz
 implicit none
 integer     ::alloc_err(2)
 if (allocated(ph_freqs_sq)) return
 allocate(ph_freqs_sq(nqibz,ph_modes),stat=alloc_err(1))
 allocate(elph_gkkp(nkbz,ph_modes,elph_nb,elph_nb),stat=alloc_err(2))
 call mem_est("ph_freqs_sq elph_gkkp",&
&             (/size(ph_freqs_sq),size(elph_gkkp)/),&
&             (/SP, 2*SP/),errors=alloc_err)
 end subroutine

 subroutine elph_gkkp_free()
!---------------------------
 use memory_m,  ONLY:mem_est
 implicit none
 deallocate(ph_freqs_sq,elph_gkkp)
 call mem_est("ph_freqs_sq elph_gkkp")
 end subroutine

 subroutine elph_dV_alloc()
!--------------------------
 use memory_m,  ONLY:mem_est
 implicit none
 integer     ::alloc_err(2)

 allocate(elph_dV(dV_ngx,elph_nb,dV_nk,ph_modes),stat=alloc_err(1))
 allocate(elph_dV_psi(dV_ngx,elph_nb,dV_nk),stat=alloc_err(2))
 call mem_est("elph_dV elph_dV_psi",&
&             (/size(elph_dV),size(elph_dV_psi)/),&
&             (/2*SP,2*SP/),errors=alloc_err)
 end subroutine

 subroutine elph_dV_free()
!-------------------------
 use memory_m,  ONLY:mem_est
 implicit none
 deallocate(elph_dV,elph_dV_psi)
 call mem_est("elph_dV elph_dV_psi")
 end subroutine


 end module ELPH
