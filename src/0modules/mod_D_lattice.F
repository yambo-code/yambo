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
 module D_lattice
!================
   use pars
!
! Temperature
!
   real(SP) :: Tel,default_Tel
!
! SYMs
!
   integer :: nsym,i_time_rev,i_space_inv
   real(SP),allocatable :: dl_sop(:,:,:)
   integer, allocatable :: sop_tab(:,:)
   integer, allocatable :: sop_inv(:)
!
! Cell
!
   integer  :: n_atoms_max
   integer  :: n_atom_species
   real(SP) :: DL_vol,a(3,3),alat(3)
   integer,  allocatable :: n_atoms(:)
   real(SP), allocatable :: atom_pos(:,:,:)
!
! KB Pseudo
!
   integer :: pp_n_l_comp_max
   integer :: pp_kbv_dim
   integer,  allocatable:: pp_n_l_comp(:)
   real(SP), allocatable:: pp_kbs(:,:)
   real(SP), allocatable:: pp_kb(:,:,:)
   real(SP), allocatable:: pp_kbd(:,:,:)

 contains

 subroutine PP_alloc()
 use memory_m,   ONLY:mem_est
 use wave_func,  ONLY:wf_ng
 integer  :: alloc_err(3)
 allocate(pp_kbs(n_atom_species,pp_n_l_comp_max),stat=alloc_err(1))
 allocate(pp_kb(wf_ng,n_atom_species,pp_n_l_comp_max),stat=alloc_err(2))
 allocate(pp_kbd(wf_ng,n_atom_species,pp_n_l_comp_max),stat=alloc_err(3))
 call mem_est("pp_kbs pp_kb pp_kbd",&
&             (/size(pp_kbs),size(pp_kb),size(pp_kbd)/),&
&             elements_kind=(/SP,SP,SP/),errors=alloc_err)
 end subroutine

 subroutine PP_free()
 use memory_m,   ONLY:mem_est
 if (.not.allocated(pp_kbs)) return
 deallocate(pp_kbs,pp_kb,pp_kbd)
 call mem_est("pp_kbs pp_kb pp_kbd")
 end subroutine

 end module D_lattice

