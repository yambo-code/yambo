!
! Copyright (C) 2000-2005 C. Hogan and the SELF team
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
subroutine e2s_kb_pp( ikibz, ncid )
  !
  ! Structure of ETSF library data:
  !   allocate( kb_formfactors_(    dims%max_number_of_coefficients,    &
  !   &                             dims%number_of_kpoints,            &
  !   &                             dims%max_number_of_projectors,      &
  !   &                             dims%max_number_of_angular_momenta, &
  !   &                             dims%number_of_atom_species ))
  !   allocate( kb_formfactor_derivative_( &
  !   &                             dims%max_number_of_coefficients,    &
  !   &                             dims%number_of_kpoints,            &
  !   &                             dims%max_number_of_projectors,      &
  !   &                             dims%max_number_of_angular_momenta, &
  !   &                             dims%number_of_atom_species ))
  ! Structure of SELF arrays, from PP_alloc:
  !   allocate( pp_kbs ( n_atom_species, pp_n_l_comp_max ),stat=alloc_err(1))
  !   allocate( pp_kb  ( wf_ng, n_atom_species, pp_n_l_comp_max ),stat=alloc_err(2))
  !   allocate( pp_kbd ( wf_ng, n_atom_species, pp_n_l_comp_max ),stat=alloc_err(3)
  !
  use etsf_io
  use etsf_io_low_level
  use etsf_data
  use D_lattice,            only : pp_n_l_comp_max,n_atom_species,pp_n_l_comp,&
&                                  n_atoms_max,pp_kbs,pp_kb,pp_kbd
  use wave_func,            only : wf_ncx,wf_nc_k
  implicit none
  integer,        intent(in)    :: ncid, ikibz
!ws
  integer                       :: ig, ib, ip, ia
  double precision              :: pp_kbs_(n_atom_species,pp_n_l_comp_max)
  double precision              :: pp_section(dims%max_number_of_coefficients, &
&           dims%max_number_of_angular_momenta, dims%number_of_atom_species )
!ETSF library stuff
  logical                       :: lstat      ! to get informed on error
  type(etsf_io_low_error)       :: error_data ! to store informations about errors
  integer                       :: start(6), count(6), ncvarid

  !
  ! Initialize the mapping: read for one k point (and one projector)
  !
  start(:) = 1     ; count(:) = 0
  start(2) = ikibz ; count(2) = 1   ! Split on k always
  !
  ! From Abinit: dims%max_number_of_projectors = 1 always
  !
  start(3) = 1     ; count(3) = 1   

  !
  ! Form factors (note order of indices)
  !
  call etsf_io_low_read_var(ncid, "kb_formfactors", &
                            & pp_section, lstat,          &
                            & error_data = error_data, start = start, count = count)
  forall( ig=1:wf_nc_k(ikibz), ia=1:n_atom_species, ip=1:pp_n_l_comp_max ) &
&          pp_kb(ig,ia,ip) = pp_section(ig,ip,ia)
  !
  ! Form factor derivatives (note order of indices)
  !
  call etsf_io_low_read_var(ncid, "kb_formfactor_derivative", &
                            & pp_section, lstat,          &
                            & error_data = error_data, start = start, count = count)
  forall( ig=1:wf_nc_k(ikibz), ia=1:n_atom_species, ip=1:pp_n_l_comp_max ) &
&          pp_kbd(ig,ia,ip) = pp_section(ig,ip,ia)
  !
  ! Form factor signs
  !
  call etsf_io_low_read_var(ncid, "kb_formfactor_sign", &
                            & pp_kbs_, lstat,          &
                            & error_data = error_data, start = start, count = count)
  forall( ia=1:n_atom_species, ip=1:pp_n_l_comp_max ) &
&          pp_kbs(ia,ip) = pp_kbs_(ia,ip)

  return
end subroutine e2s_kb_pp
