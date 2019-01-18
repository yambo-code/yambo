!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH
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
subroutine e2y_kb_pp( ikibz, ncid )
  !
  ! Structure of ETSF library data: (fortran order, opp to netcdf order)
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
  ! Structure of YAMBO arrays, from PP_alloc:
  !   allocate( pp_kbs ( n_atom_species, pp_n_l_comp_max ),stat=alloc_err(1))
  !   allocate( pp_kb  ( ng_vec, n_atom_species, pp_n_l_comp_max ),stat=alloc_err(2))
  !   allocate( pp_kbd ( ng_vec, n_atom_species, pp_n_l_comp_max ),stat=alloc_err(3)
  !
  use etsf_io
  use etsf_io_low_level
  use etsf_data
  use D_lattice,            only : n_atomic_species,n_atoms_species_max
  use pseudo,               only : pp_n_l_times_proj_max,pp_n_l_comp,&
&                                  pp_kbs,pp_kb,pp_kbd
  use wave_func,            only : wf_ncx,wf_nc_k
  use mod_com2y,            only : ng_vec_abinit
  !
  implicit none
  !
  integer,        intent(in)    :: ncid, ikibz
  ! 
  ! Work Space
  !
  integer                       :: ig, ib, ip, ia
  integer                       :: pp_kbs_( &
&           dims%max_number_of_angular_momenta,  dims%number_of_atom_species )
  double precision              :: pp_section(dims%max_number_of_coefficients, &
&           dims%max_number_of_angular_momenta, dims%number_of_atom_species )
!ETSF library stuff
  logical                       :: lstat      ! to get informed on error
  type(etsf_io_low_error)       :: error_data ! to store informations about errors
  integer                       :: start(5), count(5), ncvarid ! must be size of netcdf array!

  !
  ! Initialize the mapping: read for one k point (and one projector)
  ! k is the *second* dimension of the netcdf variable in *Fortran* order
  ! From Abinit: dims%max_number_of_projectors = 1 always
  !
  start(:) = 1     ; count(:) = 0
  start(2) = ikibz ; count(2) = 1   ! Split on k always
  !
  ! Form factors 
  !
  call etsf_io_low_read_var(ncid, "kb_formfactors", &
                            & pp_section, lstat, error_data = error_data, &
&                             start = start, count =  count )
  forall( ig=1:ng_vec_abinit, ia=1:n_atomic_species, ip=1:pp_n_l_times_proj_max ) &
&          pp_kb(ig,ia,ip,1) = pp_section(ig,ip,ia)
  !
  ! Form factor derivatives 
  !
  call etsf_io_low_read_var(ncid, "kb_formfactor_derivative", &
                            & pp_section, lstat, error_data = error_data, &
&                             start = start, count = count)
  forall( ig=1:ng_vec_abinit, ia=1:n_atomic_species, ip=1:pp_n_l_times_proj_max ) &
&          pp_kbd(ig,ia,ip,1) = pp_section(ig,ip,ia)
  !
  ! Form factor signs
  !
  if(ikibz.gt.1) return
  call etsf_io_low_read_var(ncid, "kb_formfactor_sign", &
                            & pp_kbs_, lstat,          &
                            & error_data = error_data)

  forall( ia=1:n_atomic_species, ip=1:pp_n_l_times_proj_max ) &
&          pp_kbs(ia,ip) = pp_kbs_(ip,ia)

  return
end subroutine e2y_kb_pp
