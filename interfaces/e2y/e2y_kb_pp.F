!
!        Copyright (C) 2000-2018 the YAMBO team
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
  !
  ! Structure of ETSF library data: (fortran order, opp to netcdf order)
  !   kb_formfactors(  dims%max_number_of_coefficients,    &
  !   &                dims%number_of_kpoints,             &
  !   &                dims%max_number_of_projectors,      &
  !   &                dims%max_number_of_angular_momenta, &
  !   &                dims%number_of_atom_species )
  !   kb_formfactor_derivative( &
  !   &                dims%max_number_of_coefficients,    &
  !   &                dims%number_of_kpoints,             &
  !   &                dims%max_number_of_projectors,      &
  !   &                dims%max_number_of_angular_momenta, &
  !   &                dims%number_of_atom_species )
  !   kb_formfactor_sign( &
  !   &                dims%max_number_of_projectors,      &
  !   &                dims%max_number_of_angular_momenta, &
  !   &                dims%number_of_atom_species )
  ! Structure of YAMBO arrays, from PP_alloc:
  !   pp_kbs ( n_atom_species, pp_n_l_comp_max )
  !   pp_kb  ( ng_vec, n_atom_species, pp_n_l_comp_max )
  !   pp_kbd ( ng_vec, n_atom_species, pp_n_l_comp_max )
  !
  use netcdf
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
  integer                       :: pp_kbs_( pp_n_l_times_proj_max,  n_atomic_species )
  double precision              :: pp_section(wf_ncx, pp_n_l_times_proj_max, n_atomic_species )
  integer                       :: start(5), count(5), varid, netcdf_error
  !
  ! Form factors 
  !
  start = [1,ikibz,1,1,1]
  count = [wf_ncx,1,pp_n_l_times_proj_max,max_number_of_angular_momenta_,n_atomic_species]
  !
  ! Form factors 
  !
  netcdf_error = nf90_inq_varid(ncid, "kb_formfactors", varid)
  netcdf_error = nf90_get_var(ncid, varid, pp_section)
  forall( ig=1:ng_vec_abinit, ia=1:n_atomic_species, ip=1:pp_n_l_times_proj_max ) &
&          pp_kb(ig,ia,ip,1) = pp_section(ig,ip,ia)

  !
  ! Form factor derivatives 
  !
  netcdf_error = nf90_inq_varid(ncid, "kb_formfactors_derivative", varid)
  netcdf_error = nf90_get_var(ncid, varid, pp_section)
  forall( ig=1:ng_vec_abinit, ia=1:n_atomic_species, ip=1:pp_n_l_times_proj_max ) &
&          pp_kbd(ig,ia,ip,1) = pp_section(ig,ip,ia)

  !
  ! Form factor signs
  !
  if(ikibz.gt.1) return
  netcdf_error = nf90_inq_varid(ncid, "kb_formfactors_sign", varid)
  netcdf_error = nf90_get_var(ncid, varid, pp_kbs)
  forall( ia=1:n_atomic_species, ip=1:pp_n_l_times_proj_max ) &
&          pp_kbs(ia,ip) = pp_kbs_(ip,ia)

  return
end subroutine e2y_kb_pp
