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
module etsf_data
  !
  ! Compatible with version 0.8 of ETSF_io
  ! Local copy of targets for reading *higher* level etsf_io libs.
  ! Variables are (1) targets: which might hurt performance inside main code,
  !               (2) double precision: since YAMBO uses SP in some calculations.
  ! Hence, local copies are generally unavoidable.
  ! ETSF specs for float/double may be modified in future for wfcs, but
  ! this is not critical.
  ! Variable names are those of ETSF specs, with an appended underscore.
  !
  ! To do: add ierr after allocates
  !
  use etsf_io,            only : etsf_charlen, etsf_dims, etsf_chemlen
  !
  ! Dimensions, as defined by the ETSF specifications
  !
  type(etsf_dims), save :: dims
  !
  ! 
  ! ETSF kind can be KSS or WFK at the moment
  character(3) :: ETSF_kind
  !
  ! Geometry group
  !
  integer,          allocatable, target :: atom_species_(:)
  double precision, allocatable, target :: reduced_atom_positions_(:,:)
  double precision, allocatable, target :: valence_charges_(:) 
  !
  ! Electrons
  !
  integer,                       target :: number_of_electrons_ 
  character(len=etsf_charlen),   target :: exchange_functional_ 
  character(len=etsf_charlen),   target :: correlation_functional_ 
  integer,          allocatable, target :: number_of_states_(:)
  double precision, allocatable, target :: eigenvalues_(:,:,:)
  !
  ! K-points
  !
  double precision, allocatable, target :: reduced_coordinates_of_kpoints_(:,:)
  !
  ! Wave function data (basis data)
  !
  character(len=etsf_charlen),   target :: basis_set_ 
  integer,          allocatable, target :: reduced_coordinates_of_plane_waves_(:,:,:)
  !
  ! Wave function coefficients
  !
  double precision, allocatable, target :: coefficients_of_wavefunctions_(:,:,:,:,:,:)
  !
  ! GWdata (KB PP)
  !
  integer                               :: max_number_of_projectors_
  integer                               :: max_number_of_angular_momenta_
  double precision, allocatable, target :: kb_formfactor_derivative_(:,:,:,:,:)
  double precision, allocatable, target :: kb_formfactors_(:,:,:,:,:)
  integer, allocatable, target          :: kb_formfactor_sign_(:,:,:)
  !
  ! Miscellaneous data, not in ETSF specs, that might appear in ETSF file.
  !
  double precision                      :: temperature_
  integer                               :: ixc_
  integer,          allocatable, target :: spin_orbit_atom_(:)
  !
end module etsf_data
