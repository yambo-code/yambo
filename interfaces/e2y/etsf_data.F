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
  double precision, allocatable, target :: primitive_vectors_(:,:)
  integer,          allocatable, target :: atom_species_(:)
  double precision, allocatable, target :: reduced_atom_positions_(:,:)
  double precision, allocatable, target :: valence_charges_(:) 
  double precision, allocatable, target :: atomic_numbers_(:) 
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
  integer,          allocatable, target :: number_of_coefficients_(:) 
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

contains
  !
  ! geometry group
  !
  subroutine allocate_geometry_group
    allocate( atomic_numbers_(dims%number_of_atom_species) )
    allocate( atom_species_(dims%number_of_atoms) )
    allocate( primitive_vectors_ (dims%number_of_cartesian_directions, &
&                                 dims%number_of_vectors ) )
    allocate( reduced_atom_positions_(dims%number_of_reduced_dimensions, &
&                                     dims%number_of_atoms ) )
    return
  end subroutine allocate_geometry_group
  
  subroutine deallocate_geometry_group
    if(allocated(atom_species_))              deallocate(atom_species_)
    if(allocated(atomic_numbers_))            deallocate(atomic_numbers_)
    if(allocated(primitive_vectors_))         deallocate(primitive_vectors_)
    if(allocated(reduced_atom_positions_))    deallocate(reduced_atom_positions_)
    return 
  end subroutine deallocate_geometry_group
  !
  ! electrons group
  !
  subroutine allocate_electrons_group
    allocate( eigenvalues_(dims%max_number_of_states, &
&                          dims%number_of_kpoints,    &
&                          dims%number_of_spins ) )
    return
  end subroutine allocate_electrons_group

  subroutine deallocate_electrons_group
    if(allocated(eigenvalues_)) deallocate( eigenvalues_)
    return
  end subroutine deallocate_electrons_group
  !
  ! gwdata group: take care with max_number_of_projectors, etc.
  !
  subroutine allocate_gwdata_group
    allocate( kb_formfactor_sign_( dims%max_number_of_projectors,      &
&                                  dims%max_number_of_angular_momenta, &
&                                  dims%number_of_atom_species ))
    allocate( kb_formfactors_(  dims%max_number_of_coefficients,    &
&                               dims%number_of_kpoints,            &
&                               dims%max_number_of_projectors,      &
&                               dims%max_number_of_angular_momenta, &
&                               dims%number_of_atom_species ))
    allocate( kb_formfactor_derivative_( &
&                               dims%max_number_of_coefficients,    &
&                               dims%number_of_kpoints,            &
&                               dims%max_number_of_projectors,      &
&                               dims%max_number_of_angular_momenta, &
&                               dims%number_of_atom_species ))
    return
  end subroutine allocate_gwdata_group
  !
  subroutine deallocate_gwdata_group
    if(allocated( kb_formfactor_sign_ ))        deallocate( kb_formfactor_sign_ )
    if(allocated( kb_formfactors_ ))            deallocate( kb_formfactors_ )
    if(allocated( kb_formfactor_derivative_ ))  deallocate( kb_formfactor_derivative_ )
    return
  end subroutine deallocate_gwdata_group
  !
  subroutine allocate_basisdata_group
    allocate(reduced_coordinates_of_plane_waves_(dims%number_of_reduced_dimensions, &
&                                                dims%max_number_of_coefficients, &
&                                                dims%number_of_kpoints ))
    allocate(number_of_coefficients_(dims%number_of_kpoints))
    return
  end subroutine allocate_basisdata_group
  !
  ! basisdata group
  !
  subroutine deallocate_basisdata_group
    if(allocated(reduced_coordinates_of_plane_waves_)) &
&                deallocate( reduced_coordinates_of_plane_waves_)
    if(allocated(number_of_coefficients_)) &
&                deallocate( number_of_coefficients_ )
    return
  end subroutine deallocate_basisdata_group
  !
end module etsf_data
