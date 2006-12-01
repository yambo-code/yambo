module etsf_data
  !
  ! Local copy of targets for reading etsf_io libs.
  ! Variables are (1) targets: which might hurt performance inside main code,
  !               (2) double precision: since SELF uses SP in some calculations.
  ! Hence, local copies are generally unavoidable.
  ! ETSF specs for float/double may be modified in future for wfcs, but
  ! this is not critical.
  !
  use etsf_io,            only : etsf_charlen, etsf_io_splitted_var_double, &
&                                etsf_io_splitted_var_integer, etsf_dims
  ! Use the ETSF names here, with an underscore.
  ! Do the 1-to-1 association inside the code, not here
  type(etsf_dims) :: dims
  !
  ! Geometry group
  !
  double precision, allocatable, target :: primitive_vectors_(:,:)
  integer,          allocatable, target :: reduced_symmetry_matrices_(:,:,:)
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
! type(etsf_io_splitted_var_double), target :: eigenvalues_
! type(etsf_io_splitted_var_double), target :: occupations_
  !
  ! K-points
  !
  double precision, allocatable, target :: reduced_coordinates_of_kpoints_(:,:)
  !
  ! Wave function data (basis data)
  !
  character(len=etsf_charlen),   target :: basis_set_ 
  integer,          allocatable, target :: number_of_coefficients_(:) 
! type(etsf_io_splitted_var_integer) :: reduced_coordinates_of_plane_waves_
  integer,          allocatable, target :: reduced_coordinates_of_plane_waves_(:,:,:)
 ! coefficients_of_wavefunctions 
  !
  ! Wave function coefficients
  !
  double precision, allocatable, target :: coefficients_of_wavefunctions_(:,:,:,:,:,:)
  !
  ! Miscellaneous data
  !
  double precision :: temperature

end module etsf_data

