
  integer function e2s_i(instr,lnstr,inf,iinf,ind,iind,od,iod,&
&                       js,ijs,np,pid)

  use etsf_io
  use etsf_io_low_level

 integer,          intent(in) :: lnstr,iind,iod,ijs,np,pid
 integer,          intent(in) :: iinf
 character(lnstr), intent(in) :: instr
 character(iinf),  intent(in) :: inf
 character(iind),  intent(in) :: ind
 character(iod),   intent(in) :: od
 character(ijs),   intent(in) :: js

  ! The main program memory
  ! Creating a silane molecule with a 10x10x10 mesh for the density
  double precision, target    :: rhor(1000)
  double precision, target    :: rprimd(3, 3)
  integer, target             :: reduced_symmetry_matrices(3 ,3 ,1)
  double precision, target    :: reduced_symmetry_translations(3, 1)
  double precision, target    :: reduced_atom_positions(3, 5)
  double precision, target    :: valence_charges(2)
  double precision, target    :: atomic_numbers(2)
  character(len = 80), target :: atom_species_names(2)
  character(len = 2), target  :: chemical_symbols(2)
  character(len = 80), target :: pseudopotential_types(2)
  integer, target             :: ntypat(5)
  integer, target             :: space_group => 1

  ! The local variable to handle ETSF data.
  type(etsf_dims)             :: dims ! The dimensions of our system
  type(etsf_geometry), target :: geometry ! the description of our system
  type(etsf_main)             :: main ! a container (will store the density)
  type(etsf_groups)           :: groups ! a container (will store the geometry)
  logical                     :: lstat ! to get informed on error
  type(etsf_io_low_error)     :: error ! to store informations about errors

  integer :: i, dummy

  ! The main program make the calculation and create the geometry
  ! and the density.
  rprimd = reshape( (/ 10, 0, 0, 0, 10, 0, 0, 0, 10 /), (/ 3, 3 /))
  reduced_symmetry_matrices = reshape( (/ 1, 0, 0, 0, 1, 0, 0, 0, 1 /), (/ 3, 3, 1 /))
  reduced_symmetry_translations = reshape( (/ 0, 0, 0 /), (/ 3, 1 /))
  reduced_atom_positions = reshape( (/ 0.5d0, 0.5d0, 0.5d0, &
                                     & 0.6d0, 0.6d0, 0.6d0, &
                                     & 0.6d0, 0.4d0, 0.4d0, &
                                     & 0.4d0, 0.4d0, 0.6d0, &
                                     & 0.4d0, 0.6d0, 0.4d0 /), (/ 3, 5 /))
  valence_charges = (/ 1.d0, 4.d0 /)
  atomic_numbers = (/ 1, 14 /)
  ntypat = (/ 2, 1, 1, 1, 1 /)
  write(atom_species_names(1), "(A)") "Helium"
  write(atom_species_names(2), "(A)") "Silicium"
  chemical_symbols = (/ "He", "Si" /)
  write(pseudopotential_types(1), "(A)") "Helium GTH"
  write(pseudopotential_types(2), "(A)") "Silicium GTH"
  ! We compute the density with a powerful algorithm.
  rhor = (/ (i, i = 1, 1000) /)



  ! We begin the ETSF write action
  ! ==============================
  dims%number_of_atoms = 5
  dims%number_of_atom_species = 2
  dims%number_of_components = 1
  dims%number_of_grid_points_vector1 = 10
  dims%number_of_grid_points_vector2 = 10
  dims%number_of_grid_points_vector3 = 10

  ! We create the file (should not exist).
  call etsf_io_data_init("etsf_demo.nc", etsf_main_density, etsf_grp_geometry, &
    & dims, "Silane molecule with its density", "Initial creation", lstat, error)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error)
    stop
  end if

  ! We associate our geometry
! geometry%space_group = 1
  geometry%space_group => space_group
  geometry%primitive_vectors => rprimd
  geometry%reduced_symmetry_matrices => reduced_symmetry_matrices
  geometry%reduced_symmetry_translations => reduced_symmetry_translations
  geometry%atom_species => ntypat
  geometry%reduced_atom_positions => reduced_atom_positions
  geometry%valence_charges => valence_charges
  geometry%atomic_numbers => atomic_numbers
  geometry%atom_species_names => atom_species_names
  geometry%chemical_symbols => chemical_symbols
  geometry%pseudopotential_types => pseudopotential_types
  groups%geometry => geometry

  ! We don't want to dupplicate the density data even if ours is 1D
  ! and ETSF is 5D, so we use the unformatted pointer in the etsf_main
  ! structure.
  main%density%data1D => rhor

  ! We write.
  call etsf_io_data_write("etsf_demo.nc", etsf_main_density, etsf_grp_geometry, main, groups, lstat, error)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error)
    stop
  end if

end function

