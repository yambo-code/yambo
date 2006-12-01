subroutine e2s_db1(en,k,ETSF_file_name,ncid)
  use etsf_io
  use etsf_io_low_level
  use etsf_data
  use pars,                 only : SP, DP, PI, schlen
  use com,                  only : msg
  use electrons,            only : levels, default_nel,n_bands,n_spin,n_sp_pol,n_spinor
  use R_lattice,            only : bz_samp, ng_vec, g_vec,nkibz
  use D_lattice,            only : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                  DL_vol, a, alat, default_Tel,&
&                                  pp_n_l_comp_max,n_atom_species,n_atoms,pp_n_l_comp,&
&                                  n_atoms_max,atom_pos
  use wave_func,            only : wf_nc_k, wf_igk,wf_ncx,wf_ng
  use vec_operate,          only : cross_product
  use defs_datatypes,       only : hdr_type, wffile_type
  use xc_functionals,       only : XC_EXCHANGE_CORRELATION,GS_xc_KIND,GS_xc_FUNCTIONAL,&
&                                  xc_string,XC_LDA_C_PW,XC_LDA_C_PZ,XC_LDA_C_WIGNER,&
&                                  XC_LDA_C_HL,XC_LDA_C_XALPHA
  implicit none
  character(*),   intent(in)    :: ETSF_file_name
  type(levels),   intent(inout) :: en
  type(bz_samp),  intent(inout) :: k
  integer,        intent(in)    :: ncid
!ws
! real(SP)                      :: lsop(3,3), b(3,3),cp(3)
  integer                       :: i1, i2, ik, is, ig, ib
! logical                       :: l_identity, l_inversion
!ETSF library stuff
! type(etsf_dims)               :: dims ! The dimensions of our system
  type(etsf_kpoints)            :: kpoints
  type(etsf_geometry)           :: geometry
  type(etsf_electrons)          :: electrons_group
  type(etsf_wavedata)           :: wavedata
! type(etsf_main)               :: main ! a container (will store the density)
! type(etsf_groups)             :: groups ! a container (will store the geometry)
  logical                       :: lstat ! to get informed on error
  type(etsf_io_low_error)       :: error_data ! to store informations about errors

  call msg('s','Dimensions...')
  !
  ! Dimensions
  !
  call etsf_io_dims_get(ncid, dims, lstat, error_data)
  ! Uncopied dimesions:
  ! n_spin_components = dims%number_of_components 
  ! 3        = number_of_reduced_dimensions 
  ! 3        = number_of_cartesian_directions = 1
  ! real space wfcs: number_of_grid_points_vector1 
  ! number_of_atoms 
  en%nb    = dims%max_number_of_states
  k%nibz   = dims%number_of_kpoints 
  nsym     = dims%number_of_symmetry_operations
  ng_vec   = dims%max_number_of_coefficients
  n_spinor = dims%number_of_spinor_components 
  n_sp_pol = dims%number_of_spins
  n_atom_species = dims%number_of_atom_species
  n_spin = max(n_spinor, n_sp_pol)


  call msg('s',':: K-points              :',k%nibz)
  call msg('s',':: Bands                 :',en%nb)
  call msg('s',':: Spin  [components/pol]:',(/n_spin,n_sp_pol/))
  call msg('s',':: (Charge) RL vectors   :',ng_vec)
  !
  ! Temperature, if present. Check for other possible names.
  !===========================================================
  call etsf_io_low_read_var(ncid, "tphysel", temperature, lstat)
  if(lstat) then
    default_Tel = temperature ! type conversion
  else
    default_Tel = 0.0_SP
  endif
  call msg('s',':: Temperature       [ev]:',default_Tel)
  !
  ! Electrons
  !===========================================================
  allocate( eigenvalues_(dims%max_number_of_states, &
&                        dims%number_of_kpoints,    &
&                        dims%number_of_spins ) )
  electrons_group%number_of_electrons      => number_of_electrons_
  electrons_group%exchange_functional      => exchange_functional_
  electrons_group%correlation_functional   => correlation_functional_

! Note: Current version returns only double precision 
! reals, hence local copies are required. Later version
! will also return reals generated on the fly by netCDF.
! The ETSF specs specify double precision storage in the -etsf.nc file.
! Also: n_spin is inconsistent with n_sp_pol: local copy needed here.
! --------------------------------------------------
! electrons_group%eigenvalues%array%data3d => en%E ! Not allowed due to type mismatch
! forall(ib=1:en%nb,ik=1:k%nibz,is=1:n_sp_pol) en%E(ib,ik,is) = eigenvalues_(ib,ik,is)

  electrons_group%eigenvalues%array%data3d => eigenvalues_
  allocate( en%E(en%nb, k%nibz, n_spin) )
  
! For splitting
! electrons_group%eigenvalues%k_splitted = .false.
! electrons_group%eigenvalues%spin_splitted = .false.
! electrons_group%eigenvalues%k_id =
! electrons_group%eigenvalues%spin_id = 

  call etsf_io_electrons_get(ncid, electrons_group, lstat, error_data)
  default_nel = number_of_electrons_
! GS_xc_FUNCTIONAL =
  call msg('s',':: Electrons             :',default_nel)
!call msg('s',':: XC potential          :',xc_string(GS_xc_KIND,GS_xc_FUNCTIONAL))
  en%E(:,:,1:n_sp_pol) = eigenvalues_(:,:,1:n_sp_pol)          ! Type conversion
  if(n_spinor==2) en%E(:,:,2) = en%E(:,:,1)
  !
  ! Geometry 
  !===========================================================
  ! All blocks have the following structure:
  !
  ! 1 - Allocate targets for data to be read according to dimensions 
  !     in specifications
  allocate( atoms_species_(dims%number_of_atoms) )
  allocate( primitive_vectors_ (dims%number_of_cartesian_directions, &
&                               dims%number_of_vectors ) )
  allocate( reduced_symmetry_matrices_(dims%number_of_reduced_dimensions,&
&                                      dims%number_of_reduced_dimensions, &
&                                      dims%number_of_symmetry_operations))
  allocate( reduced_atom_positions_(dims%number_of_reduced_dimensions, &
&                                   dims%number_of_atoms ) )
  !
  ! 2 - Point library variable to local target variables
  !
  geometry%primitive_vectors         => primitive_vectors_
  geometry%reduced_symmetry_matrices => reduced_symmetry_matrices_
  geometry%reduced_atom_positions    => reduced_atom_positions_
  geometry%atom_species              => atom_species_
  !
  ! 3 - Call the library for this group of variables
  !
  call etsf_io_geometry_get(ncid, geometry, lstat, error_data)
  !
  ! 4 - Copy target variables to SELF variables, allocating and processing
  !     where representations differ
  !
  ! Lattice vectors
  !
  allocate( dl_sop(3,3,nsym) )
  a(:,:) = primitive_vectors_(:,:)
  alat(1) = 2.0_SP*maxval( abs( a(1,:) ))
  alat(2) = 2.0_SP*maxval( abs( a(2,:) ))
  alat(3) = 2.0_SP*maxval( abs( a(3,:) ))
  call msg('s',':: Lattice factors [a.u.]:',alat)
  !
  ! Atomic data
  !
  !n_atoms_max     maximum number of atoms in  single species = maxval(n_atoms)
  !n_atom_species  number of atomic species
  !n_atoms         n_atoms(n_atom_species) : number of atoms for each atom species
  !atom_pos        the positions
  allocate( n_atoms(n_atom_species) )
  do i1 = 1, dims%number_of_atoms
    n_atoms( atom_species_(i1) ) = n_atoms( atom_species_(i1) ) + 1
  enddo
  n_atoms_max=maxval(n_atoms)
  n_atoms(:)=0 ! reset and fill it again (above just determines n_atoms_max)
  allocate( atom_pos(3, n_atoms_max, n_atom_species) )
  do i1 = 1, dims%number_of_atoms
    n_atoms( atom_species_(i1) ) = n_atoms( atom_species_(i1) ) + 1
    atom_pos(:, n_atoms( atom_species_(i1) ) , atom_species_(i1) ) = matmul( transpose(a), reduced_atom_positions_(:,i1) )
  enddo
  !
  ! Symmetry (time rev to be done)
  !===========================================================
  do is=1,nsym
    dl_sop(:,:,is) = reduced_symmetry_matrices_(:,:,is) 
  enddo
  i_time_rev = 0
  call msg('s',':: Symmetries   [spatial]:',nsym)
  call msg('s','::                [T-rev]:',i_time_rev==1)
  !
  ! K-points (convert to new units)
  !===========================================================
  allocate(reduced_coordinates_of_kpoints_(dims%number_of_reduced_dimensions, &
&                                          dims%number_of_kpoints))
  allocate(k%pt(k%nibz,3))

  kpoints%reduced_coordinates_of_kpoints => reduced_coordinates_of_kpoints_

  call etsf_io_kpoints_get(ncid, kpoints, lstat, error_data)
  !
  ! Note here the indices are reversed in SELF (why?!)
  ! Could map direct using low level
  !  etsf_io_low_read_var routine.
  !
  forall(i1=1:3, ik=1:k%nibz) k%pt(ik,i1) = reduced_coordinates_of_kpoints_(i1,ik)  ! type conversion
  !
  ! Wavefunction grids (need to be set up differently)
  !===========================================================

  allocate(reduced_coordinates_of_plane_waves_(dims%number_of_reduced_dimensions, &
&                                              dims%max_number_of_coefficients,   &
&                                              dims%number_of_kpoints ))
  allocate(number_of_coefficients_(dims%number_of_kpoints))
  allocate(g_vec(ng_vec,3))
  allocate(wf_nc_k(k%nibz))
  write(*,*) "TEST",dims%number_of_reduced_dimensions,dims%max_number_of_coefficients,dims%number_of_kpoints
  write(*,*) "TEST",ng_vec,k%nibz

  wavedata%reduced_coordinates_of_plane_waves%array%data3d => reduced_coordinates_of_plane_waves_
  wavedata%number_of_coefficients                          => number_of_coefficients_

  call etsf_io_wavedata_get(ncid, wavedata, lstat, error_data)
  if(.not.lstat) write(*,*) error_data
  write(*,*) "wavedata",wavedata%number_of_coefficients
!do i1=1,ng_vec
!   g_vec(i1,:)=matmul(transpose(b),agvec(:,i1))*alat(:)/2./pi
! enddo
  forall( ig=1:ng_vec, i1=1:3) g_vec(ig,i1) = reduced_coordinates_of_plane_waves_(i1,ig,1)
  forall( ik=1:k%nibz) wf_nc_k(ik) = number_of_coefficients_(ik)
  write(*,*) "nc_k",wf_nc_k

! Check all these based om information in specs.
  wf_ncx = maxval(wf_nc_k)
  wf_ng = wf_ncx
  allocate(wf_igk(wf_ncx,k%nibz))
  forall(i1=1:ng_vec) wf_igk(i1,:)=i1
  !
  ! KB factors, if present
  !===========================================================
  call etsf_io_low_read_var(ncid, "max_number_of_angular_momenta", max_number_of_angular_momenta_, lstat)
  if(lstat) then
    pp_n_l_comp_max = max_number_of_angular_momenta_
  else
    pp_n_l_comp_max = 0
    lwrite_PP = .false.
  endif
! call msg('s',':: PP n l comp max      :',pp_n_l_comp_max)
  call msg('s',':: Writing PP KB information    :',lwrite_PP)
  allocate( kb_formfactors_(dims%max_number_of_coefficients,    &
&                           dims%number_of_kpoints,             &
&                           dims%max_number_of_projectors,      &
&                           dims%max_number_of_angular_momenta, &
&                           dims%number_of_atomic_species ) )
allocate( kb_formfactor_derivative_(dims%max_number_of_coefficients,    &
&                                   dims%numer_of_kpoints,              &
&                                   dims%max_number_of_projectors,      &
&                                   dims%max_number_of_angular_momenta, &
&                                   dims%number_of_atomic_species ) )

  allocate( pp_n_l_comp(n_atom_species) )
  allocate( pp_kb (:,:,:)
  allocate( pp_kbd(:,:,:)
  


 !SELF
 !pp_n_l_comp_max 
 !I1=pp_n_l_comp
 !PP_alloc
 !R2=pp_kbs
 !pp_kbd
 !do i1=1,wf_nc_k(ik)
 !     if (wf_igk(i1,ik) > wf_ng) cycle
  !    pp_kb( wf_igk(i1,ik) ,:,:) = pp_disk(i1,:,:)
  !  enddo
 !allocate(pp_disk(wf_ncx,n_atom_species,pp_n_l_comp_max))
! n_atoms = dims%number_of_atoms 
! geometry%primitive_vectors => alat



  return
end subroutine e2s_db1
