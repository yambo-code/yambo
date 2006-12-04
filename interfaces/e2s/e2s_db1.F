subroutine e2s_db1(en,k,ncid)
  use etsf_io
  use etsf_io_low_level
  use etsf_data
  use pars,                 only : SP, DP, PI, schlen
  use com,                  only : msg
  use electrons,            only : levels, n_bands, n_spin,n_sp_pol,n_spinor
  use R_lattice,            only : bz_samp, ng_vec
  use D_lattice,            only : nsym, n_atom_species
  implicit none
  type(levels),   intent(inout) :: en
  type(bz_samp),  intent(inout) :: k
  integer,        intent(in)    :: ncid
!ws
  integer                       :: i1, i2, ik, is, ig, ib
!ETSF library stuff
  type(etsf_wavedata)           :: wavedata_group
  logical                       :: lstat ! to get informed on error
  type(etsf_io_low_error)       :: error_data ! to store informations about errors

  call msg('s','Dimensions...')
  !
  ! Dimensions
  !
  call etsf_io_dims_get(ncid, dims, lstat, error_data)

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
  ! Electrons
  !
  call import_electrons_group
  !
  ! K-points
  !
  call import_kpoints_group
  !
  ! Geometry
  !
  call import_geometry_group
  !
  ! All other data
  !
  call import_miscellaneous_data
  !
  ! Report
  !
! call interface report

  return

contains

  subroutine import_miscellaneous_data
    use D_lattice,            only : default_Tel
    ! check for all possible code dependent names
    call etsf_io_low_read_var(ncid, "tphysel", temperature_, lstat)
    if(lstat) then
      default_Tel = temperature_ ! type conversion
    else
      default_Tel = 0.0_SP
    endif
    call msg('s',':: Temperature       [ev]:',default_Tel)
  end subroutine import_miscellaneous_data

  subroutine import_geometry_group
    use vec_operate,          only : cross_product
    use D_lattice,            only : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                  DL_vol, a, alat, &
&                                  n_atom_species,n_atoms,&
&                                  n_atoms_max,atom_pos
    implicit none
    type(etsf_geometry)           :: geometry_group
! logical                       :: l_identity, l_inversion
! real(SP)                      :: lsop(3,3), b(3,3),cp(3)
  !
  ! Geometry 
  !
  ! All blocks have the following structure:
  !
  ! 1 - Allocate targets for data to be read according to dimensions 
  !     in specifications

  call allocate_geometry_group
  !
  ! 2 - Point library variable to local target variables
  !
  geometry_group%primitive_vectors         => primitive_vectors_
  geometry_group%reduced_symmetry_matrices => reduced_symmetry_matrices_
  geometry_group%reduced_atom_positions    => reduced_atom_positions_
  geometry_group%atom_species              => atom_species_
  !
  ! 3 - Call the library for this group of variables
  !
  call etsf_io_geometry_get(ncid, geometry_group, lstat, error_data)
  !
  ! 4 - Copy target variables to SELF variables, allocating and processing
  !     where representations differ
  !
  ! Lattice vectors
  !
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
  !
  allocate( dl_sop(3,3,nsym) )
  do is=1,nsym
    dl_sop(:,:,is) = reduced_symmetry_matrices_(:,:,is) 
  enddo
  i_time_rev = 0
  call msg('s',':: Symmetries   [spatial]:',nsym)
  call msg('s','::                [T-rev]:',i_time_rev==1)
  call deallocate_geometry_group
  end subroutine import_geometry_group

  subroutine import_electrons_group
    use electrons,            only : default_nel
    use xc_functionals,       only : XC_EXCHANGE_CORRELATION,GS_xc_KIND,GS_xc_FUNCTIONAL,&
&                                  xc_string,XC_LDA_C_PW,XC_LDA_C_PZ,XC_LDA_C_WIGNER,&
&                                  XC_LDA_C_HL,XC_LDA_C_XALPHA
    implicit none
    type(etsf_electrons)          :: electrons_group

    call allocate_electrons_group
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
    call deallocate_electrons_group
    return
  end subroutine import_electrons_group

  !
  ! Wavefunction grids (need to be set up differently)
  !===========================================================
  subroutine import_wavedata_group
    use R_lattice,            only : g_vec
    use wave_func,            only : wf_nc_k, wf_igk,wf_ncx,wf_ng

    call allocate_wavedata_group
    allocate(g_vec(ng_vec,3))
    allocate(wf_nc_k(k%nibz))
    write(*,*) "TEST",dims%number_of_reduced_dimensions,dims%max_number_of_coefficients,dims%number_of_kpoints
    write(*,*) "TEST",ng_vec,k%nibz

    wavedata_group%reduced_coordinates_of_plane_waves%array%data3d => reduced_coordinates_of_plane_waves_
    wavedata_group%number_of_coefficients                          => number_of_coefficients_

    call etsf_io_wavedata_get(ncid, wavedata_group, lstat, error_data)
    if(.not.lstat) write(*,*) error_data
    write(*,*) "wavedata",wavedata_group%number_of_coefficients
!do i1=1,ng_vec
!   g_vec(i1,:)=matmul(transpose(b),agvec(:,i1))*alat(:)/2./pi
! enddo
    forall( ig=1:ng_vec, i1=1:3) g_vec(ig,i1) = reduced_coordinates_of_plane_waves_(i1,ig,1)
    forall( ik=1:k%nibz) wf_nc_k(ik) = number_of_coefficients_(ik)
    write(*,*) "nc_k",wf_nc_k

!   Check all these based om information in specs.
    wf_ncx = maxval(wf_nc_k)
    wf_ng = wf_ncx
    allocate(wf_igk(wf_ncx,k%nibz))
    forall(i1=1:ng_vec) wf_igk(i1,:)=i1

    call deallocate_wavedata_group
    return
  end subroutine import_wavedata_group

  !
  ! K-points (convert to new units)
  !===========================================================
  subroutine import_kpoints_group
    implicit none
    type(etsf_kpoints)            :: kpoints_group

    call allocate_kpoints_group
    allocate(k%pt(k%nibz,3))

    kpoints_group%reduced_coordinates_of_kpoints => reduced_coordinates_of_kpoints_

    call etsf_io_kpoints_get(ncid, kpoints_group, lstat, error_data)
    !
    ! Note here the indices are reversed in SELF (why?!)
    ! Could map direct using low level
    !  etsf_io_low_read_var routine.
    !
    forall(i1=1:3, ik=1:k%nibz) k%pt(ik,i1) = reduced_coordinates_of_kpoints_(i1,ik)  ! type conversion
    call deallocate_kpoints_group
    return
  end subroutine import_kpoints_group

end subroutine e2s_db1
