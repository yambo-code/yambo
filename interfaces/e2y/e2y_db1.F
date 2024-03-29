!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CH DS
!
subroutine e2y_db1(en,k,ncid)
 !
 ! For version 1.0.3 of ETSF library, downloadable from
 ! http://www.etsf.eu/system/files/etsf_io-1.0.3.tar.gz
 !
 ! Note: Current version returns only double precision 
 ! reals, hence local copies are required. Later version
 ! may return reals generated on the fly by netCDF.
 ! The ETSF specs specify double precision storage in the -etsf.nc file.
 ! --------------------------------------------------
 ! Note:
 ! electrons_group%eigenvalues%data3d => en%E ! Not allowed due to type mismatch
 ! forall(ib=1:en%nb,ik=1:k%nibz,is=1:n_sp_pol) en%E(ib,ik,is) = eigenvalues_(ib,ik,is)
 ! For splitting (verion 0.5)
 ! electrons_group%eigenvalues%k_splitted = .false.
 ! electrons_group%eigenvalues%spin_splitted = .false.
 ! electrons_group%eigenvalues%k_id =
 ! electrons_group%eigenvalues%spin_id = 
 !
 use etsf_io
 use etsf_io_low_level
 use etsf_data
 use pars,                 only : SP,   schlen
 use com,                  only : msg
 use electrons,            only : levels, n_bands, n_spin,n_sp_pol,n_spinor, l_spin_orbit
 use wave_func,            only : wf_ncx
 use R_lattice,            only : bz_samp, nkibz
 use D_lattice,            only : nsym, n_atomic_species
 use mod_com2y,            only : print_interface_dimensions,ng_vec_abinit
 !
 implicit none
 !
 type(levels),   intent(inout) :: en
 type(bz_samp),  intent(inout) :: k
 integer,        intent(in)    :: ncid
 ! 
 ! Work Space
 !
 integer                       :: i1,ik,ib,is
 !ETSF library stuff
 logical                       :: lstat ! to get informed on error
 type(etsf_io_low_error)       :: error_data ! to store informations about errors

 call msg('s','Header/K-points/Energies...')
 !
 ! Dimensions
 !
 call etsf_io_dims_get(ncid, dims, lstat, error_data)
 !
 ! Directly read dimensions
 !
 en%nb    = dims%max_number_of_states
 k%nibz   = dims%number_of_kpoints 
 n_spinor = dims%number_of_spinor_components 
 n_sp_pol = dims%number_of_spins
 n_atomic_species = dims%number_of_atom_species
 if(trim(ETSF_kind)=='KSS') ng_vec_abinit = dims%max_number_of_coefficients
 if(trim(ETSF_kind)=='WFK') wf_ncx        = dims%max_number_of_coefficients
 !
 ! Derived YAMBO dimensions
 !
 n_spin  = max(n_spinor, n_sp_pol)
 nkibz   = k%nibz
 n_bands = en%nb
 call msg("l","done")
 !>>>DEBUG
 ! call msg('s',' K-points              ',k%nibz)
 ! call msg('s',' Bands                 ',en%nb)
 ! call msg('s',' Spin  [components/pol]',(/n_spinor,n_sp_pol/))
 ! call msg('s',' Max WF components     ',wf_ncx) 
 ! call msg('s',' Atomic species        ',n_atom_species)
 !<<<DEBUG
 !
 ! Geometry (must appear first, in order to define alat)
 !
 call msg('s','Cell data...')
 call import_geometry_group
 call msg("l","...done")
 !
 ! Electrons
 !
 call msg('s','Eigenvalues data...')
 call import_electrons_group
 call msg("l","done")
 !
 ! K-points
 !
 call msg('s','K-points mesh...')
 call import_kpoints_group
 call msg('l','done')
 !
 ! PP 
 !
 if(trim(ETSF_kind)=='KSS') call import_gwdata_group
 if(trim(ETSF_kind)=='WFK') call warning('Commutator not supported. You need a KSS file to include it.')
 !
 ! G-vectors grids and indices
 !
 call import_basisdata_group
 call msg('s','RL vectors...done')
 !
 ! All other data
 !
 call import_miscellaneous_data
 !
 ! Report
 !
 call print_interface_dimensions(en,k)

 return

contains
 
  !=------------------------------------------------------------------!
  ! All import blocks have the following structure:                   !
  !                                                                   !
  ! 1 - Allocate targets for data to be read according to dimensions  !
  !     in specifications                                             ! 
  !                       [call allocate_XXX_group ]                  !
  !                                                                   !
  ! 2 - Point library variable to local target variables              !
  !                       [ group%x => x_ ]                           !
  !                                                                   !
  ! 3 - Call the library for this group of variables                  !
  !                       [ call etsf_io_XXX_get ]                    !
  !                                                                   !
  ! 4 - Copy target variables to YAMBO variables, allocating and       !
  !     processing where representations differ                       !
  !                       [ yambo_x = x_ ]                             !
  !=------------------------------------------------------------------!

  !
  ! Geometry group subroutines
  !
  subroutine import_geometry_group
    use R_lattice,            only : b
    use D_lattice,            only : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                  a, alat, &
&                                  n_atomic_species,n_atoms_species,&
&                                  n_atoms_species_max,atom_pos, Z_species
    use mod_com2y,           only : symmetries_check_and_load, alat_mult_factor
    use vec_operate,         only :  define_b_and_DL_vol
    implicit none
    type(etsf_geometry)           :: geometry_group
    logical                       :: l_identity, l_inversion
    real(SP)                      :: lsop(3,3)
    integer                       :: a_nsym
    integer, allocatable          :: asop(:,:,:)
    character(3), allocatable     :: atom_species_names(:)

    call allocate_geometry_group

    geometry_group%primitive_vectors         => primitive_vectors_
    geometry_group%reduced_symmetry_matrices => reduced_symmetry_matrices_
    geometry_group%reduced_atom_positions    => reduced_atom_positions_
    geometry_group%atom_species              => atom_species_
    geometry_group%chemical_symbols          => chemical_symbols_
    geometry_group%atomic_numbers            => atomic_numbers_

    call etsf_io_geometry_get(ncid, geometry_group, lstat, error_data)
    !
    ! Lattice vectors
    !
    a(:,:) = transpose(primitive_vectors_(:,:))
    alat(1) = alat_mult_factor*maxval( abs( a(1,:) ))
    alat(2) = alat_mult_factor*maxval( abs( a(2,:) ))
    alat(3) = alat_mult_factor*maxval( abs( a(3,:) ))
!   call msg('s',' Lattice factors [a.u.]',alat)
    call define_b_and_DL_vol()
    !
    ! Atomic position data
    !
    ! n_atoms_max     maximum number of atoms in  single species = maxval(n_atoms)
    ! n_atom_species  number of atomic species
    ! n_atoms         n_atoms(n_atom_species) : number of atoms for each atom species
    ! atom_pos        the positions
    !
    allocate( n_atoms_species(n_atomic_species) )
    !
    ! Determine n_atoms_max
    !
    n_atoms_species(:) = 0 
    do i1 = 1, dims%number_of_atoms
      n_atoms_species( atom_species_(i1) ) = n_atoms_species( atom_species_(i1) ) + 1
    enddo
    n_atoms_species_max = maxval(n_atoms_species)
    !
    ! Reset n_atoms, and fill _natoms and  atom_pos arrays
    !
    n_atoms_species(:) = 0 
    allocate( atom_pos(3, n_atoms_species_max, n_atomic_species) )
    do i1 = 1, dims%number_of_atoms
      n_atoms_species( atom_species_(i1) ) = n_atoms_species( atom_species_(i1) ) + 1
      atom_pos(:, n_atoms_species( atom_species_(i1) ) , atom_species_(i1) ) = &
&                 matmul( transpose(a), reduced_atom_positions_(:,i1) )
    enddo
    !
    ! Atomic species data
    !
    allocate( Z_species(n_atomic_species) )
    do i1 = 1, n_atomic_species
      Z_species(i1) = atomic_numbers_( i1 )
    enddo
    !
    ! Symmetry 
    !
    a_nsym   = dims%number_of_symmetry_operations
    allocate( asop(3,3,a_nsym) )
    do is=1,a_nsym
      asop(:,:,is) = reduced_symmetry_matrices_(:,:,is) 
    enddo
    call symmetries_check_and_load(asop,a_nsym)

    call deallocate_geometry_group
    return
  end subroutine import_geometry_group


  subroutine import_electrons_group
    use electrons,            only : default_nel
    implicit none
    type(etsf_electrons)          :: electrons_group

    call allocate_electrons_group
    electrons_group%number_of_electrons      => number_of_electrons_
    electrons_group%eigenvalues%data3d => eigenvalues_ 

    call etsf_io_electrons_get(ncid, electrons_group, lstat, error_data)
    if (.not. lstat) call etsf_long_error(error_data)
    default_nel = number_of_electrons_
    default_nel = -1  ! Until fixed in ETSF file. Is calculated later.
    !
    ! n_spin is inconsistent with n_sp_pol: local copy always needed here.
    !
    allocate( en%E(en%nb, k%nibz, n_spin) )
    en%E(:,:,1:n_sp_pol) = eigenvalues_(:,:,1:n_sp_pol)     ! Type conversion
    if(n_spinor==2) en%E(:,:,2) = en%E(:,:,1)

    call deallocate_electrons_group
    ! Note that functional information is read in 'miscellaneous' section
    return
  end subroutine import_electrons_group
  !
  ! Wavefunction grids: l_KSS == (k-dependent = no)
  !===========================================================
  subroutine import_basisdata_group
    !
    use pars,                 only : pi
    use vec_operate,          only : sort, iku_v_norm
    use R_lattice,            only : g_vec, b, ng_vec
    use D_lattice,            only : alat, i_space_inv, i_time_rev
    use wave_func,            only : wf_nc_k, wf_igk,wf_ncx,wf_ng
    !
    implicit none
    !
    type(etsf_basisdata)          :: basisdata_group
    integer                       :: g_lim(6), gtotmax, ig_wf, ng_vec_old, ig, ic, ik, g_vec_new(3)
    integer,     allocatable      :: g_vec_tmp(:,:)
    real(SP),    allocatable      :: G_mod(:),g_vec_sort(:,:)
    integer,     allocatable      :: wf_igk_tmp(:,:),G_mod_indx(:)
    logical                       :: gamma_only,l_new
    logical,     allocatable      :: l_g_mapped(:)
    character(len=15)             :: format1
    !
    call allocate_basisdata_group
    !
    if(trim(ETSF_kind)=='KSS') &
&     basisdata_group%reduced_coordinates_of_plane_waves%data2d => reduced_coordinates_of_plane_waves_KSS_
    if(trim(ETSF_kind)=='WFK') &
&     basisdata_group%reduced_coordinates_of_plane_waves%data3d => reduced_coordinates_of_plane_waves_WFK_
    basisdata_group%number_of_coefficients  => number_of_coefficients_
    !
    call etsf_io_basisdata_get(ncid, basisdata_group, lstat, error_data)
    !
    if(trim(ETSF_kind)=='KSS') then
      !
      allocate( g_vec(ng_vec_abinit,3) )
      do i1=1,ng_vec_abinit
        g_vec(i1,:)=matmul(transpose(b),reduced_coordinates_of_plane_waves_KSS_(:,i1))*alat(:)/2.0_SP/pi
      enddo
      !
      ng_vec=ng_vec_abinit
      !
      gamma_only=(nkibz==1 .and. all(k%pt(1,:)==0.) )
      !
      if( i_time_rev==1 .or. i_space_inv==1 .and. .not.gamma_only) then
        call msg('s','Closing shells against inversion...')
        call G_rot_grid(-1,'extend_grid')
        call msg('l','done')
        if(ng_vec/=ng_vec_abinit) call msg('s',' ng_vec was increased to close the G-shells')
      endif
      !
      call e2y_wf_components(ncid)
      !
    endif
    !
    if(trim(ETSF_kind)=='WFK') then
      !
      allocate( wf_nc_k(k%nibz), wf_igk(wf_ncx,k%nibz) )
      wf_nc_k = basisdata_group%number_of_coefficients
      wf_igk=-1
      !
      ! First guess: G vectors= gvectors at gamma
      ng_vec = wf_nc_k(1) 
      allocate(g_vec_tmp(2*wf_ncx,3))
      !
      do ic=1,wf_nc_k(1)
        wf_igk(ic,1)=ic
        ig=wf_igk(ic,1)
        g_vec_tmp(ig,:)=reduced_coordinates_of_plane_waves_WFK_(:,ic,1)
      enddo
      !
      do ik=2,k%nibz
        ng_vec_old=ng_vec
        allocate(l_g_mapped(ng_vec_old))
        l_g_mapped=.false.
        do ic=1,wf_nc_k(ik)
          g_vec_new=reduced_coordinates_of_plane_waves_WFK_(:,ic,ik)
          l_new=.true.
          do ig=1,ng_vec_old
            if(l_g_mapped(ig)) cycle
            if( all(g_vec_tmp(ig,:)-g_vec_new(:)==0) ) then
              l_new=.false.
              l_g_mapped(ig)=.true.
              wf_igk(ic,ik)=ig
              exit
            endif
          enddo
          if(.not.l_new) cycle
          ng_vec=ng_vec+1
          g_vec_tmp(ng_vec,:)=g_vec_new
          wf_igk(ic,ik)=ng_vec
        enddo
        deallocate(l_g_mapped)
      enddo
      !
      ng_vec_abinit=ng_vec
      !
      allocate(g_vec(ng_vec,3))
      do ig=1,ng_vec
        g_vec(ig,:3)=matmul(transpose(b),g_vec_tmp(ig,:3))*alat(:)/2.0_SP/pi
      enddo
      deallocate(g_vec_tmp)
      !
      if(nsym>1) then
        call msg('s','Closing shells against symmetries...')
        do is=2,nsym
          call G_rot_grid(is,'extend_grid')
        enddo
        call msg('l','done')
        if(ng_vec/=ng_vec_abinit) call msg('s',' ng_vec was increased to close the G-shells')
      endif
      !
      ! Re-ordering by increasing module
      !==================================
      ! indx_m1 (j) went to position indx_m1(j)
      ! indx    (i) before was in position indx(i) 
      allocate(G_mod(ng_vec),G_mod_indx(ng_vec))
      do ig=1,ng_vec
        G_mod(ig)=iku_v_norm(g_vec(ig,:))
      enddo
      call sort(G_mod,indx_m1=G_mod_indx)
      deallocate(G_mod)
      !
      allocate(g_vec_sort(ng_vec,3))
      g_vec_sort=g_vec
      g_vec(G_mod_indx(:ng_vec),:3)=g_vec_sort(:ng_vec,:3)
      deallocate(g_vec_sort)
      !
      ! Sort wf_igk accordingly
      allocate( wf_igk_tmp(wf_ncx,k%nibz) )
      wf_igk_tmp=wf_igk
      wf_igk=-1
      do ik=1,k%nibz
        wf_igk(:wf_nc_k(ik),ik)=G_mod_indx(wf_igk_tmp(:wf_nc_k(ik),ik))
      enddo
      deallocate( wf_igk_tmp,G_mod_indx )
      wf_ng=maxval(wf_igk)
      !
    endif
    !
    call deallocate_basisdata_group
    !
    return
    !
  end subroutine import_basisdata_group
  !
  ! K-points (convert to new units)
  !===========================================================
  subroutine import_kpoints_group
    use pars,                 only : pi
    use R_lattice,            only : b
    use D_lattice,            only : alat
    implicit none
    type(etsf_kpoints)            :: kpoints_group

    call allocate_kpoints_group

    kpoints_group%reduced_coordinates_of_kpoints => reduced_coordinates_of_kpoints_

    call etsf_io_kpoints_get(ncid, kpoints_group, lstat, error_data)
    !
    ! Note here the indices are reversed in YAMBO.
    ! However, could map directly using low level
    ! etsf_io_low_read_var routine, if type conversion problem was absent
    !
    allocate(k%pt(k%nibz,3))
    do ik = 1,k%nibz
      k%pt(ik,:)=matmul(transpose(b),reduced_coordinates_of_kpoints_(:,ik))*alat(:)/2.0_SP/pi
    enddo

    call deallocate_kpoints_group
    return
  end subroutine import_kpoints_group

  !
  ! GW data
  !===========================================================
  ! Note that kbpp data is read and converted separately (split) over k-point,
  ! therefore is not done here. Use of pointers is left here for completeness.
  ! This routine must be called, however, to set pp_n_l_comp array.
  !
  subroutine import_gwdata_group
    use D_lattice,             only : n_atomic_species
    use pseudo,                only : pp_n_l_times_proj_max,&
&                                  pp_n_l_comp, pp_kbs,pp_kb,pp_kbd,&
&                                  pp_table,l_many_proj,pp_n_l_max
    implicit none
    type(etsf_gwdata)            :: gwdata_group

    pp_n_l_times_proj_max = dims%max_number_of_angular_momenta

!   call allocate_gwdata_group

!   gwdata_group%kb_formfactors%data5d => kb_formfactors_
!   gwdata_group%kb_formfactor_derivative%data5d => kb_formfactor_derivative_
!   gwdata_group%kb_formfactor_sign%data3d => kb_formfactor_sign_

!   call etsf_io_gwdata_get(ncid, gwdata_group, lstat, error_data)

    l_many_proj=.false.
    pp_n_l_max=abs(pp_n_l_times_proj_max)
    allocate(pp_n_l_comp(n_atomic_species),&
&            pp_table(3,n_atomic_species,pp_n_l_times_proj_max))
    pp_n_l_comp(:)=pp_n_l_max
    !
    ! if "l_many_proj" the table is readen in a2y_wf
    if(.not.l_many_proj) then
      do i1=1,pp_n_l_times_proj_max
        pp_table(1,:,i1)=i1  !  l+1
        pp_table(2,:,i1)=1   !  n_proj
        pp_table(3,:,i1)=1   !  i_spin
      enddo
    endif
    !
    call deallocate_gwdata_group
    return
  end subroutine import_gwdata_group
  !
  ! Miscellaneous data 
  !===========================================================
  subroutine import_miscellaneous_data
    !
    ! Here read any data not appearing in ETSF specs, or not
    ! yet supported properly
    !
    use D_lattice,            only : input_GS_Tel, n_atomic_species, n_atoms_species
    use electrons,            only : l_spin_orbit, default_nel
    use xc_functionals,       only : GS_xc_KIND,GS_xc_FUNCTIONAL
    use mod_xc2y,             only : XC_yamboID, XC_yamboID2kind
    integer                       :: i1
    !
    ! Temperature (Abinit)
    !
    call etsf_io_low_read_var(ncid, "tphysel", temperature_, lstat)
    if(lstat) then
      input_GS_Tel = temperature_ ! type conversion
    else
      input_GS_Tel = 0.0_SP
    endif
    !
    ! Number of electrons (Abinit)
    !
    allocate(valence_charges_(n_atomic_species))
    call etsf_io_low_read_var(ncid, "valence_charges", valence_charges_, lstat)
    if(lstat) then
      default_nel = 0.
      do i1 = 1,n_atomic_species
        default_nel = default_nel + n_atoms_species(i1)*valence_charges_(i1)
      enddo
    endif
    deallocate(valence_charges_)
    !
    ! Spin orbit splitting (Abinit)
    !
    allocate(spin_orbit_atom_(n_atomic_species))
    call etsf_io_low_read_var(ncid, "so_typat", spin_orbit_atom_, lstat)
    if(lstat) then
      l_spin_orbit = .false.
      if(any(spin_orbit_atom_.eq.2).or.any(spin_orbit_atom_.eq.3)) then
        l_spin_orbit = .true.
      endif
    else
      l_spin_orbit = .false.
    endif
    deallocate(spin_orbit_atom_)
    !
    ! XC functional (Abinit)
    !
    call etsf_io_low_read_var(ncid, "ixc", ixc_, lstat)
    GS_xc_FUNCTIONAL = XC_yamboID('abinit',abinit_func=ixc_)
    GS_xc_KIND = XC_yamboID2kind(GS_xc_FUNCTIONAL)
    return
  end subroutine import_miscellaneous_data

end subroutine e2y_db1
