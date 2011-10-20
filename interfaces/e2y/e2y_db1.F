!
! Copyright (C) 2000-2011 C. Hogan and the YAMBO team
!              http://www.yambo-code.org
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
 use pars,                 only : SP, DP, PI, schlen
 use com,                  only : msg, error
 use electrons,            only : levels, n_bands, n_spin,n_sp_pol,n_spinor, spin_orbit
 use wave_func,            only : wf_ncx
 use R_lattice,            only : bz_samp
 use D_lattice,            only : nsym, n_atomic_species
 use mod_com2y,            only : print_interface_dimensions
 implicit none
 type(levels),   intent(inout) :: en
 type(bz_samp),  intent(inout) :: k
 integer,        intent(in)    :: ncid
 ! 
 ! Work Space
 !
 integer                       :: i1,i2,ik,ib,is
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
 wf_ncx   = dims%max_number_of_coefficients
 n_spinor = dims%number_of_spinor_components 
 n_sp_pol = dims%number_of_spins
 n_atomic_species = dims%number_of_atom_species
 !
 ! Derived YAMBO dimensions
 !
 n_spin = max(n_spinor, n_sp_pol)
 call msg("l","done")
 !>>>DEBUG
 ! call msg('s',':: K-points              :',k%nibz)
 ! call msg('s',':: Bands                 :',en%nb)
 ! call msg('s',':: Spin  [components/pol]:',(/n_spinor,n_sp_pol/))
 ! call msg('s',':: Max WF components     :',wf_ncx) 
 ! call msg('s',':: Atomic species        :',n_atom_species)
 !<<<DEBUG
 !
 ! Geometry (must appear first, in order to define alat)
 !
 call msg('s','Cell data...')
 call import_geometry_group
 call msg("l","done")
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
 call import_gwdata_group
 !
 ! G-vectors grids and indices
 !
 call msg('s','RL vectors...')
 call import_basisdata_group
 call msg('l','done')
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
    use vec_operate,          only : cross_product
    use R_lattice,            only : b
    use D_lattice,            only : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                  DL_vol, a, alat, &
&                                  n_atomic_species,n_atoms_species,&
&                                  n_atoms_species_max,atom_pos, Z_species
    use mod_com2y,           only : symmetries_check_and_load, alat_mult_factor
    implicit none
    type(etsf_geometry)           :: geometry_group
    logical                       :: l_identity, l_inversion
    real(SP)                      :: lsop(3,3), cp(3)
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
    a(:,:) = primitive_vectors_(:,:)
    alat(1) = alat_mult_factor*maxval( abs( a(1,:) ))
    alat(2) = alat_mult_factor*maxval( abs( a(2,:) ))
    alat(3) = alat_mult_factor*maxval( abs( a(3,:) ))
!   call msg('s',':: Lattice factors [a.u.]:',alat)
    cp = cross_product(a(2,:),a(3,:))
    do i1 = 1,3
      DL_vol = DL_vol+a(1,i1)*cp(i1)
    enddo
    b(1,:) = cross_product(a(2,:),a(3,:))*2.0_SP*pi/DL_vol
    b(2,:) = cross_product(a(3,:),a(1,:))*2.0_SP*pi/DL_vol
    b(3,:) = cross_product(a(1,:),a(2,:))*2.0_SP*pi/DL_vol
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
  ! Wavefunction grids: assumes k-dependent = no 
  !===========================================================
  subroutine import_basisdata_group
    use R_lattice,            only : g_vec, b, ng_vec
    use D_lattice,            only : alat
    use wave_func,            only : wf_nc_k, wf_igk,wf_ncx,wf_ng
    implicit none
    type(etsf_basisdata)           :: basisdata_group
    integer                       :: g_lim(6), gtotmax, ig_wf, ig, ix, ik, gmin, gmax
    integer, allocatable          :: g_vec_temp(:,:)
    logical                       :: gfound
    character(len=15)             :: format1

    call allocate_basisdata_group

    basisdata_group%reduced_coordinates_of_plane_waves%data2d => reduced_coordinates_of_plane_waves_ 
    basisdata_group%number_of_coefficients                    => number_of_coefficients_

    call etsf_io_basisdata_get(ncid, basisdata_group, lstat, error_data)
    !
    ! NB: the following are true because the igk array is trivial for k-dep=no
    !
    ng_vec  = wf_ncx
    wf_ng   = wf_ncx
    allocate( wf_nc_k(k%nibz), g_vec(ng_vec,3), wf_igk(wf_ncx,k%nibz) )

    wf_nc_k = ng_vec
    do i1=1,ng_vec
      g_vec(i1,:)=matmul(transpose(b),reduced_coordinates_of_plane_waves_(:,i1))*alat(:)/2.0_SP/pi
    enddo
    forall(i1=1:ng_vec) wf_igk(i1,:)=i1

    call deallocate_basisdata_group
    return

  end subroutine import_basisdata_group
  !
  ! K-points (convert to new units)
  !===========================================================
  subroutine import_kpoints_group
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
&                                  pp_n_l_comp, pp_kbs,pp_kb,pp_kbd
    implicit none
    type(etsf_gwdata)            :: gwdata_group

    pp_n_l_times_proj_max = dims%max_number_of_angular_momenta

!   call allocate_gwdata_group

!   gwdata_group%kb_formfactors%data5d => kb_formfactors_
!   gwdata_group%kb_formfactor_derivative%data5d => kb_formfactor_derivative_
!   gwdata_group%kb_formfactor_sign%data3d => kb_formfactor_sign_

!   call etsf_io_gwdata_get(ncid, gwdata_group, lstat, error_data)

    allocate(pp_n_l_comp(n_atomic_species))
    pp_n_l_comp = pp_n_l_times_proj_max
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
    use electrons,            only : spin_orbit, default_nel
    use xc_functionals,       only : XC_EXCHANGE_CORRELATION,GS_xc_KIND,GS_xc_FUNCTIONAL,&
&                                  xc_string,XC_LDA_C_PW,XC_LDA_C_PZ,XC_LDA_C_WIGNER,&
&                                  XC_LDA_C_HL,XC_LDA_C_XALPHA
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
      spin_orbit = .false.
      if(any(spin_orbit_atom_.eq.2).or.any(spin_orbit_atom_.eq.3)) then
        spin_orbit = .true.
      endif
    else
      spin_orbit = .false.
    endif
    deallocate(spin_orbit_atom_)
    !
    ! XC functional (Abinit)
    !
    call etsf_io_low_read_var(ncid, "ixc", ixc_, lstat)
    if(lstat) then
      select case (ixc_)
      case (1,7)
        GS_xc_FUNCTIONAL=XC_LDA_C_PW
        GS_xc_KIND=XC_EXCHANGE_CORRELATION
      case (2)
        GS_xc_FUNCTIONAL=XC_LDA_C_PZ
        GS_xc_KIND=XC_EXCHANGE_CORRELATION
      case (3)
        GS_xc_FUNCTIONAL=XC_LDA_C_PZ
        GS_xc_KIND=XC_EXCHANGE_CORRELATION
      case (4)
        GS_xc_FUNCTIONAL=XC_LDA_C_WIGNER
        GS_xc_KIND=XC_EXCHANGE_CORRELATION
      case (5)
        GS_xc_FUNCTIONAL=XC_LDA_C_HL
        GS_xc_KIND=XC_EXCHANGE_CORRELATION
      case (6)
        GS_xc_FUNCTIONAL=XC_LDA_C_XALPHA
        GS_xc_KIND=XC_EXCHANGE_CORRELATION
      case default
        GS_xc_FUNCTIONAL=-1
        GS_xc_KIND=-1
      end select 
    else
      GS_xc_FUNCTIONAL=-1
      GS_xc_KIND=-1
    endif

    return
  end subroutine import_miscellaneous_data

end subroutine e2y_db1
