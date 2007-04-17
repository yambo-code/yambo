subroutine e2s_db1(en,k,ncid)
!
! For version 0.6 of ETSF library
!
! Note: Current version returns only double precision 
! reals, hence local copies are required. Later version
! will also return reals generated on the fly by netCDF.
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
  use etsf_io
  use etsf_io_low_level
  use etsf_data
  use pars,                 only : SP, DP, PI, schlen
  use com,                  only : msg, error
  use electrons,            only : levels, n_bands, n_spin,n_sp_pol,n_spinor, spin_orbit
  use R_lattice,            only : bz_samp, ng_vec
  use D_lattice,            only : nsym, n_atom_species
  use D_lattice,            only : pp_n_l_comp_max
!  use D_lattice,            only : pp_n_l_comp_max,n_atom_species,n_atoms,pp_n_l_comp,&
!&                                  n_atoms_max,atom_pos

  use interfaces,           only : print_interface_dimensions
  implicit none
  type(levels),   intent(inout) :: en
  type(bz_samp),  intent(inout) :: k
  integer,        intent(in)    :: ncid
!ws
  integer                       :: i1, i2, ik, is, ig, ib
  integer                       :: a_nsym ! this dimension is changed later
!ETSF library stuff
  logical                       :: lstat ! to get informed on error
  type(etsf_io_low_error)       :: error_data ! to store informations about errors

  call msg('s','Dimensions...')
  !
  ! Dimensions
  !
  call etsf_io_dims_get(ncid, dims, lstat, error_data)
  !
  ! Directly read dimensions
  !
  en%nb    = dims%max_number_of_states
  k%nibz   = dims%number_of_kpoints 
  nsym     = dims%number_of_symmetry_operations ! This is modified later
  ng_vec   = dims%max_number_of_coefficients
  n_spinor = dims%number_of_spinor_components 
  n_sp_pol = dims%number_of_spins
  n_atom_species = dims%number_of_atom_species
  pp_n_l_comp_max = dims%max_number_of_angular_momenta
  !
  ! Derived SELF dimensions
  !
  n_spin = max(n_spinor, n_sp_pol)

!>>>DEBUG
! call msg('s',':: K-points              :',k%nibz)
! call msg('s',':: Bands                 :',en%nb)
! call msg('s',':: Spin  [components/pol]:',(/n_spinor,n_sp_pol/))
! call msg('s',':: (Charge) RL vectors   :',ng_vec) ! ?
! call msg('s',':: Atomic species        :',n_atom_species)
!<<<DEBUG
  !
  ! Geometry (must be first)
  !
  call import_geometry_group
  !
  ! Electrons
  !
  call import_electrons_group
  !
  ! K-points
  !
  call import_kpoints_group
  !
  ! PP/GW 
  !
  call import_gwdata_group
  !
  ! G-vectors grids and indices
  !
  call import_wavedata_group
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
  ! 4 - Copy target variables to SELF variables, allocating and       !
  !     processing where representations differ                       !
  !                       [ self_x = x_ ]                             !
  !=------------------------------------------------------------------!

  !
  ! Geometry group subroutines
  !
  subroutine import_geometry_group
    use vec_operate,          only : cross_product
    use R_lattice,            only : b
    use D_lattice,            only : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                  DL_vol, a, alat, &
&                                  n_atom_species,n_atoms,&
&                                  n_atoms_max,atom_pos, znucl
    implicit none
    type(etsf_geometry)           :: geometry_group
    logical                       :: l_identity, l_inversion
    real(SP)                      :: lsop(3,3), cp(3), scaling_factor, a_nsym
    real(SP), allocatable         :: asop(:,:,:)
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
    scaling_factor = 1.0_SP
    alat(1) = scaling_factor*maxval( abs( a(1,:) ))
    alat(2) = scaling_factor*maxval( abs( a(2,:) ))
    alat(3) = scaling_factor*maxval( abs( a(3,:) ))
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
    allocate( n_atoms(n_atom_species) )
    n_atoms(:) = 0 
    do i1 = 1, dims%number_of_atoms
      n_atoms( atom_species_(i1) ) = n_atoms( atom_species_(i1) ) + 1
    enddo
    n_atoms_max = maxval(n_atoms)
    n_atoms(:) = 0 ! reset and fill it again (above just determines n_atoms_max)
    allocate( atom_pos(3, n_atoms_max, n_atom_species) )
    do i1 = 1, dims%number_of_atoms
      n_atoms( atom_species_(i1) ) = n_atoms( atom_species_(i1) ) + 1
      atom_pos(:, n_atoms( atom_species_(i1) ) , atom_species_(i1) ) = &
&                 matmul( transpose(a), reduced_atom_positions_(:,i1) )
    enddo
    !
    ! Atomic species data
    !
    allocate( znucl(n_atom_species) )
    do i1 = 1, n_atom_species
      znucl(i1) = atomic_numbers_( i1 )
    enddo
    !
    ! Symmetry 
    !
    allocate( asop(3,3,nsym) )
    do is=1,nsym
      asop(:,:,is) = reduced_symmetry_matrices_(:,:,is) 
    enddo
    i_time_rev  = 1
    l_identity=.false.
    l_inversion=.false.
    do i1=1,nsym
      if (asop(1,1,i1)==1.and.asop(1,2,i1)==0.and.asop(1,3,i1)==0.and.&
&        asop(2,1,i1)==0.and.asop(2,2,i1)==1.and.asop(2,3,i1)==0.and.&
&        asop(3,1,i1)==0.and.asop(3,2,i1)==0.and.asop(3,3,i1)==1) l_identity=.true.
      if (asop(1,1,i1)==-1.and.asop(1,2,i1)==0.and.asop(1,3,i1)==0.and.&
&         asop(2,1,i1)==0.and.asop(2,2,i1)==-1.and.asop(2,3,i1)==0.and.&
&         asop(3,1,i1)==0.and.asop(3,2,i1)==0.and.asop(3,3,i1)==-1) l_inversion=.true.
    enddo
    if (.not.l_identity) call msg("l","[ID not found]...")
    if (l_identity) call msg("l","[ID found]...")
    if (l_inversion) then
      if (l_identity) then
        !
        ! Special case where I,-I present: no TR used.
        !
        call msg("l","[INV found. Time rev is off]")
        i_time_rev=0
      else
        !
        ! Identity was stripped in outkss: swap all
        ! symmetries with their inversions
        !
        call msg("l","[INV found]")
        asop(:,:,:)=-asop(:,:,:)
      endif
    else
      call msg("l","[INV not found]")
    endif
    !
    ! Calculate total number of symmetries, taking into
    ! account spin_orbit and i_time_reversal
    !
    a_nsym = nsym
    if (spin_orbit) then
      call msg("s","Spin orbit present: time reversal off.")
      i_time_rev = 0
      nsym=a_nsym
    else
      if (i_time_rev==1) then
        call msg("s","Time reversal being used.")
        nsym=a_nsym*2
      else
        call msg("s","Time reversal not being used.")
        nsym=a_nsym
      endif
    endif
    !
    ! Now fill the final SELF array of symmetries
    !
    allocate( dl_sop(3,3,nsym) )
    dl_sop(:,:,:)=0.0_SP
    do i1=1,a_nsym
      lsop=matmul(transpose(a),asop(:,:,i1))
      dl_sop(:,:,i1)=matmul(lsop,b)/2.0_SP/pi
      if (i_time_rev==1.or.n_spin==2) dl_sop(:,:,i1+a_nsym)=-dl_sop(:,:,i1)
    enddo
    deallocate(asop)
    ! XXXXX Why n_spin==2? Spin_orbit?
!   call msg('s',':: Symmetries   [spatial]:',nsym)
!   call msg('s','::                [T-rev]:',i_time_rev==1)

    call deallocate_geometry_group
    return
  end subroutine import_geometry_group

  subroutine import_electrons_group
    use electrons,            only : default_nel
    use xc_functionals,       only : XC_EXCHANGE_CORRELATION,GS_xc_KIND,GS_xc_FUNCTIONAL,&
&                                  xc_string,XC_LDA_C_PW,XC_LDA_C_PZ,XC_LDA_C_WIGNER,&
&                                  XC_LDA_C_HL,XC_LDA_C_XALPHA
    
    implicit none
    integer(2)                    :: e2s_functional_(2)
    type(etsf_electrons)          :: electrons_group

    call allocate_electrons_group
    electrons_group%number_of_electrons      => number_of_electrons_
    electrons_group%exchange_functional      => exchange_functional_
    electrons_group%correlation_functional   => correlation_functional_

    electrons_group%eigenvalues%data3d => eigenvalues_ 
    allocate( en%E(en%nb, k%nibz, n_spin) )

    call etsf_io_electrons_get(ncid, electrons_group, lstat, error_data)
    default_nel = number_of_electrons_
    default_nel = -1  ! Until fixed in ETSF file. Is calculated later.
!   call msg('s',':: Electrons             :',default_nel)
    !
    ! n_spin is inconsistent with n_sp_pol: local copy always needed here.
    !
    en%E(:,:,1:n_sp_pol) = eigenvalues_(:,:,1:n_sp_pol)     ! Type conversion
    if(n_spinor==2) en%E(:,:,2) = en%E(:,:,1)
    !
    ! Convert ETSF functional name to SELF variable
    !
    e2s_functional_(1:2) = e2s_functional(exchange_functional_, correlation_functional_)
    GS_xc_KIND       = e2s_functional_(1)
    GS_xc_FUNCTIONAL = e2s_functional_(2)
    ! For the moment, thesevalues are overwritten by "ixc" in miscellaneous data

    call deallocate_electrons_group
    return
  end subroutine import_electrons_group
  !
  ! This has to be fixed, and should be moved to module.
  !
  function e2s_functional(exc,corr)
    integer :: e2s_functional(2)
    character(len=*) :: exc, corr
    e2s_functional(1) = -1
    e2s_functional(2) = -1
  end function  e2s_functional

  !
  ! Wavefunction grids 
  !===========================================================
  subroutine import_wavedata_group
    use R_lattice,            only : g_vec, b
    use D_lattice,            only : alat
    use wave_func,            only : wf_nc_k, wf_igk,wf_ncx,wf_ng
    implicit none
    type(etsf_wavedata)           :: wavedata_group
    integer                       :: g_lim(6), gtotmax, ig_wf, ig, ix, ik, gmin, gmax
    integer, allocatable          :: g_vec_temp(:,:)
    logical                       :: gfound
    character(len=15)             :: format1

    call allocate_wavedata_group

    wavedata_group%reduced_coordinates_of_plane_waves%data3d => reduced_coordinates_of_plane_waves_ 
    wavedata_group%number_of_coefficients                    => number_of_coefficients_

    call etsf_io_wavedata_get(ncid, wavedata_group, lstat, error_data)
    if(.not.lstat) write(*,*) error_data


    write(format1,'(a4,i2,a9)') "(i6,",k%nibz,"(2x,3i4))"
   write(*,*) "forat|",format1,"|"
    do ig= 1, dims%max_number_of_coefficients
   write(97,format1) ig,((reduced_coordinates_of_plane_waves_(ix,ig,ik),ix=1,3),ik=1,k%nibz)
    enddo

    !
    ! Directly accessible data
    !
    allocate(wf_nc_k(k%nibz))
    forall( ik=1:k%nibz) wf_nc_k(ik) = number_of_coefficients_(ik)
    wf_ncx = maxval( wf_nc_k(:) )
    allocate(wf_igk(wf_ncx,k%nibz))
    wf_igk = -1
    !
    ! Here we construct the igk table from the G-vector lists provided for 
    ! each k explicitly, which refers to a single table.
    !
    ! Make the igk table: find the max G-vector possible
    ! g_lim(1) = minval( reduced_coordinates_of_plane_waves_(1,:,:) ) has
    ! some problems...
    !                                                     (x,G,k)
    do ix = 1,3
      g_lim(2*ix-1) = minval( reduced_coordinates_of_plane_waves_(ix,1:wf_nc_k(1),1) )
      g_lim(2*ix)   = maxval( reduced_coordinates_of_plane_waves_(ix,1:wf_nc_k(1),1) )
    enddo
    do ix = 1,3
      do ik = 2, k%nibz
        gmin = minval( reduced_coordinates_of_plane_waves_(ix,1:wf_nc_k(ik),ik) )
        gmax = maxval( reduced_coordinates_of_plane_waves_(ix,1:wf_nc_k(ik),ik) )
        if( gmin.lt.g_lim(2*ix-1) ) g_lim(2*ix-1) = gmin
        if( gmax.gt.g_lim(2*ix  ) ) g_lim(2*ix  ) = gmax
      enddo
    enddo

    gtotmax = (g_lim(2)-g_lim(1)+1)*(g_lim(4)-g_lim(3)+1)*(g_lim(6)-g_lim(5)+1)
    allocate( g_vec_temp(3, gtotmax) )
    !
    ! Copy the first k-point data   
    !    
    wf_nc_k(1) = number_of_coefficients_(1)
    forall( i1=1:3, ig=1:wf_nc_k(1)) g_vec_temp(i1, ig ) = reduced_coordinates_of_plane_waves_(i1, ig ,1)
    forall( i1 = 1:wf_nc_k(1)  ) wf_igk(i1,1)=i1
    ng_vec = wf_nc_k(1)
    !    
    ! Scan the remaining k-points for additional G-vectors
    !    
    do ik = 2, k%nibz
      do ig_wf = 1, wf_nc_k(ik)  ! Loop over G's in a new k shell
        gfound = .false.
        do ig = 1, ng_vec        ! Scan all G's in main G-vector (k-independent) table
                                 ! This could be made more efficient by cycling around list?
                                 ! do ig = min(ig_wf,ng_vec), ng_vec
                                 ! do ig = 1, min(ig_wf,ng_vec) etc
           if(reduced_coordinates_of_plane_waves_(1,ig_wf,ik).eq.g_vec_temp(1,ig).and. &
&             reduced_coordinates_of_plane_waves_(2,ig_wf,ik).eq.g_vec_temp(2,ig).and. &
&             reduced_coordinates_of_plane_waves_(3,ig_wf,ik).eq.g_vec_temp(3,ig)) then
              gfound = .true.
              wf_igk(ig_wf,ik)=ig
              exit
           endif
        enddo
        if(.not.gfound) then     ! Add new G-vector to main table
           ng_vec = ng_vec + 1 
           g_vec_temp( :, ng_vec ) = reduced_coordinates_of_plane_waves_(:, ig_wf ,ik)
           wf_igk(ig_wf,ik) = ng_vec
        endif
      enddo 
    enddo
    wf_ng  = maxval( wf_igk(:,:) )
    if(wf_ng.ne.ng_vec) call error('Error in calculating igk table.')
    !
    ! Convert G-vector table to SELF format
    !
    allocate(g_vec(ng_vec,3))
    do ig=1,ng_vec
      g_vec(ig,:)=matmul(transpose(b),g_vec_temp(:,ig))*alat(:)/2.0_SP/pi
    enddo
!DEBUG>
!    do ig=1,wf_ncx
!      write(*,101) ig,g_vec_temp(:,wf_igk(ig,1)),g_vec_temp(:,wf_igk(ig,2)),g_vec_temp(:,wf_igk(ig,3)),g_vec_temp(:,wf_igk(ig,4))
!    enddo
!101 format(i6,4(3i3,1x))
!DEBUG<
    deallocate(g_vec_temp)

    call deallocate_wavedata_group
    return
  end subroutine import_wavedata_group

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
    ! Note here the indices are reversed in SELF.
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
  ! therefore is not done here. This routine is left here for completeness.
  !
  subroutine import_gwdata_group
 use D_lattice,             ONLY : n_atom_species,pp_n_l_comp_max,&
&                                  PP_alloc,pp_kbs,pp_kb,pp_kbd
    implicit none
    type(etsf_gwdata)            :: gwdata_group
!    real(DP)                       :: pp_kb_kdb_disk(wf_ncx),&
!&                                  pp_kbs_disk(n_atom_species,pp_n_l_comp_max)

    call allocate_gwdata_group

    gwdata_group%kb_formfactors%data5d => kb_formfactors_
    gwdata_group%kb_formfactor_derivative%data5d => kb_formfactor_derivative_
!   gwdata_group%kb_formfactor_sign%data4d => kb_formfactor_sign_

    call etsf_io_gwdata_get(ncid, gwdata_group, lstat, error_data)
    !
    call deallocate_gwdata_group
    return
  end subroutine import_gwdata_group

  !
  ! Miscellaneous data 

  !
  ! Miscellaneous data 
  !===========================================================
  subroutine import_miscellaneous_data
    ! Here read any data not appearing in ETSF specs, or not
    ! yet supported properly
    use D_lattice,            only : default_Tel, n_atom_species, n_atoms
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
      default_Tel = temperature_ ! type conversion
    else
      default_Tel = 0.0_SP
    endif
!   call msg('s',':: Temperature      [ev] :',default_Tel)
    !
    ! Number of electrons (Abinit)
    !
    allocate(valence_charges_(n_atom_species))
    call etsf_io_low_read_var(ncid, "valence_charges", valence_charges_, lstat)
    if(lstat) then
      default_nel = 0.
      do i1 = 1,n_atom_species
        default_nel = default_nel + n_atoms(i1)*valence_charges_(i1)
      enddo
    endif
    deallocate(valence_charges_)
!   call msg('s',':: No. of electrons     : ',default_nel)
    !
    ! Spin orbit splitting (Abinit)
    !
    allocate(spin_orbit_atom_(n_atom_species))
    call etsf_io_low_read_var(ncid, "so_typat", spin_orbit_atom_, lstat)
    if(lstat) then
      spin_orbit = .false.
      if(any(spin_orbit_atom_.eq.2).or.any(spin_orbit_atom_.eq.3)) then
        spin_orbit = .true.
      endif
    else
      spin_orbit = .false.
    endif
!   call msg('s',':: Spin orbit splitting: ',spin_orbit)
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
!   call msg('s',':: XC potential          :',xc_string(GS_xc_KIND,GS_xc_FUNCTIONAL))

    return
  end subroutine import_miscellaneous_data

end subroutine e2s_db1
