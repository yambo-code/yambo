subroutine e2s_kb_pp( pp_section, ikibz, ncid )
  use etsf_io
  use etsf_io_low_level
  use etsf_data
  use pars,                 only : SP, DP, PI, schlen
  use com,                  only : msg
  use electrons,            only : n_spin,n_sp_pol,n_spinor
  use D_lattice,            only : pp_n_l_comp_max,n_atom_species,pp_n_l_comp,&
&                                  n_atoms_max,pp_kbs,pp_kb,pp_kbd
! use D_lattice,             ONLY : n_atom_species,pp_n_l_comp_max,&
!&                                  PP_alloc,pp_kbs,pp_kb,pp_kbd
  use wave_func,            only : wf_ncx,wf_nc_k
  implicit none
  integer,        intent(in)    :: ncid, ikibz
!ws
  integer                       :: ig, ib, ip, ia
!ETSF library stuff
  logical                       :: lstat ! to get informed on error
  type(etsf_io_low_error)       :: error_data ! to store informations about errors
  integer        :: start(6), count(6), ncvarid
  real(DP)                      :: pp_kbs_(n_atom_species,pp_n_l_comp_max)
!  double precision, intent(inout) :: pp_section(dims%max_number_of_coefficients, &
!&           dims%max_number_of_angular_momenta, dims%number_of_atom_species )
  double precision, allocatable :: pp_section(:,:,:)

!    allocate( kb_formfactors_(  dims%max_number_of_coefficients,    &
!&                               dims%number_of_kpoints,            &
!&                               dims%max_number_of_projectors,      &
!&                               dims%max_number_of_angular_momenta, &
!&                               dims%number_of_atom_species ))
!    allocate( kb_formfactor_derivative_( &
!&                               dims%max_number_of_coefficients,    &
!&                               dims%number_of_kpoints,            &
!&                               dims%max_number_of_projectors,      &
!&                               dims%max_number_of_angular_momenta, &
!&                               dims%number_of_atom_species ))

  !
  ! Initialize the mapping
  !
  start(:) = 1     ; count(:) = 0
  start(2) = ikibz ; count(2) = 1   ! Split on k always

! dims%max_number_of_projectors = 1 always
  start(3) = 1     ; count(3) = 1   ! Split on k always

! From PP_alloc:
!allocate(pp_kbs(n_atom_species,pp_n_l_comp_max),stat=alloc_err(1))
!allocate(pp_kb(wf_ng,n_atom_species,pp_n_l_comp_max),stat=alloc_err(2))
!allocate(pp_kbd(wf_ng,n_atom_species,pp_n_l_comp_max),stat=alloc_err(3)

! These just need to be filled for ioKBPP
! Read for just one k point

  call etsf_io_low_read_var(ncid, "kb_formfactors", &
                            & pp_section, lstat,          &
                            & error_data = error_data, start = start, count = count)
  forall( ig=1:wf_nc_k(ikibz), ia=1:n_atom_species, ip=1:pp_n_l_comp_max ) &
&          pp_kb(ig,ia,ip) = pp_section(ig,ia,ip)
  call etsf_io_low_read_var(ncid, "kb_formfactor_derivative", &
                            & pp_section, lstat,          &
                            & error_data = error_data, start = start, count = count)
  forall( ig=1:wf_nc_k(ikibz), ia=1:n_atom_species, ip=1:pp_n_l_comp_max ) &
&          pp_kbd(ig,ia,ip) = pp_section(ig,ia,ip)
  call etsf_io_low_read_var(ncid, "kb_formfactor_sign", &
                            & pp_kbs_, lstat,          &
                            & error_data = error_data, start = start, count = count)
  forall( ia=1:n_atom_species, ip=1:pp_n_l_comp_max ) &
&          pp_kbs(ia,ip) = pp_kbs_(ia,ip)

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

!   call PP_alloc()
!   do i1=1,n_atom_species
!  do i2=1,pp_n_l_comp_max
!    read(11) pp_kb_kdb_disk
!    pp_kb(:,i1,i2)=pp_kb_kdb_disk
!    read(11) pp_kb_kdb_disk
!    pp_kbd(:,i1,i2)=pp_kb_kdb_disk
!  enddo
!enddo

!  #max_number_of_projectors = 1 always
!kbff(#coeff,#k,#proj,#ang,#species)
!pp_kb(#G,#species,#pp_n_l_comp_max=#proj) ?? #ang, #k


  return
end subroutine e2s_kb_pp
