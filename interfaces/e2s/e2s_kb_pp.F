subroutine e2s_kb_pp(ncid,lwrite_PP)
  use etsf_io
  use etsf_io_low_level
  use etsf_data
  use pars,                 only : SP, DP, PI, schlen
  use com,                  only : msg
  use electrons,            only : levels, default_nel,n_bands,n_spin,n_sp_pol,n_spinor
  use R_lattice,            only : bz_samp, ng_vec, g_vec,nkibz
  use D_lattice,            only : pp_n_l_comp_max,n_atom_species,n_atoms,pp_n_l_comp,&
&                                  n_atoms_max,atom_pos
  implicit none
  logical,        intent(inout) :: lwrite_PP
  integer,        intent(in)    :: ncid
!ws
  integer                       :: i1, i2, ik, is, ig, ib
!ETSF library stuff
  type(etsf_kpoints)            :: kpoints_group
  type(etsf_geometry)           :: geometry_group
  type(etsf_electrons)          :: electrons_group
  type(etsf_gwdata)             :: gwdata_group
  logical                       :: lstat ! to get informed on error
  type(etsf_io_low_error)       :: error_data ! to store informations about errors
  !
  ! Check for KB factors
  !===========================================================
  call etsf_io_low_read_var(ncid, "max_number_of_angular_momenta", &
&                                  max_number_of_angular_momenta_, lstat)
  if(lstat) then
    pp_n_l_comp_max = max_number_of_angular_momenta_
    lwrite_PP = .true.
  else
    pp_n_l_comp_max = 0
    lwrite_PP = .false.
  endif
  call etsf_io_low_read_var(ncid, "max_number_of_projectors", &
&                                  max_number_of_projectors_, lstat)
  if(lstat) then
    pp_n_l_comp = max_number_of_projectors_
    lwrite_PP = lwrite_PP.and..true.
  else
    pp_n_l_comp = 0
    lwrite_PP = .false.
  endif


  if(lwrite_PP) then
    call msg('s',':: Writing PP KB information    :',lwrite_PP)
    call msg('s',':: PP n l comp max      :',pp_n_l_comp_max)
!   call allocate_gwdata_group
!   call etsf_io_gwdata_get(ncid, gwdata_group, lstat, error_data)
!   call deallocate_gwdata_group
    return
  endif
! !
! ! 
! !
! if(force_PP) then ! NO THIS IS STUPID
! endif

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


  return
end subroutine e2s_kb_pp
