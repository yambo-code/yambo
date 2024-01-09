!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine e2y_wf_components(ncid)
  !==============================================
  ! Reads wf for ib=1 (all kpt) and returns
  ! wf_ncx,wf_nc_k,wf_igk,wf_ng
  ! This is needed if we are reading a KSS
  !
  use etsf_io
  use etsf_io_low_level
  use etsf_data,             only : dims
  use pars,                  only : SP,DP
  use mod_com2y,             only : ng_vec_abinit
  use R_lattice,             only : nkibz
  use wave_func,             only : wf_ncx,wf_nc_k,wf_igk,wf_ng
  !
  implicit none
  !
  integer,           intent(in)  :: ncid
  !
  real(SP)                       :: wf_disk(2,ng_vec_abinit,nkibz)
  integer,          allocatable  :: wf_igk_tmp(:,:)
  real(DP),         allocatable  :: wavefunction_section_(:,:,:,:)
  logical                        :: lstat
  type(etsf_io_low_error)        :: error_data 
  ! 
  ! Work Space
  !
  integer        :: ic, ik, ig, ireal
  integer        :: start(6), count(6)
  !
  allocate(wavefunction_section_(dims%real_or_complex_coefficients, &
&                                dims%max_number_of_coefficients,   &
&                                dims%number_of_spinor_components,  &
&                                dims%number_of_kpoints ) )
  !
  ! Initialize the mapping
  !
  start(:) = 1     ; count(:) = 0
  start(4) = 1     ; count(4) = 1  ! nb
  start(6) = 1     ; count(6) = 1  ! spin polarization
  !
  call etsf_io_low_read_var(ncid, "coefficients_of_wavefunctions", &
                            & wavefunction_section_, lstat,          &
                            & error_data = error_data, start = start, count = count)
  !
  forall( ireal=1:2, ig=1:ng_vec_abinit, ik=1:nkibz ) wf_disk(ireal,ig,ik) = wavefunction_section_(ireal,ig,1,ik)
  !
  deallocate(wavefunction_section_)
  !
  allocate(wf_igk_tmp(ng_vec_abinit,nkibz),wf_nc_k(nkibz))
  wf_igk_tmp=-1
  wf_nc_k=0
  !
  do ik=1,nkibz
    ic=0
    do ig=1,ng_vec_abinit
      if(all(wf_disk(:,ig,ik)==0)) cycle
      ic=ic+1
      wf_igk_tmp(ic,ik)=ig
    enddo
    wf_nc_k(ik)=ic
  enddo
  !
  wf_ncx=maxval(wf_nc_k)
  wf_ng=maxval(wf_igk_tmp)
  !
  allocate(wf_igk(wf_ncx,nkibz))
  wf_igk=-1
  !
  do ik=1,nkibz
    wf_igk(1:wf_ncx,ik)=wf_igk_tmp(1:wf_ncx,ik)
  enddo
  !
  deallocate(wf_igk_tmp)
  ! 
  return
  !
end subroutine e2y_wf_components
