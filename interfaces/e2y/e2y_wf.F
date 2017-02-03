!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH DS
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
subroutine e2y_wf(wf_disk,ikibz, ib_grp, ncid)
!==============================================
  ! Reads and returns wf_disk for both spins!
  use etsf_io
  use etsf_io_low_level
  use etsf_data,             only : dims,ETSF_kind
  use pars,                  only : SP
  use vec_operate,           only : sort
  use com,                   only : msg, error
  use electrons,             only : n_bands, n_spin, n_sp_pol, n_spinor
  use R_lattice,             only : nkibz
  use wave_func,             only : wf_ncx,wf_nc_k, wf_igk,wf_nb_io,wf_nb_io_groups
  !
  implicit none
  !
  integer,           intent(in)  :: ikibz, ncid, ib_grp
  real(SP),          intent(out) :: wf_disk(2,n_bands,wf_ncx,n_spin)
  !
  double precision, allocatable  :: wavefunction_section_(:,:,:)
  logical                        :: lstat
  type(etsf_io_low_error)        :: error_data 
  ! 
  ! Work Space
  !
  integer        :: ib, ierr, npwk, ik, ic, ispin, ireal, nb_to_read
  integer        :: start(6), count(6), ncvarid
  integer        :: wf_igk_indx(wf_nc_k(ikibz))
  !
  type(etsf_main)   :: main_group
!
!---------------------------------------------------------------------*
!    Main data                                                        *
!---------------------------------------------------------------------*

!  allocate(coefficients_of_wavefunctions_(dims%real_or_complex, &
!&                                         dims%max_number_of_coefficients,    &
!&                                         dims%number_of_spinor_components,   &
!&                                         dims%max_number_of_states,          &
!&                                         1,                                  &
!&                                         dims%number_of_spins )  )            
! main_group%coefficients_of_wavefunctions%k_splitted = .true.
! main_group%coefficients_of_wavefunctions%spin_splitted = .true.
! main_group%coefficients_of_wavefunctions%k_id = ikibz

! Using low level routine to select the spin AND spinor as wanted.
! No high level option to read wavefunction spinor components: the choice
! was made to keep high level routines split on nkibz and n_sp_pol
  allocate(wavefunction_section_(dims%real_or_complex_coefficients, &
&                                dims%max_number_of_coefficients,    &
&                                dims%max_number_of_states ) )
  !
  ! Read all bands or only the remaining in the last block
  !
  nb_to_read=wf_nb_io
  if (ib_grp*wf_nb_io>n_bands) nb_to_read=n_bands-wf_nb_io*(ib_grp-1)
  ! Note: Splitting over bands does not presently work, since
  ! etsf-nc files may also be split, and things get confusing.
  if (nb_to_read.ne.n_bands) call error('Splitting over bands does not currently work for etsf-nc.'//&
& '  Contact developers.')
  nb_to_read = n_bands
  !
  ! Initialize the mapping
  !
  start(:) = 1     ; count(:) = 0
  start(5) = ikibz ; count(5) = 1   ! Split on k always
  !
  if(ETSF_kind=='WFK') call sort(wf_igk(:wf_nc_k(ikibz),ikibz),indx=wf_igk_indx)
  !
  do ispin = 1, n_spin
    !
    ! Select the part of the array to split on
    !
    if(n_spinor==2) then
      start(3) = ispin
      count(3) = 1
    else if(n_sp_pol==2) then
      start(6) = ispin
      count(6) = 1
    endif
    !
    call etsf_io_low_read_var(ncid, "coefficients_of_wavefunctions", &
                            & wavefunction_section_, lstat,          &
                            & error_data = error_data, start = start, count = count)
    !
    if(ETSF_kind=='KSS') then
      forall( ireal=1:2, ib=1:n_bands, ic=1:wf_nc_k(ikibz) ) &
&          wf_disk(ireal,ib,ic,ispin) = wavefunction_section_(ireal,wf_igk(ic,ikibz),ib)
    endif
    !
    if(ETSF_kind=='WFK') then
      forall( ireal=1:2, ib=1:n_bands, ic=1:wf_nc_k(ikibz) ) &
&          wf_disk(ireal,ib,ic,ispin) = wavefunction_section_(ireal,wf_igk_indx(ic),ib)
    endif
    !
  enddo
  !
  deallocate(wavefunction_section_)
  ! 
  return
  !
end subroutine e2y_wf
