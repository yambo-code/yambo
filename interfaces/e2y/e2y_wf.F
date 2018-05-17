!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine e2y_wf(wf_disk,i_sp_pol,ikibz,ib_grp, ncid)
!==============================================
  ! Reads and returns wf_disk for both spins!
  use etsf_io
  use etsf_io_low_level
  use etsf_data,             only : dims,ETSF_kind
  use pars,                  only : SP,cZERO
  use vec_operate,           only : sort
  use com,                   only : msg, error
  use electrons,             only : n_bands, n_sp_pol, n_spinor
  use R_lattice,             only : nkibz
  use wave_func,             only : wf_ncx,wf_nc_k, wf_igk,wf_nb_io,wf_nb_io_groups
  !
  implicit none
  !
  integer,           intent(in)  :: i_sp_pol,ikibz, ncid, ib_grp
  complex(SP),       intent(out) :: wf_disk(wf_ncx,n_spinor,wf_nb_io)
  !
  double precision, allocatable  :: wavefunction_section_(:,:,:,:)
  logical                        :: lstat
  type(etsf_io_low_error)        :: error_data 
  ! 
  ! Work Space
  !
  integer        :: ib, ierr, npwk, ik, ig, ic, i_spinor, ireal, nb_to_read
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
&                                dims%max_number_of_coefficients,   &
&                                dims%number_of_spinor_components,  &
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
  start(:) = 1        ; count(:) = 0
  start(5) = ikibz    ; count(5) = 1   ! Split on   k    always
  start(6) = i_sp_pol ; count(6) = 1   ! Split on sp_pol always
  !
  if(ETSF_kind=='WFK') call sort(wf_igk(:wf_nc_k(ikibz),ikibz),indx=wf_igk_indx)
  !
  call etsf_io_low_read_var(ncid, "coefficients_of_wavefunctions", &
                          & wavefunction_section_, lstat,          &
                          & error_data = error_data, start = start, count = count)
  !
  do ib=1,n_bands
    do i_spinor=1,n_spinor
      do ic=1,wf_nc_k(ikibz)
        if(ETSF_kind=='KSS') ig=wf_igk(ic,ikibz)
        if(ETSF_kind=='WFK') ig=wf_igk_indx(ic)
        wf_disk(ic,i_spinor,ib) = cmplx(wavefunction_section_(1,ig,i_spinor,ib),wavefunction_section_(2,ig,i_spinor,ib),SP)
      enddo
    enddo
  enddo
  !
  deallocate(wavefunction_section_)
  ! 
  return
  !
end subroutine e2y_wf
