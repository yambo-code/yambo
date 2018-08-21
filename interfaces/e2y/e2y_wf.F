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
  !
  ! Reads and returns wf_disk for both spins
  !
  use netcdf
  use etsf_data,             only : ETSF_kind,netcdf_check
  use pars,                  only : SP,cZERO
  use vec_operate,           only : sort
  use com,                   only : msg, error
  use electrons,             only : n_bands, n_sp_pol, n_spinor
  use R_lattice,             only : nkibz
  use wave_func,             only : wf_ncx,wf_nc_k, wf_igk,wf_nb_io,wf_nb_io_groups
  !
  implicit none
  !
  integer,           intent(in)  :: i_sp_pol,ikibz, ib_grp, ncid
  complex(SP),       intent(out) :: wf_disk(wf_ncx,n_spinor,wf_nb_io)
  !
  real(SP), allocatable  :: wavefunction_section_(:,:,:,:)
  ! 
  ! Work Space
  !
  integer        :: ib, ierr, npwk, ik, ig, ic, i_spinor, ireal
  integer        :: start(6), count(6), ncvarid
  integer        :: wf_igk_indx(wf_nc_k(ikibz))
  integer        :: varid, netcdf_error
  double precision, allocatable, target :: coefficients_of_wavefunctions_(:,:,:,:,:,:)
  !
  allocate(wavefunction_section_(2, wf_ncx, n_spinor, n_bands) )
  !
  start = [1,1,1,1,ikibz,i_sp_pol]
  count = [2,wf_ncx,n_spinor,n_bands,1,1]
  !
  netcdf_error = nf90_inq_varid(ncid, "coefficients_of_wavefunctions", varid)
  netcdf_error = nf90_get_var(ncid, varid, wavefunction_section_, start, count)
  !
  if(ETSF_kind=='WFK') then
    call sort(wf_igk(:wf_nc_k(ikibz),ikibz),indx=wf_igk_indx)
  else
    wf_igk_indx = wf_igk(:wf_nc_k(ikibz),ikibz)
  endif
  !
  do ib=1,n_bands
    do i_spinor=1,n_spinor
      do ic=1,wf_nc_k(ikibz)
        ig=wf_igk_indx(ic)
        wf_disk(ic,i_spinor,ib) = cmplx(wavefunction_section_(1,ig,i_spinor,ib),&
&                                       wavefunction_section_(2,ig,i_spinor,ib),SP)
      enddo
    enddo
  enddo
  !
  deallocate(wavefunction_section_)
  ! 
  return
  !
end subroutine e2y_wf
