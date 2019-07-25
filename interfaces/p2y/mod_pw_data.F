!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH
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
module pw_data
  use pars,              only : DP
  save
  public
  
! Cell
  real(DP)                  :: alat_, a1_(3), a2_(3), a3_(3)

! Atoms
  integer                    :: nat_, nsp_
  real(DP), allocatable      :: tau_(:,:)
  integer, allocatable       :: ityp_(:)
  character(3), allocatable  :: atm_(:)
  character(len=2), allocatable   :: atom_type_(:), species_type_(:)

! Symmetry
  integer, allocatable       :: s_(:,:,:)
  integer                    :: nsym_, isym_(3,3,48)
  real(DP)                   :: rsym_(3,3,48)
  integer                    :: t_rev_(48)
  logical                    :: invsym_
  logical                    :: trevsym_

! Planewaves
  integer                    :: ngm_, npwx_
  real(DP)                   :: ecutwfc_
  integer, allocatable       :: igv_(:,:)
  logical                    :: gamma_only_

! Spin
  logical                    :: lspinorb_
  logical                    :: noncolin_
  integer                    :: n_spin_pw_

! Bandstructure
  integer                    :: nbnd_, num_k_points_, nspin_
  real(DP)                   :: nelec_
  real(DP), allocatable      :: eig_s_(:,:,:), eig_(:,:), eigb_(:)
  real(DP), allocatable      :: xk_(:,:)

! Smearing parameters
  logical                    :: lgauss_
  integer                    :: ngauss_
  character(256)             :: smearing_type_    ! XSD: alternative to ngauss_
  character(256)             :: degauss_units_
  real(dp)                   :: degauss_

! igk grids
  integer                    :: npwk_, nfft_(3)
  integer, allocatable       :: index_(:), igkv_(:,:), pw_npwk_(:)
  integer, allocatable       :: fft2igv_(:)
  integer, allocatable       :: pw_igk_(:,:)

! Wavefunctions
  complex(DP), allocatable   :: wf_kindip_(:,:)

! XC
  character(256)             :: pw_dft
  logical                    :: pw_lda_plus_u

! EXX part
  real(dp)                   :: pw_exx_fraction,pw_screening_parameter
  logical                    :: pw_exx_is_active

! Pseudopotentials
  character(256),allocatable :: psfile(:)
  integer, allocatable       :: nmesh(:), nbeta(:)

end module pw_data
