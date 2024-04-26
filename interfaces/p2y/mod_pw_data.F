!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CH
!   
module pw_data
  use pars,              only : DP
  save
  public
  
! Cell
  real(DP)                   :: alat_, a1_(3), a2_(3), a3_(3)

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
  logical                    :: nosym_,noinv_

! Planewaves
  integer                    :: npwx_
  integer                    :: nr1_,nr2_,nr3_,ngm_
  integer                    :: nr1s_,nr2s_,nr3s_,ngms_
  real(DP)                   :: ecutwfc_
  real(DP)                   :: ecutrho_
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
  character(256)             :: pw_exxdiv_treatment
  real(dp)                   :: pw_exx_fraction,pw_screening_parameter
  logical                    :: pw_exx_is_active

! Pseudopotentials
  character(256),allocatable :: psfile(:)
  integer, allocatable       :: nmesh(:), nbeta(:)
  integer, allocatable       :: nqf(:), nqlc(:), lmax(:)

end module pw_data
