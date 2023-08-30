!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AF IM
!
module control_flags
  logical :: gamma_only
  logical :: smallmem=.false.
  logical :: tqr=.false.
end module

module io_global
  logical :: ionode=.true.
  integer :: stdout=6
end module

module io_files
  use parameters, ONLY: ntypx
  private
  character(256) :: pseudo_dir
  character(256) :: psfile(ntypx)
  public :: pseudo_dir, psfile
end module

module lsda_mod
  integer :: nspin
end module

module noncollin_module
  logical :: noncolin
  integer :: npol
  integer :: nspin_mag
end module

module ions_base
  use kinds,      ONLY: DP
  use parameters, ONLY: ntypx
  private
  integer :: nat
  integer :: nsp
  integer, allocatable :: ityp(:)
  ! internally, tau in alat units
  real(DP),allocatable :: tau(:,:)
  REAL(DP) :: zv(ntypx)    = 0.0_DP
  !
  public :: nat, nsp, ityp, tau, zv
end module

module cell_base
  use kinds, only : DP
  private
  ! bohr units
  real(DP) :: alat
  real(DP) :: omega
  ! bohr^-1 units
  real(DP) :: tpiba
  public :: omega, tpiba, alat
end module

module cellmd
  use kinds, only : DP
  private
  real(DP) :: cell_factor = 1.1_DP
  public :: cell_factor
end module

module klist
  use kinds, only : DP
  private
  real(DP) :: qnorm=0.0_DP
  public :: qnorm
end module

module wvfct
  integer :: npwx
end module

module gvecw
  use kinds, only : DP
  private
  ! tpiba^2 units
  real(DP) :: gcutw
  ! Ry-au
  real(DP) :: ecutwfc
  public :: gcutw
  public :: ecutwfc
end module

module symm_base
  use kinds, only : DP
  integer  :: nsym
  real(DP) :: &
       d1(3,3,48),           &! matrices for rotating spherical
       d2(5,5,48),           &! harmonics (d1 for l=1, ...)
       d3(7,7,48)             !
end module
