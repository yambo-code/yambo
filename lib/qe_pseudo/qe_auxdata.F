
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
  character(256) :: pseudo_dir
  character(256) :: psfile(ntypx)
end module

module lsda_mod
  integer :: nspin
end module

module noncollin_module
  logical :: noncolin
  integer :: npol
end module

module ions_base
  use kinds,      ONLY: DP
  use parameters, ONLY: ntypx
  integer :: nat
  integer :: nsp
  integer, allocatable :: ityp(:)
  ! internally, tau in alat units
  real(DP),allocatable :: tau(:,:)
  REAL(DP) :: zv(ntypx)    = 0.0_DP
end module

module cell_base
  use kinds, only : DP
  real(DP) :: omega
  real(DP) :: tpiba
end module

module cellmd
  use kinds, only : DP
  real(DP) :: cell_factor = 1.1_DP
end module

module klist
  use kinds, only : DP
  real(DP) :: qnorm=0.0_DP
end module

module wvfct
  integer :: npwx
end module

module gvecw
  use kinds, only : DP
  ! tpiba^2 units
  real(DP) :: gcutw
  ! Ry-au
  real(DP) :: ecutwfc
end module
