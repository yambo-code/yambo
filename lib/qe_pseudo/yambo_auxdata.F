
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

module control_flags
  logical :: gamma_only
  logical :: smallmem=.false.
end module

module ions_base
  use kinds,      ONLY: DP
  use parameters, ONLY: ntypx
  integer :: nat
  integer :: nsp
  integer, allocatable :: ityp(:)
  real(DP),allocatable :: tau(:,:)
  REAL(DP) :: zv(ntypx)    = 0.0_DP
end module

module cell_base
  use kinds, only : DP
  real(DP) :: omega
  real(DP) :: tpiba
end module

module wvfct
  integer :: npwx
end module
