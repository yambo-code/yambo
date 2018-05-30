!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, IM
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
  ! bohr^3 units
  real(DP) :: omega
  ! bohr^-1 units
  real(DP) :: tpiba
  public :: omega, tpiba
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

