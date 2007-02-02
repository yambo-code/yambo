!
! Copyright (C) 2000-2005 C. Hogan and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
module qexml_data
  use qexml_module,      ONLY : dbl
  private                    :: dbl 
  save
  public
  
! Cell
  real(dbl)                  :: alat_, a1_(3), a2_(3), a3_(3)

! Atoms
  integer                    :: nat_, nsp_
  real(dbl), allocatable     :: tau_(:,:)
  integer, allocatable       :: ityp_(:)
  character(3), allocatable  :: atm_(:)

! Symmetry
  integer, allocatable       :: s_(:,:,:)
  integer                    :: nsym_
  logical                    :: invsym_

! Planewaves
  integer                    :: ngm_, npwx_
  integer, allocatable       :: igv_(:,:)

! Spin
  logical                    :: lspinorb_

! Bandstructure
  integer                    :: nbnd_, num_k_points_, nspin_
  real(dbl)                  :: nelec_
  real(dbl), allocatable     :: eig_s_(:,:,:), eig_(:,:)
  real(dbl), allocatable     :: xk_(:,:)

! igk grids
  integer                    :: npwk_
  integer, allocatable       :: index_(:), igk_(:,:)

! Wavefunctions
  complex(dbl), allocatable  :: wf_kindip_(:,:)

end module qexml_data
