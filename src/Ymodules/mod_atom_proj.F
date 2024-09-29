!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
module atom_proj
 !
 use pars,      ONLY:SP
 !
 implicit none
 !
 integer  :: n_atom_wfc
 integer  :: n_proj_bands
 !
 integer,     allocatable :: atomic_table(:,:)
 !
 complex(SP), allocatable :: atomic_projections(:,:,:,:)
 !
end module

