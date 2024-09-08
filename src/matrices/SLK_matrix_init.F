!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
!
subroutine SLK_matrix_init(M_slk, N, N_blocks)
 !
 use SLK_m,          ONLY:SLK_ORTHO
 use matrix,         ONLY:PAR_matrix
#include<y_memory.h>
 !
 integer           :: N,N_blocks
 type(PAR_matrix)  :: M_slk
 !
 ! Work Space
 !
 integer   :: lld,SLK_block_size,nprow,npcol,myrow,mycol
 !
 ! init global blacs grid
 !
 call BLACS_GRIDINFO( SLK_ORTHO%ortho_cntx, SLK_ORTHO%grid(1), SLK_ORTHO%grid(2), SLK_ORTHO%coordinate(1), SLK_ORTHO%coordinate(2) )
 !
 nprow=SLK_ORTHO%grid(1)
 npcol=SLK_ORTHO%grid(2)
 myrow=SLK_ORTHO%coordinate(1)
 mycol=SLK_ORTHO%coordinate(2)
 !
 ! spectator tasks
 ! only MPI tasks in the grid need to go ahead
 !
 if (SLK_ORTHO%coordinate(1) == -1) return
 !
 ! distribute the matrix on the process grid
 ! Initialize the array descriptors for the matrices A and B
 !
 SLK_block_size                               = int(N/nprow)
 if (SLK_block_size*nprow < N ) SLK_block_size=SLK_block_size+1
 !
 lld = SLK_block_size
 !
 call DESCINIT( M_slk%desc, N, N, SLK_block_size, SLK_block_size, 0, 0, SLK_ORTHO%ortho_cntx, lld, M_slk%info )
 !
 M_slk%rows       = (/myrow*SLK_block_size+1,min(myrow*SLK_block_size+SLK_block_size,N)/)
 M_slk%cols       = (/mycol*SLK_block_size+1,min(mycol*SLK_block_size+SLK_block_size,N)/)
 M_slk%nrows      = M_slk%rows(2)-M_slk%rows(1)+1
 M_slk%ncols      = M_slk%cols(2)-M_slk%cols(1)+1
 M_slk%BLCrows    = (/M_slk%rows(1),M_slk%rows(1)+SLK_block_size-1/)
 M_slk%BLCcols    = (/M_slk%cols(1),M_slk%cols(1)+SLK_block_size-1/)
 M_slk%BLCnrows   = SLK_block_size
 M_slk%BLCncols   = SLK_block_size  
 M_slk%N          = N
 M_slk%I          = 1
 M_slk%Nb         = N_blocks
 M_slk%kind       = "SLK"
 !
end subroutine 
