!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, AF
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
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
subroutine SLK_matrix_init(M_slk, N)
 !
 use SLK_m,          ONLY:SLK_ORTHO
 use parallel_m,     ONLY:ncpu
 use linear_algebra, ONLY:PAR_matrix,MATRIX_reset 
#include<memory.h>
 !
 integer           :: N
 type(PAR_matrix)  :: M_slk
 !
 ! Work Space
 !
 integer   :: lld,N_blcs,nprow,npcol,myrow,mycol
 !
 ! init global blacs grid
 !
 if (ncpu==1) then
   SLK_ORTHO%coordinate(1)=-1
   M_slk%rows             = (/myrow*N_blcs+1,min(myrow*N_blcs+N_blcs,N)/)
   M_slk%cols             = (/mycol*N_blcs+1,min(mycol*N_blcs+N_blcs,N)/)
   M_slk%nrows_used       = N
   M_slk%ncols_used       = N
   M_slk%N                = N
   return
 endif
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
 N_blcs = int(N/nprow)
 if (N_blcs*nprow < N ) N_blcs=N_blcs+1
 !
 lld = N_blcs
 !
 call DESCINIT( M_slk%desc, N, N, N_blcs, N_blcs, 0, 0, SLK_ORTHO%ortho_cntx, lld, M_slk%info )
 !
 call MATRIX_reset(M_slk)
 !
 M_slk%rows       = (/myrow*N_blcs+1,min(myrow*N_blcs+N_blcs,N)/)
 M_slk%cols       = (/mycol*N_blcs+1,min(mycol*N_blcs+N_blcs,N)/)
 M_slk%nrows      = M_slk%rows(2)-M_slk%rows(1)+1
 M_slk%ncols      = M_slk%cols(2)-M_slk%cols(1)+1
 M_slk%nrows_used = M_slk%nrows
 M_slk%ncols_used = M_slk%ncols
 M_slk%N          = N
 !
end subroutine 
