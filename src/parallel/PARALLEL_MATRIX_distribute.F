!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine PARALLEL_MATRIX_distribute(pCOMM,what,nb)
 !
 use parallel_m,    ONLY:MPI_comm
 use interfaces,    ONLY:PARALLEL_index
 use hamiltonian,   ONLY:B_mat_index
 ! IND
 use parallel_m,    ONLY:PAR_IND_B_mat,PAR_IND_Bp_mat
 ! INDEX
 use parallel_m,    ONLY:PAR_B_mat_index,PAR_Bp_mat_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_n_B_mat_elements,PAR_n_Bp_mat_elements
 ! ID's
 use parallel_m,    ONLY:PAR_IND_B_mat_ID,PAR_IND_Bp_mat_ID
 !
#include <memory.h>
 !
 type(MPI_comm) :: pCOMM
 character(*)   :: what
 integer        :: nb(2)
 !
 ! Work space
 !
 integer        :: ib1,ib2
 !
 if (what=="B") then
   call PARALLEL_index(PAR_IND_B_mat,(/ (nb(2)-nb(1)+1)**2 /),COMM=pCOMM,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_B_mat_ID=pCOMM%CPU_id
 else
   call PARALLEL_index(PAR_IND_Bp_mat,(/ (nb(2)-nb(1)+1)**2 /),COMM=pCOMM,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_Bp_mat_ID=pCOMM%CPU_id
 endif
 !
 if (what=="B") then
   YAMBO_ALLOC(PAR_B_mat_index,(nb(1):nb(2),nb(1):nb(2)))
   PAR_n_B_mat_elements=PAR_IND_B_mat%n_of_elements(PAR_IND_B_mat_ID+1)
   PAR_B_mat_index=0
   PAR_n_B_mat_elements=0
   do ib1=nb(1),nb(2)
     do ib2=nb(1),nb(2)
       if (PAR_IND_B_mat%element_1D( B_mat_index(ib1,ib2,nb) ) ) then
         PAR_n_B_mat_elements=PAR_n_B_mat_elements+1
         PAR_B_mat_index(ib1,ib2)=PAR_n_B_mat_elements
       endif
     enddo
   enddo
 else
   YAMBO_ALLOC(PAR_Bp_mat_index,(nb(1):nb(2),nb(1):nb(2)))
   PAR_n_Bp_mat_elements=PAR_IND_Bp_mat%n_of_elements(PAR_IND_Bp_mat_ID+1)
   PAR_Bp_mat_index=0
   PAR_n_Bp_mat_elements=0
   do ib1=nb(1),nb(2)
     do ib2=nb(1),nb(2)
       if (PAR_IND_Bp_mat%element_1D( B_mat_index(ib1,ib2,nb) ) ) then
         PAR_n_Bp_mat_elements=PAR_n_Bp_mat_elements+1
         PAR_Bp_mat_index(ib1,ib2)=PAR_n_Bp_mat_elements
       endif
     enddo
   enddo
 endif
 !
end subroutine
