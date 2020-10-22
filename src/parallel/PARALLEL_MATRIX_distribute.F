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
subroutine PARALLEL_MATRIX_distribute(COMM,PAR_IND,nb,PAR_index,PAR_ID,PAR_n_elements)
 !
 use parallel_m,    ONLY:MPI_comm,PP_indexes
 use interfaces,    ONLY:PARALLEL_index
 use hamiltonian,   ONLY:B_mat_index
 !
 implicit none
 !
 type(MPI_comm)   :: COMM
 type(PP_indexes) :: PAR_IND
 integer          :: nb(2)
 !
 integer, optional:: PAR_index(:,:)
 integer, optional:: PAR_ID
 integer, optional:: PAR_n_elements
 !
 ! Work space
 !
 integer        :: ib1,ib2,Nel
 !
 call PARALLEL_index(PAR_IND,(/ (nb(2)-nb(1)+1)**2 /),COMM=COMM,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 !
 if (present(PAR_ID)) then
   PAR_ID=COMM%CPU_id
 endif
 !
 if (present(PAR_index)) then
   Nel=PAR_IND%n_of_elements(COMM%CPU_id+1)
   PAR_index=0
   Nel=0
   do ib1=nb(1),nb(2)
     do ib2=nb(1),nb(2)
       if (PAR_IND%element_1D( B_mat_index(ib1,ib2,nb) ) ) then
         Nel=Nel+1
         PAR_index(ib1,ib2)=Nel
       endif
     enddo
   enddo
 endif
 !
 if (present(PAR_n_elements)) then
   PAR_n_elements=PAR_IND%n_of_elements(COMM%CPU_id+1)
 endif
 !
end subroutine
