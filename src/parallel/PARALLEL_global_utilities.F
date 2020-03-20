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
#if defined _SC || defined _RT
!
subroutine G_b_to_B_mat(pCOMM,what,nb)
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
!
#endif
!
subroutine distribute_BZk_using_IBZk(COMM,K,IND_ibz,IBZ_id,IND_out,BZ_id,BZ_index,PAR_n_bz)
 !
 use R_lattice,     ONLY:bz_samp
 use parallel_m,    ONLY:PP_indexes,MPI_comm
 !
#include <memory.h>
 !
 type(MPI_comm)   :: COMM
 type(PP_indexes) :: IND_ibz,IND_out
 type(bz_samp)    :: K
 !
 integer          :: IBZ_id,BZ_id,BZ_index(K%nbz),PAR_n_bz
 !
 ! Work Space
 integer :: i_bz,i_ibz,i_p
 !
 ! K-points in the BZ 
 !
 YAMBO_ALLOC(IND_out%element_1D,(K%nbz))
 YAMBO_ALLOC(IND_out%n_of_elements,(COMM%n_CPU))
 IND_out%element_1D=.FALSE.
 ! 
 BZ_index=0
 !
 BZ_id=IBZ_id
 !
 i_p=0
 !
 do i_bz=1,K%nbz
   !
   i_ibz=K%sstar(i_bz,1)
   !
   if (IND_ibz%element_1D(i_ibz)) then
     i_p=i_p+1
     IND_out%element_1D(i_bz)=.TRUE.
     BZ_index(i_bz)=i_p
   else
     IND_out%element_1D(i_bz)=.FALSE.
   endif
   !
 enddo
 !
 PAR_n_bz=i_p
 !
 IND_out%n_of_elements(BZ_ID+1)=PAR_n_bz
 !
end subroutine
