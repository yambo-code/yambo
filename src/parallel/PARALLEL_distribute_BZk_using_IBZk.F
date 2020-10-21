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
subroutine PARALLEL_distribute_BZk_using_IBZk(COMM,K,IND_ibz,IBZ_id,IND_out,BZ_id,BZ_index,PAR_n_bz)
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
