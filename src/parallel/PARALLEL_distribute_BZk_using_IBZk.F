!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_distribute_BZk_using_IBZk(COMM,K,IND_ibz,IBZ_id,IND_out,BZ_id,BZ_index,PAR_n_bz)
 !
 use R_lattice,     ONLY:bz_samp
 use parallel_m,    ONLY:PP_indexes,yMPI_comm
 !
#include <memory.h>
 !
 type(yMPI_comm)   :: COMM
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
