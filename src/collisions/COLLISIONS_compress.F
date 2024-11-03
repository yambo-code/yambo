!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
! This subroutine works directly with collisions in single-precision
! if present
!
subroutine COLLISIONS_compress(COLL)
 !
 use pars,            ONLY:SP,DP,SP6,cZERO
 use collision_ext,   ONLY:COLLISIONS_element,COLL_bands,COLLISIONS_cutoff
 use R_lattice,       ONLY:nXkibz
 !
#include<y_memory.h>
 !
 type(COLLISIONS_element), intent(inout) :: COLL
 !
 ! Work Space
 !
 real(SP)  :: COLLISIONS_max
 integer   :: ib,ibp,i_kmq,ic
 logical   :: l_coll_SP
 !
 if(allocated(COLL%v_c_SP)) then
   COLLISIONS_max=maxval(abs(COLL%v_c_SP(:)))        
 else
   COLLISIONS_max=maxval(abs(COLL%v_c(:)))        
 endif
 !
 l_coll_SP=.FALSE.
 if(SP==DP.and.allocated(COLL%v_c_SP)) l_coll_SP=.TRUE.
 !
 ! Search for non-zero collisions
 !
 YAMBO_ALLOC(COLL%v3_c,(COLL_bands(1):COLL_bands(2),COLL_bands(1):COLL_bands(2),1:nXkibz))
 !
 ic=0
 !
 COLL%table='t'
 do i_kmq=1,nXkibz
   do ibp=COLL_bands(1),COLL_bands(2)
     do ib=COLL_bands(1),COLL_bands(2)
       !
       ic=ic+1
       !
       if(l_coll_SP)      COLL%v3_c(ib,ibp,i_kmq)=cmplx(COLL%v_c_SP(ic),kind=SP)
       if(.not.l_coll_SP) COLL%v3_c(ib,ibp,i_kmq)=COLL%v_c(ic)
       !
       if(abs(COLL%v3_c(ib,ibp,i_kmq))<COLLISIONS_max*COLLISIONS_cutoff)  COLL%table(ib,ibp,i_kmq)='f'
       !
     enddo
   enddo
 enddo
 !
 COLL%N=count(COLL%table(:,:,:)=='t')
 !      
 if(l_coll_SP) then
   YAMBO_FREE(COLL%v_c_SP)
   YAMBO_ALLOC(COLL%v_c_SP,(COLL%N))
 else
   YAMBO_FREE(COLL%v_c)
   YAMBO_ALLOC(COLL%v_c,(COLL%N))
 endif
 !
 ! Fill collisions again
 !
 ic=0
 do i_kmq=1,nXkibz
   do ibp=COLL_bands(1),COLL_bands(2)
     do ib=COLL_bands(1),COLL_bands(2)
       !
       if(COLL%table(ib,ibp,i_kmq)=='t') then
         ic=ic+1
         if(l_coll_SP)      COLL%v_c_SP(ic)=cmplx(COLL%v3_c(ib,ibp,i_kmq),kind=SP6)
         if(.not.l_coll_SP) COLL%v_c(ic)   =COLL%v3_c(ib,ibp,i_kmq)
         !
       endif
       !
     enddo
   enddo
 enddo
 !
 YAMBO_FREE(COLL%v3_c)
 !
end subroutine COLLISIONS_compress
