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
subroutine COLLISIONS_compress(COLL_element)
 !
 use pars,            ONLY:SP,DP,SP6,cZERO
 !
 implicit none                                              
 !
#include<collisions.h>
 !
 ! Work Space
 !
 real(SP)  :: COLLISIONS_max
 !
 if(allocated(COLL_element%v_c_SP)) then
   COLLISIONS_max=maxval(abs(COLL_element%v_c_SP(:)))        
 else
   COLLISIONS_max=maxval(abs(COLL_element%v_c(:)))        
 endif
 !
 ic=0
 !
 do i3=COLL%D(3,1),COLL%D(3,2)
   do i2=COLL%D(2,1),COLL%D(2,2)
     do i1=COLL%D(1,1),COLL%D(1,2)
       !
       ic=ic+1
       !
       if(allocated(COLL_element%v_c_SP)) then
         COLL_element%v3_c(i1,i2,i3)=COLL_element%v_c_SP(ic)
       else
         COLL_element%v3_c(i1,i2,i3)=COLL_element%v_c(ic)
       endif
       !
       if(abs(COLL_element%v3_c(i1,i2,i3))<COLLISIONS_max*COLLISIONS_cutoff) then
           COLL_element%table(i1,i2,i3)=.FALSE.
       endif
       !
     enddo
   enddo
 enddo
 !
 COLL_element%N=count(COLL_elements%table(:,:,:).eqv..TRUE.)
 !

 !
end subroutine COLLISIONS_compress
