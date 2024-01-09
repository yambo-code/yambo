!
! License-Identifier: GPL
!
! Copyright (C) 2012 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine INTERPOLATION_driver_end(ID)
 !
 use interpolate_tools,    ONLY:INTERPOLATION_reset
 use interpolate,          ONLY:INTERPOLATE_msg_fmt,INTERPOLATE_initialized,&
&                               lattice_vectors,INTERP_obj,max_interpls,int_sop,NN_nk_exact
 !
#include<memory.h>
 !
 integer, intent(in) :: ID
 !
 ! Work space
 !
 integer             :: i_obj
 !
 if (ID>0) then
   !
   ! Reset only specific ID
   !
   if (.not.INTERP_obj(ID)%in_use) return 
   call INTERPOLATION_reset(ID)
   !
 else
   !
   ! Reset all IDs and clear lattice vectors
   !
   do i_obj=1,max_interpls
     call INTERPOLATION_reset(i_obj)
   enddo
   !
   YAMBO_FREE_P(lattice_vectors)
   !
   YAMBO_FREE(int_sop)
   INTERPOLATE_initialized=.FALSE.
   INTERPOLATE_msg_fmt="rs"
   !
 endif
 !
 NN_nk_exact=0
 !
end subroutine INTERPOLATION_driver_end
