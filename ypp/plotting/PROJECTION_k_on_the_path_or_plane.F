! 
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function PROJECTION_k_on_the_path_or_plane(ik)
 !
 use YPPm,          ONLY:PROJECT_N_perp
 !
 integer :: ik
 !
 if (PROJECT_N_perp==1) then
   PROJECTION_k_on_the_path_or_plane=ik
   return
 endif
 if (mod(ik,PROJECT_N_perp)/=1) then
   PROJECTION_k_on_the_path_or_plane=0
   return
 endif
 PROJECTION_k_on_the_path_or_plane=ik/PROJECT_N_perp+1
 !
end function
