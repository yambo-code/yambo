!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine K_set_bands_ranges_rt(Ken,V_bands,C_bands)
 !
 use electrons,    ONLY:levels
 use BS,           ONLY:BS_bands
 use electrons,    ONLY:n_full_bands,n_met_bands
 use drivers,      ONLY:l_rt_carriers_in_use
 use BS,           ONLY:EQ_Transitions,EQ_NoOcc
 !
 implicit none
 !
 type(levels), intent(in) :: Ken
 integer,     intent(out) :: V_bands(2),C_bands(2)
 !
 ! Bands range. In the _RT case %nbf and %nbm turn metallic when carriers are used.
 if (l_rt_carriers_in_use.and.EQ_Transitions) then
   V_bands=(/BS_bands(1),maxval(n_met_bands)/)
   C_bands=(/minval(n_full_bands)+1,BS_bands(2)/)
 endif
 if (EQ_NoOcc) then
   V_bands=BS_bands
   C_bands=BS_bands
 endif
 !
end subroutine
