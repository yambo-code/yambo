!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine FREQUENCIES_reset(Xw,what)
 !
 ! Input
 !-------
 ! integer     :: npts
 ! real(SP)    :: bg_pt(npts),cg_percentual
 !
 ! Deallocate:  
 ! bg_pt(:), cg_pt(:), cg_index_bg(:),
 ! X_poles_tab(:), rg_index_bg(:), bg_npts(:)
 !
 use frequency,   ONLY:w_samp,bare_grid_N,coarse_grid_Pt,ordered_grid_index,coarse_grid_index
 use X_m,         ONLY:X_poles_tab
#include<memory.h>
 !
 type(w_samp)  :: Xw
 character(*)  :: what
 !
 if (what=="points".or.what=="all") then
   YAMBO_FREE(Xw%p)
 endif
 !
 if (what=="coarse_grid".or.what=="all") then
   YAMBO_FREE(ordered_grid_index)
   if (allocated(X_poles_tab)) then
     YAMBO_FREE(X_poles_tab)
     YAMBO_FREE(bare_grid_N)
     YAMBO_FREE(coarse_grid_Pt)
     YAMBO_FREE(coarse_grid_index)
   endif
 endif
 !
end subroutine
