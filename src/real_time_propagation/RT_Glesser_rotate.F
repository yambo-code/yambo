!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_Glesser_rotate(dG)
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:RT_bands,RT_nk
 !
 implicit none
 !
 complex(SP), intent(in) :: dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Work Space
 !
 integer                 :: ik
 !
 do ik=1,RT_nk
   !
 enddo
 !if (.not.l_RT_eval_lifetimes.and..not..RT_do_it('OUT')) return
 !
 !call OBS_rotate(R,O,N,direction)
 !
end subroutine RT_Glesser_rotate
