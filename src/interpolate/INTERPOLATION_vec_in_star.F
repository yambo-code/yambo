!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CA AM
!
logical function INTERPOLATION_vec_in_star(vec,nstar,star_vec)
 !
 use pars,         ONLY:SP
 use vec_operate,  ONLY:v_is_zero
 implicit none
 integer,  intent(in)  :: nstar
 real(SP), intent(in)  :: vec(3),star_vec(3,nstar)
 !
 ! Work Space
 !
 integer         :: i1
 !---------------------------------------------------------------------  
 INTERPOLATION_vec_in_star=.FALSE.
 do i1=1,nstar
   if(v_is_zero(star_vec(:,i1)-vec(:))) INTERPOLATION_vec_in_star=.TRUE.
 enddo
 !
end function INTERPOLATION_vec_in_star
