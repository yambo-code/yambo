!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine RT_build_T1_and_T2(ik,T1_k,T2_k,I1_matrix,dT)
 !
 ! dG(T) = G(T) - G(0)
 !
 ! T1 = (1-iHdt/2) / (1+iHdt/2)-1
 ! T2 = (1+iHdt/2) / (1-iHdt/2)-1
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:RT_bands
 !
 implicit none
 !
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: dT
 complex(SP), intent(in)  :: I1_matrix(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: T1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: T2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 !
 !
 logical      :: l_use_lin_sys=.true.
 !
end subroutine RT_build_T1_and_T2
