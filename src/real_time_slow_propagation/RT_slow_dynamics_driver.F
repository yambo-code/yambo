!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): AM 
!
subroutine RT_slow_dynamics_driver(dG_old,dG_in,dG_out,ik,dT)
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:l_RT_EXP,l_RT_ACC,l_RT_INV,RT_bands,l_RT_DIAG
 !
 implicit none
 !
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: dT
 complex(SP), intent(in)  :: dG_old(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)  ::  dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Work Space
 logical :: l_RT_STD
 !
 l_RT_STD=.not.(l_RT_ACC.or.l_RT_DIAG)
 !
 if (l_RT_EXP.and.l_RT_ACC ) call RT_EXP_step_accurate(dG_old,dG_in,dG_out,ik,dT)
 if (l_RT_EXP.and.l_RT_STD ) call RT_EXP_step_std(dG_old,dG_in,dG_out,ik,dT)
 !
 if (l_RT_INV.and.l_RT_ACC ) call RT_INV_step_accurate(dG_old,dG_in,dG_out,ik,dT)
 if (l_RT_INV.and.l_RT_DIAG) call RT_INV_step_diago(dG_old,dG_in,dG_out,ik,dT)
 if (l_RT_INV.and.l_RT_STD ) call RT_INV_step_std(dG_old,dG_in,dG_out,ik,dT)
 !
end subroutine RT_slow_dynamics_driver
