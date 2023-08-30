!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QP_logicals( )
 !
 use pars,          ONLY:SP
 use parser_m,      ONLY:parser
 use drivers,       ONLY:l_life
 use QP_m,          ONLY:QP_dSc_test,On_Mass_Shell_approx,l_QP_Expand,COHSEX_use_empties,l_QP_symmetrize,&
&                        use_GreenF_to_eval_QP,l_extended_output,use_GreenF_Zoom,QP_G_Zoom_treshold,&
&                        GW_terminator_Kind,l_GW_terminator,l_QP_eval_GF,l_QP_eval_Z,QP_solver
 use stderr,        ONLY:STRING_same
#if defined _ELPH || defined _PHEL
 use PHEL,          ONLY:PH_Self_Energy_mode
 use ELPH,          ONLY:l_GKKP_hosts_bare_dV
#endif
 !
 implicit none
 !
 ! OMS...
 call parser('OnMassShell',On_Mass_Shell_approx)
 !
 ! Netwon and Beta...
 call parser('NewtDchk',   QP_dSc_test)
 l_QP_eval_Z=STRING_same(QP_solver,"n").and..not.On_Mass_Shell_approx
 if (On_Mass_Shell_approx) QP_dSc_test=.FALSE.
 !
 ! Empty states...
 call parser('UseEbands',  COHSEX_use_empties)
 l_GW_terminator=GW_terminator_Kind/='none'
 !
 ! Reporting and Post-Processing
 call parser('QPExpand'   ,l_QP_Expand)
 call parser('QPsymmtrz',  l_QP_symmetrize)
 call parser('ExtendOut',  l_extended_output)
 !
 ! GF...
 l_QP_eval_GF=STRING_same(QP_solver,"g")
 call parser('GreenF2QP',  use_GreenF_to_eval_QP)
 use_GreenF_Zoom=QP_G_Zoom_treshold>0._SP
 !
 ! Lifetimes
 if (l_life) QP_solver="none"
 !
 ! Ph-El...
 !
#if defined _ELPH || defined _PHEL
 ! ... Kind
 if ( index(PH_Self_Energy_mode,"bare")>0 ) l_GKKP_hosts_bare_dV = .true.
#endif
 !
end subroutine
