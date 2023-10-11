!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine XC_additional_SC_potentials()
 !
 use drivers,        ONLY:l_oep_kli,l_oep_exact,l_oep_ceda,l_oep_slater,l_sc_exx
 use hamiltonian,    ONLY:H_kind,H_perturbation
#if defined _SC
 use SC,             ONLY:OEP_approx,l_oep_EWeight,l_oep_iterative
 use drivers,        ONLY:l_sc_magnetic
 use global_XC,      ONLY:EXT_B_LANDAU,EXT_B_PAULI,EXT_B_MAGNETISM
 use magnetic,       ONLY:MAG_landau,MAG_pauli
#endif
 use global_XC,      ONLY:EXT_NONE
 use stderr,         ONLY:STRING_same
 !
 implicit none
 !
 ! Work
 !
 integer, parameter :: KLI_APPRX = 10, SLT_APPRX = 20, CED_APPRX = 30, LEVY_APPRX = 40
 !
#if defined _SC
 !
 ! OEP approximation
 !===================
 !
 if (l_sc_exx) then
   if (STRING_same(trim(OEP_approx),'n')) then
     l_oep_exact=.true.
     if (l_oep_iterative) l_oep_EWeight=.true.
   else if (STRING_same(trim(OEP_approx),'k')) then
     l_oep_kli=.true.
     H_kind=H_kind+KLI_APPRX 
   else if (STRING_same(trim(OEP_approx),'s')) then
     l_oep_slater=.true.
     l_oep_iterative=.false.
     H_kind=H_kind+SLT_APPRX
   else if (STRING_same(trim(OEP_approx),'c')) then
     l_oep_ceda=.true.
     H_kind=H_kind+CED_APPRX
   else if (STRING_same(trim(OEP_approx),'sw')) then
     l_oep_slater=.true.
     l_oep_EWeight=.true.
     l_oep_iterative=.false.
     H_kind=H_kind+LEVY_APPRX
   else if (STRING_same(trim(OEP_approx),'kw')) then
     l_oep_kli=.true.
     l_oep_EWeight=.true.
     H_kind=H_kind+LEVY_APPRX
   else if (STRING_same(trim(OEP_approx),'cw')) then
     l_oep_ceda=.true.
     l_oep_EWeight=.true.
     H_kind=H_kind+LEVY_APPRX
   else
     call error("Unknown EXX approximation! ")
   endif
   !
 end if
#endif
 !
 ! PERTURBATION
 !==============
 !
 H_perturbation=EXT_NONE
 !
#if defined _SC
 if (l_sc_magnetic) then
   if (MAG_pauli)  H_perturbation=EXT_B_PAULI
   if (MAG_landau) H_perturbation=EXT_B_LANDAU
   if (MAG_landau.and.MAG_pauli) H_perturbation=EXT_B_MAGNETISM
 endif
#endif
 !
end subroutine XC_additional_SC_potentials
