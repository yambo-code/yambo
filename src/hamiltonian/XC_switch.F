!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
!
! This file is distributed under the terms of the GNU
! General Public License. You can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will
! be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE.  See the GNU General Public License
! for more details.
!
! You should have received a copy of the GNU General Public
! License along with this program; if not, write to the Free
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine XC_switch()
 !
 use drivers,        ONLY:l_oep_kli,l_oep_exact,l_oep_ceda,l_oep_slater,l_sc_exx
 use hamiltonian,    ONLY:H_kind,H_potential,H_perturbation
#if defined _SC
 use SC,             ONLY:OEP_approx,l_oep_EWeight,l_oep_iterative
#endif
#if defined _MAGNETIC
 use drivers,        ONLY:l_sc_magnetic
 use global_XC,      ONLY:EXT_B_LANDAU,EXT_B_PAULI,EXT_B_MAGNETISM
 use magnetic,       ONLY:MAG_landau,MAG_pauli
#endif
 use global_XC,      ONLY:EXT_NONE
 use com,            ONLY:error
 use stderr,         ONLY:STRING_match
 !
 implicit none
 !
 ! Work
 !
 integer, parameter :: KLI_APPRX = 10, SLT_APPRX = 20, CED_APPRX = 30, LEVY_APPRX = 40
 !
 ! POTENTIAL selector
 !===================
 !
 call XC_potentials(H_potential,"SC")
 !
 ! OEP approximation
 !===================
 !
#if defined _SC
 if (l_sc_exx) then
   if (STRING_match(trim(OEP_approx),'n')) then
     l_oep_exact=.true.
     if (l_oep_iterative) l_oep_EWeight=.true.
   else if (STRING_match(trim(OEP_approx),'k')) then
     l_oep_kli=.true.
     H_kind=H_kind+KLI_APPRX 
   else if (STRING_match(trim(OEP_approx),'s')) then
     l_oep_slater=.true.
     l_oep_iterative=.false.
     H_kind=H_kind+SLT_APPRX
   else if (STRING_match(trim(OEP_approx),'c')) then
     l_oep_ceda=.true.
     H_kind=H_kind+CED_APPRX
   else if (STRING_match(trim(OEP_approx),'sw')) then
     l_oep_slater=.true.
     l_oep_EWeight=.true.
     l_oep_iterative=.false.
     H_kind=H_kind+LEVY_APPRX
   else if (STRING_match(trim(OEP_approx),'kw')) then
     l_oep_kli=.true.
     l_oep_EWeight=.true.
     H_kind=H_kind+LEVY_APPRX
   else if (STRING_match(trim(OEP_approx),'cw')) then
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
#if defined _MAGNETIC
 !
 if (l_sc_magnetic) then
   if (MAG_pauli)  H_perturbation=EXT_B_PAULI
   if (MAG_landau) H_perturbation=EXT_B_LANDAU
   if (MAG_landau.and.MAG_pauli) H_perturbation=EXT_B_MAGNETISM
 endif
#endif
 !
end subroutine XC_switch
