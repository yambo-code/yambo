!
!        Copyright (C) 2000-2016 the YAMBO team
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
 use pars,           ONLY:schlen
 use drivers,        ONLY:l_sc_ip,l_sc_hartree,l_sc_exx,l_sc_fock,l_sc_coh,l_sc_sex,l_sc_is_libDFT,&
&                         l_sc_srpa,l_oep_kli,l_oep_exact,l_oep_ceda,l_oep_slater,l_sc_magnetic
 use SC,             ONLY:SC_kind,SC_xc_functional,SC_potential,l_sc_pot_is_local,SC_perturbation,&
&                         OEP_approx,l_oep_EWeight,l_oep_iterative
 use xc_functionals, ONLY:XC_LDA_X,XC_LDA_C_PZ,XC_EXCHANGE_CORRELATION,XC_EXCHANGE,&
&                         GS_xc_KIND,GS_xc_FUNCTIONAL
#if defined _MAGNETIC
 use global_XC,      ONLY:EXT_B_LANDAU,EXT_B_PAULI,EXT_B_MAGNETISM
 use magnetic,       ONLY:MAG_landau,MAG_pauli
#endif
 use global_XC,      ONLY:SC_EXX,SC_Hartree,SC_FOCK,SC_COHSEX,SC_COH,SC_SEX,SC_EXXC,SC_SRPA,EXT_NONE
 use electrons,      ONLY:n_full_bands,n_met_bands
 use com,            ONLY:error
 !
 implicit none
 !
 ! Work
 !
 character(schlen)  :: SC_Hartree_potential,SC_XConly_potential
 integer            :: index_plus
 integer, parameter :: KLI_APPRX = 10, SLT_APPRX = 20, CED_APPRX = 30, LEVY_APPRX = 40
 !
 index_plus = SCAN(SC_potential,'+')
 SC_Hartree_potential=(trim(SC_potential(1:index_plus-1)))
 SC_XConly_potential =(trim(SC_potential(index_plus+1: )))
 !
 select case (trim(SC_XConly_potential))
   case('IP')
     ! This means Hartree and V_xc are frozed: IP=KS-IP
     l_sc_ip=.true.
     SC_kind=-1
     SC_xc_functional=0
   case('default')
     ! special case
     l_sc_hartree=.true.
     l_sc_is_libDFT=.true.
     SC_kind=GS_xc_KIND+SC_HARTREE
     SC_xc_functional=GS_xc_FUNCTIONAL
     SC_potential="HARTREE+GS_xc"
   case('HARTREE')
     l_sc_hartree=.true.
     SC_kind=SC_HARTREE
     SC_xc_functional=0
   case('GS_xc')
     l_sc_is_libDFT=.true.
     SC_kind=GS_xc_KIND
     SC_xc_functional=GS_xc_FUNCTIONAL
   case('FOCK')
     l_sc_fock=.true.
     l_sc_pot_is_local=.false.
     SC_kind=SC_FOCK
     SC_xc_functional=0
   case('LDA_X')
     l_sc_is_libDFT=.true.
     SC_kind=XC_EXCHANGE
     SC_xc_functional=XC_LDA_X
   case('PZ')
     l_sc_is_libDFT=.true.
     SC_kind=XC_EXCHANGE_CORRELATION
     SC_xc_functional=XC_LDA_C_PZ
   case('EXX')
     l_sc_exx=.true.
     SC_kind=SC_EXX
     SC_xc_functional=0
     if (n_full_bands/=n_met_bands) return
   case('EXXC')
     l_sc_exx=.true.
     l_sc_is_libDFT=.true.
     SC_kind=SC_EXXC
     SC_xc_functional=XC_LDA_C_PZ
     if (n_full_bands/=n_met_bands) return
   case('SRPA')
     l_sc_exx=.true.
     l_sc_srpa=.true.
     SC_kind=SC_SRPA
     SC_xc_functional=0
     if (n_full_bands/=n_met_bands) return
   case('COH')
     l_sc_coh=.true.
     l_sc_pot_is_local=.false.
     SC_kind=SC_COH
     SC_xc_functional=0
   case('SEX')
     l_sc_sex=.true.
     l_sc_pot_is_local=.false.
     SC_kind=SC_SEX
     SC_xc_functional=0
   case('COHSEX')
     l_sc_coh=.true.
     l_sc_sex=.true.
     l_sc_pot_is_local=.false.
     SC_kind=SC_COHSEX
     SC_xc_functional=0
   case default
     call error("Unknown SC potential! ")
 end select
 !
 if (trim(SC_Hartree_potential)=="HARTREE") then
   l_sc_hartree=.true.
   SC_kind=SC_kind+SC_Hartree
 endif
 !
 ! OEP approximation
 !
 if (l_sc_exx) then
   select case (trim(OEP_approx))
     case('n')
       l_oep_exact=.true.
       if (l_oep_iterative) l_oep_EWeight=.true.
     case('k')
       l_oep_kli=.true.
       SC_kind=SC_kind+KLI_APPRX 
     case('s')
       l_oep_slater=.true.
       l_oep_iterative=.false.
       SC_kind=SC_kind+SLT_APPRX
     case('c')
       l_oep_ceda=.true.
       SC_kind=SC_kind+CED_APPRX
     case('sw')
       l_oep_slater=.true.
       l_oep_EWeight=.true.
       l_oep_iterative=.false.
       SC_kind=SC_kind+LEVY_APPRX
     case('kw')
       l_oep_kli=.true.
       l_oep_EWeight=.true.
       SC_kind=SC_kind+LEVY_APPRX
     case('cw')
       l_oep_ceda=.true.
       l_oep_EWeight=.true.
       SC_kind=SC_kind+LEVY_APPRX
     case default
       call error("Unknown EXX approximation! ")
   end select
 end if
 !
 SC_perturbation=EXT_NONE
 !
#if defined _MAGNETIC
 !
 if (l_sc_magnetic) then
   if (MAG_pauli)  SC_perturbation=EXT_B_PAULI
   if (MAG_landau) SC_perturbation=EXT_B_LANDAU
   if (MAG_landau.and.MAG_pauli) SC_perturbation=EXT_B_MAGNETISM
 endif
#endif
 !
end subroutine XC_switch
