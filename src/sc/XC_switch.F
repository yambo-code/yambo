!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team
!              http://www.yambo-code.org
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
 use drivers
 use SC,             ONLY:SC_kind,SC_xc_functional,SC_potential,local_V,SC_perturbation,&
&                         OEP_approx,l_oep_EWeight,l_oep_iterative
 use xc_functionals, ONLY:XC_LDA_X,XC_LDA_C_PZ,XC_EXCHANGE_CORRELATION,XC_EXCHANGE,&
&                         GS_xc_KIND,GS_xc_FUNCTIONAL
#if defined _MAGNETIC
 use global_XC,      ONLY:EXT_B_LANDAU,EXT_B_PAULI,EXT_B_MAGNETISM
 use magnetic,       ONLY:MAG_landau,MAG_pauli
#endif
 use global_XC,      ONLY:SC_EXX,SC_HF,SC_COHSEX,SC_EXXC,SC_SRPA,EXT_NONE
 use electrons,      ONLY:n_full_bands,n_met_bands
 use com,            ONLY:error
 implicit none
 !WorkPars:
 integer, parameter :: KLI_APPRX = 10, SLT_APPRX = 20, CED_APPRX = 30, LEVY_APPRX = 40
 !
 local_V=.FALSE.
 !
 select case (trim(SC_potential))
   case('HARTREE')
     l_sc_contains_lda=.true.
     local_V=.true.
     SC_kind=0
     SC_xc_functional=0          
   case('HARTREE-FOCK')
     l_sc_hf=.true.
     SC_kind=SC_HF
     SC_xc_functional=0
   case('EXX')
     l_sc_exx=.true.
     local_V=.true.
     SC_kind=SC_EXX
     SC_xc_functional=0
     if (n_full_bands/=n_met_bands) return
   case('EXXC')
     l_sc_exx=.true.
     local_V=.true.
     l_sc_contains_lda=.true.
     SC_kind=SC_EXXC
     SC_xc_functional=XC_LDA_C_PZ
     if (n_full_bands/=n_met_bands) return
   case('SRPA')
     l_sc_exx=.true.
     l_sc_srpa=.true.
     local_V=.true.
     SC_kind=SC_SRPA
     SC_xc_functional=0
     if (n_full_bands/=n_met_bands) return
   case('COHSEX')
     l_sc_cohsex=.true.
     SC_kind=SC_COHSEX
     SC_xc_functional=0
   case('LDA_X')
     l_sc_contains_lda=.true.
     local_V=.true.
     SC_kind=XC_EXCHANGE
     SC_xc_functional=XC_LDA_X
   case('PZ')
     l_sc_contains_lda=.true.
     local_V=.true.
     SC_kind=XC_EXCHANGE_CORRELATION
     SC_xc_functional=XC_LDA_C_PZ
   case('default')
     l_sc_contains_lda=.true.
     local_V=.true.
     SC_kind=GS_xc_KIND
     SC_xc_functional=GS_xc_FUNCTIONAL
   case default
     call error("Unknown SC potential! ")
 end select
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
