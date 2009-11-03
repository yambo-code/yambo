!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team
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
logical function XC_switch()
 !
 use drivers
 use SC,             ONLY:SC_kind,SC_xc_functional,SC_potential,local_V,SC_perturbation 
 use xc_functionals, ONLY:XC_LDA_X,XC_LDA_C_PZ,XC_EXCHANGE_CORRELATION,XC_EXCHANGE,&
&                         GS_xc_KIND,GS_xc_FUNCTIONAL
#if defined _MAGNETIC
 use global_XC,      ONLY:EXT_B_LANDAU,EXT_B_PAULI,EXT_B_MAGNETISM
 use magnetic,       ONLY:MAG_landau,MAG_pauli
#endif
 use global_XC,      ONLY:SC_EXX,SC_HF,SC_CHOSEX,SC_EXXC,EXT_NONE
 use electrons,      ONLY:n_full_bands,n_met_bands
 use QP_m,           ONLY:CHOSEX_use_empties
 use com,            ONLY:error
 implicit none
 !
 XC_switch=.TRUE.
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
   case('CHOSEX')
     l_sc_chosex=.true.
     SC_kind=SC_CHOSEX
     SC_xc_functional=0
     CHOSEX_use_empties=.FALSE.
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
   case('none','NONE')
     l_sc_contains_lda=.false.
     local_V=.true.
     SC_kind=0
     SC_xc_functional=0
     XC_switch=.FALSE.
     return
   case('default')
     l_sc_contains_lda=.true.
     local_V=.true.
     SC_kind=GS_xc_KIND
     SC_xc_functional=GS_xc_FUNCTIONAL
   case default
     call error("Unknown SC potential! ")
 end select
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
end function XC_switch
