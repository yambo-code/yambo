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
 use SC,             ONLY:SC_kind,SC_xc_functional,SC_potential,local_V
 use xc_functionals, ONLY:XC_LDA_X,XC_LDA_C_PZ,XC_EXCHANGE_CORRELATION,XC_EXCHANGE
 use global_XC,      ONLY:SC_EXX,SC_HF,SC_CHOSEX,SC_EXXC
 use electrons,      ONLY:n_full_bands,n_met_bands
 use QP_m,           ONLY:CHOSEX_use_empties
 implicit none
 !
 select case (trim(SC_potential))
   case('HARTREE')
     l_sc_hartree=.true.
     SC_kind=0
     SC_xc_functional=0
   case('HARTREE-FOCK')
     l_sc_hf=.true.
     SC_kind=SC_HF
     SC_xc_functional=0
   case('EXX')
     l_sc_exx=.true.
     SC_kind=SC_EXX
     SC_xc_functional=0
     if (n_full_bands/=n_met_bands) return
   case('EXXC')
     l_sc_exxc=.true.
     SC_kind=SC_EXXC
     SC_xc_functional=XC_LDA_C_PZ
     if (n_full_bands/=n_met_bands) return
   case('CHOSEX')
     l_sc_chosex=.true.
     SC_kind=SC_CHOSEX
     SC_xc_functional=0
     CHOSEX_use_empties=.FALSE.
   case('LDA_X')
     l_sc_lda_x=.true.
     SC_kind=XC_EXCHANGE
     SC_xc_functional=XC_LDA_X
   case('PZ')
     l_sc_pz=.true.
     SC_kind=XC_EXCHANGE_CORRELATION
     SC_xc_functional=XC_LDA_C_PZ
   case('none')
     local_V=.true.
     SC_kind=0
     SC_xc_functional=0
     XC_switch=.false.
     return
 end select
 !
 XC_switch=any((/l_sc_hartree,l_sc_hf,l_sc_exx,l_sc_exxc,l_sc_chosex,l_sc_lda_x,l_sc_pz/))
 !
 local_V=l_sc_exx.or.l_sc_exxc.or.l_sc_hartree.or.l_sc_lda_x.or.l_sc_pz
 !
end function XC_switch
