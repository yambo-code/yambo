!
!        Copyright (C) 2000-2018 the YAMBO team
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
&                         l_sc_srpa,l_oep_kli,l_oep_exact,l_oep_ceda,l_oep_slater
 use global_XC,      ONLY:H_SE_EXX,H_SE_FOCK,H_SE_COH,H_SE_SEX,H_SE_EXXC,H_SE_SRPA,H_SE_HARTREE
 use hamiltonian,    ONLY:H_kind,H_xc_functional,H_potential,H_perturbation,l_sc_XC_is_local,l_sc_V_is_local
#if defined _SC
 use SC,             ONLY:OEP_approx,l_oep_EWeight,l_oep_iterative
#endif
 use xc_functionals, ONLY:XC_LDA_X,XC_LDA_C_PZ,XC_EXCHANGE_CORRELATION,XC_EXCHANGE,&
&                         GS_xc_KIND,GS_xc_FUNCTIONAL
#if defined _MAGNETIC
 use drivers,        ONLY:l_sc_magnetic
 use global_XC,      ONLY:EXT_B_LANDAU,EXT_B_PAULI,EXT_B_MAGNETISM
 use magnetic,       ONLY:MAG_landau,MAG_pauli
#endif
 use global_XC,      ONLY:EXT_NONE
 use electrons,      ONLY:n_full_bands,n_met_bands
 use com,            ONLY:error,warning
 use stderr,         ONLY:string_split
 !
 implicit none
 !
 ! Work
 !
 character(schlen)  :: potentials(10),potential
 integer            :: i_c
 integer, parameter :: KLI_APPRX = 10, SLT_APPRX = 20, CED_APPRX = 30, LEVY_APPRX = 40
 !
 ! POTENTIAL selector
 !===================
 !
 potential=trim(H_potential)
 do i_c=1,len_trim(H_potential)
   if (potential(i_c:i_c)=="+") potential(i_c:i_c)=" "
 enddo
 H_xc_functional=0
 H_kind=0
 call string_split(potential,potentials)
 do i_c=1,10
   if (len_trim(potentials(i_c))==0) cycle
   call POTENTIALS_scan(potentials(i_c))
 enddo
 !
 ! OEP approximation
 !===================
 !
#if defined _SC
 if (l_sc_exx) then
   select case (trim(OEP_approx))
     case('n')
       l_oep_exact=.true.
       if (l_oep_iterative) l_oep_EWeight=.true.
     case('k')
       l_oep_kli=.true.
       H_kind=H_kind+KLI_APPRX 
     case('s')
       l_oep_slater=.true.
       l_oep_iterative=.false.
       H_kind=H_kind+SLT_APPRX
     case('c')
       l_oep_ceda=.true.
       H_kind=H_kind+CED_APPRX
     case('sw')
       l_oep_slater=.true.
       l_oep_EWeight=.true.
       l_oep_iterative=.false.
       H_kind=H_kind+LEVY_APPRX
     case('kw')
       l_oep_kli=.true.
       l_oep_EWeight=.true.
       H_kind=H_kind+LEVY_APPRX
     case('cw')
       l_oep_ceda=.true.
       l_oep_EWeight=.true.
       H_kind=H_kind+LEVY_APPRX
     case default
       call error("Unknown EXX approximation! ")
   end select
 end if
#endif
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
 contains
   !
   subroutine POTENTIALS_scan(POT)
     character(schlen) :: POT
     !
     select case (trim(POT))
       case('IP')
         ! This means Hartree and V_xc are frozed: IP=KS-IP
         l_sc_ip=.true.
         H_kind=-1
         H_xc_functional=0
       case('DEFAULT')
         ! special case
         l_sc_hartree=.true.
         l_sc_is_libDFT=.true.
         H_kind=H_SE_HARTREE+GS_xc_KIND
         H_xc_functional=GS_xc_FUNCTIONAL
         H_potential="HARTREE+GS_xc"
       case('HARTREE')
         l_sc_hartree=.true.
         H_kind=H_kind+H_SE_HARTREE
       case('GS_xc')
         l_sc_is_libDFT=.true.
         H_kind=H_kind+GS_xc_KIND
         if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
         H_xc_functional=GS_xc_FUNCTIONAL
       case('FOCK')
         l_sc_fock=.true.
         l_sc_V_is_local=.false.
         H_kind=H_kind+H_SE_FOCK
       case('LDA_X')
         l_sc_is_libDFT=.true.
         H_kind=H_kind+XC_EXCHANGE
         if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
         H_xc_functional=XC_LDA_X
       case('PZ')
         l_sc_is_libDFT=.true.
         H_kind=H_kind+XC_EXCHANGE_CORRELATION
         if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
         H_xc_functional=XC_LDA_C_PZ
       case('EXX')
         l_sc_exx=.true.
         H_kind=H_kind+H_SE_EXX
         if (n_full_bands/=n_met_bands) return
       case('EXXC')
         l_sc_exx=.true.
         l_sc_is_libDFT=.true.
         H_kind=H_kind+H_SE_EXXC
         if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
         H_xc_functional=XC_LDA_C_PZ
         if (n_full_bands/=n_met_bands) return
       case('SRPA')
         l_sc_exx=.true.
         l_sc_srpa=.true.
         H_kind=H_kind+H_SE_SRPA
         if (n_full_bands/=n_met_bands) return
       case('COH')
         l_sc_coh=.true.
         l_sc_XC_is_local=.false.
         l_sc_V_is_local=.false.
         H_kind=H_kind+H_SE_COH
       case('SEX')
         l_sc_sex=.true.
         l_sc_V_is_local=.false.
         l_sc_XC_is_local=.false.
         H_kind=H_kind+H_SE_SEX
       case('COHSEX')
         l_sc_coh=.true.
         l_sc_sex=.true.
         l_sc_V_is_local=.false.
         l_sc_XC_is_local=.false.
         H_kind=H_kind+H_SE_SEX+H_SE_COH
       case ('NONE')
         call warning("NULL SC potential! ")
       case default
         call error("Unknown SC potential! ")
     end select
     !
   end subroutine
   !
end subroutine XC_switch
