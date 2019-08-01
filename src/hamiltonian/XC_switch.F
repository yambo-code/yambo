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
 use hamiltonian,    ONLY:H_kind,H_xc_functional,H_potential,H_perturbation,l_sc_XC_is_local,l_sc_V_is_local
 use xc_functionals, ONLY:XC_LDA_X,XC_LDA_C_PZ,XC_EXCHANGE_CORRELATION,XC_EXCHANGE
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
 !
 ! PERTURBATION
 !==============
 !
 H_perturbation=EXT_NONE
 !
 !
end subroutine XC_switch
