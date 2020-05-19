!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine XC_potentials(POTENTIALS_string)
 !
 use pars,           ONLY:schlen
 use drivers,        ONLY:l_sc_ip,l_sc_hartree,l_sc_exx,l_sc_fock,l_sc_coh,l_sc_sex,l_sc_is_libDFT,&
&                         l_sc_srpa,l_use_Hxc_collisions,l_use_COH_collisions,l_eval_collisions
 use global_XC,      ONLY:H_SE_EXX,H_SE_FOCK,H_SE_COH,H_SE_SEX,H_SE_EXXC,H_SE_SRPA,H_SE_HARTREE
 use hamiltonian,    ONLY:H_kind,H_xc_functional,l_sc_XC_is_local,l_sc_V_is_local
 use xc_functionals, ONLY:XC_LDA_X,XC_LDA_C_PZ,XC_EXCHANGE_CORRELATION,XC_EXCHANGE,&
&                         GS_xc_KIND,GS_xc_FUNCTIONAL
 use collision_ext,  ONLY:COLLISIONS_have_HARTREE,COLLISIONS_HXC_local,COLLISIONS_HXC_MB,COLLISIONS_HXC_use_TDDFT
 use electrons,      ONLY:n_full_bands,n_met_bands
 use stderr,         ONLY:STRING_split,STRING_same
 !
 implicit none
 !
 character(*)  :: POTENTIALS_string
 !
 ! Work
 !
 character(schlen)  :: potentials(10),potential
 integer            :: i_c
 !
 ! POTENTIAL selector
 !===================
 potential=trim(POTENTIALS_string)
 do i_c=1,len_trim(POTENTIALS_string)
   if (potential(i_c:i_c)=="+") potential(i_c:i_c)=" "
 enddo
 !
 H_xc_functional=0
 H_kind=0
 if (l_eval_collisions) then
   COLLISIONS_have_HARTREE=.FALSE.
   l_use_Hxc_collisions   =.FALSE.
   l_use_COH_collisions   =.FALSE.
 endif
 !
 call STRING_split(potential,potentials)
 !
 do i_c=1,10
   !
   if (len_trim(potentials(i_c))==0) cycle
   !
   if (STRING_same(trim(potentials(i_c)),'IP')) then
     ! This means Hartree and V_xc are frozed: IP=KS-IP
     l_sc_ip=.true.
     H_kind=-1
     H_xc_functional=0
   else if (STRING_same(trim(potentials(i_c)),'DEFAULT')) then
     l_sc_hartree=.true.
     l_sc_is_libDFT=.true.
     H_xc_functional=GS_xc_FUNCTIONAL
     POTENTIALS_string="HARTREE+GS_xc"
     if (l_eval_collisions) then
       COLLISIONS_HXC_use_TDDFT=.TRUE.
       COLLISIONS_HXC_local=.TRUE.
       COLLISIONS_have_HARTREE=.TRUE.
       l_use_Hxc_collisions=.TRUE.
     endif
     H_kind=H_SE_HARTREE+GS_xc_KIND
   else if (STRING_same(trim(potentials(i_c)),'HARTREE')) then
     l_sc_hartree=.true.
     if (l_eval_collisions) then
       COLLISIONS_have_HARTREE=.TRUE.
       COLLISIONS_HXC_local=.TRUE.
       l_use_Hxc_collisions=.TRUE.
     endif
     H_kind=H_kind+H_SE_HARTREE
   else if (STRING_same(trim(potentials(i_c)),'GS_XC')) then
     l_sc_is_libDFT=.true.
     if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
     H_xc_functional=GS_xc_FUNCTIONAL
     if (l_eval_collisions) then
       COLLISIONS_HXC_use_TDDFT=.TRUE.
       COLLISIONS_HXC_local=.TRUE.
       l_use_Hxc_collisions=.TRUE.
     endif
     H_kind=H_kind+GS_xc_KIND
   else if (STRING_same(trim(potentials(i_c)),'FOCK')) then
     l_sc_fock=.true.
     l_sc_V_is_local=.false.
     if (l_eval_collisions) then
       COLLISIONS_HXC_MB=.TRUE.
       l_use_Hxc_collisions=.TRUE.
     endif
     H_kind=H_kind+H_SE_FOCK
   else if (STRING_same(trim(potentials(i_c)),'LDA_X')) then
     l_sc_is_libDFT=.true.
     if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
     H_xc_functional=XC_LDA_X
     H_kind=H_kind+XC_EXCHANGE
   else if (STRING_same(trim(potentials(i_c)),'PZ')) then
     l_sc_is_libDFT=.true.
     if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
     H_xc_functional=XC_LDA_C_PZ
     H_kind=H_kind+XC_EXCHANGE_CORRELATION
   else if (STRING_same(trim(potentials(i_c)),'EXX')) then
     l_sc_exx=.true.
     if (n_full_bands/=n_met_bands) return
     H_kind=H_kind+H_SE_EXX
   else if (STRING_same(trim(potentials(i_c)),'EXXC')) then
     l_sc_exx=.true.
     l_sc_is_libDFT=.true.
     if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
     H_xc_functional=XC_LDA_C_PZ
     if (n_full_bands/=n_met_bands) return
     H_kind=H_kind+H_SE_EXXC
   else if (STRING_same(trim(potentials(i_c)),'SRPA')) then
     l_sc_exx=.true.
     l_sc_srpa=.true.
     if (n_full_bands/=n_met_bands) return
     H_kind=H_kind+H_SE_SRPA
   else if (STRING_same(trim(potentials(i_c)),'COH')) then
     l_sc_coh=.true.
     l_sc_XC_is_local=.false.
     l_sc_V_is_local=.false.
     if (l_eval_collisions) then
       l_use_COH_collisions=.TRUE.
     endif
     H_kind=H_kind+H_SE_COH
   else if (STRING_same(trim(potentials(i_c)),'SEX')) then
     l_sc_sex=.true.
     l_sc_V_is_local=.false.
     l_sc_XC_is_local=.false.
     if (l_eval_collisions) then
       COLLISIONS_HXC_MB=.TRUE.
       l_use_Hxc_collisions=.TRUE.
     endif
     H_kind=H_kind+H_SE_SEX
   else if (STRING_same(trim(potentials(i_c)),'COHSEX')) then
     l_sc_coh=.true.
     l_sc_sex=.true.
     l_sc_V_is_local=.false.
     l_sc_XC_is_local=.false.
     H_kind=H_kind+H_SE_SEX+H_SE_COH
   else if (STRING_same(trim(potentials(i_c)),'NONE')) then
     call warning("NULL SC potential! ")
   else
     call error("Unknown SC potential! ")
   endif
   !
 enddo
 !
end subroutine XC_potentials
