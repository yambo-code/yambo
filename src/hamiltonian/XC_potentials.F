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
subroutine XC_potentials(POTENTIALS_string,mode)
 !
 use pars,           ONLY:schlen
 use drivers,        ONLY:l_sc_ip,l_sc_hartree,l_sc_exx,l_sc_fock,l_sc_coh,l_sc_sex,l_sc_is_libDFT,&
&                         l_sc_srpa,l_use_Hxc_collisions,l_use_COH_collisions
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
 character(*)  :: mode
 !
 ! Work
 !
 character(schlen)  :: potentials(10),potential
 integer            :: i_c
 logical            :: l_SC,l_COLL
 !
 ! MODE
 !======
 !
 l_SC  =mode=="SC"
 l_COLL=mode=="COLLISIONS"
 !
 ! POTENTIAL selector
 !===================
 potential=trim(POTENTIALS_string)
 do i_c=1,len_trim(POTENTIALS_string)
   if (potential(i_c:i_c)=="+") potential(i_c:i_c)=" "
 enddo
 !
 if (l_SC) then
   H_xc_functional=0
   H_kind=0
 else if (l_COLL) then
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
     if (l_SC) then
       ! This means Hartree and V_xc are frozed: IP=KS-IP
       l_sc_ip=.true.
       H_kind=-1
       H_xc_functional=0
     endif
   else if (STRING_same(trim(potentials(i_c)),'DEFAULT')) then
     if (l_SC) then
       l_sc_hartree=.true.
       l_sc_is_libDFT=.true.
       H_kind=H_SE_HARTREE+GS_xc_KIND
       H_xc_functional=GS_xc_FUNCTIONAL
       POTENTIALS_string="HARTREE+GS_xc"
     else if (l_COLL) then
       COLLISIONS_HXC_use_TDDFT=.TRUE.
       COLLISIONS_HXC_local=.TRUE.
       l_use_Hxc_collisions=.TRUE.
     endif
   else if (STRING_same(trim(potentials(i_c)),'HARTREE')) then
     if (l_SC) then
       l_sc_hartree=.true.
       H_kind=H_kind+H_SE_HARTREE
     else if (l_COLL) then
       COLLISIONS_have_HARTREE=.TRUE.
       COLLISIONS_HXC_local=.TRUE.
       l_use_Hxc_collisions=.TRUE.
     endif
   else if (STRING_same(trim(potentials(i_c)),'GS_XC')) then
     if (l_SC) then
       l_sc_is_libDFT=.true.
       H_kind=H_kind+GS_xc_KIND
       if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
       H_xc_functional=GS_xc_FUNCTIONAL
     else if (l_COLL) then
       COLLISIONS_HXC_use_TDDFT=.TRUE.
       COLLISIONS_HXC_local=.TRUE.
       l_use_Hxc_collisions=.TRUE.
     endif
   else if (STRING_same(trim(potentials(i_c)),'FOCK')) then
     if (l_SC) then
       l_sc_fock=.true.
       l_sc_V_is_local=.false.
       H_kind=H_kind+H_SE_FOCK
     else if (l_COLL) then
       COLLISIONS_HXC_MB=.TRUE.
       l_use_Hxc_collisions=.TRUE.
     endif
   else if (STRING_same(trim(potentials(i_c)),'LDA_X')) then
     if (l_SC) then
       l_sc_is_libDFT=.true.
       H_kind=H_kind+XC_EXCHANGE
       if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
       H_xc_functional=XC_LDA_X
     endif
   else if (STRING_same(trim(potentials(i_c)),'PZ')) then
     if (l_SC) then
       l_sc_is_libDFT=.true.
       H_kind=H_kind+XC_EXCHANGE_CORRELATION
       if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
       H_xc_functional=XC_LDA_C_PZ
     endif
   else if (STRING_same(trim(potentials(i_c)),'EXX')) then
     if (l_SC) then
       l_sc_exx=.true.
       H_kind=H_kind+H_SE_EXX
       if (n_full_bands/=n_met_bands) return
     endif
   else if (STRING_same(trim(potentials(i_c)),'EXXC')) then
     if (l_SC) then
       l_sc_exx=.true.
       l_sc_is_libDFT=.true.
       H_kind=H_kind+H_SE_EXXC
       if(H_xc_functional/=0) call error(' Cannot use two DFT XC flavors. Set one only in input')
       H_xc_functional=XC_LDA_C_PZ
       if (n_full_bands/=n_met_bands) return
     endif
   else if (STRING_same(trim(potentials(i_c)),'SRPA')) then
     if (l_SC) then
       l_sc_exx=.true.
       l_sc_srpa=.true.
       H_kind=H_kind+H_SE_SRPA
       if (n_full_bands/=n_met_bands) return
     endif
   else if (STRING_same(trim(potentials(i_c)),'COH')) then
     if (l_SC) then
       l_sc_coh=.true.
       l_sc_XC_is_local=.false.
       l_sc_V_is_local=.false.
       H_kind=H_kind+H_SE_COH
     else if (l_COLL) then
       l_use_COH_collisions=.TRUE.
     endif
   else if (STRING_same(trim(potentials(i_c)),'SEX')) then
     if (l_SC) then
       l_sc_sex=.true.
       l_sc_V_is_local=.false.
       l_sc_XC_is_local=.false.
       H_kind=H_kind+H_SE_SEX
     else if (l_COLL) then
       COLLISIONS_HXC_MB=.TRUE.
       l_use_Hxc_collisions=.TRUE.
     endif
   else if (STRING_same(trim(potentials(i_c)),'COHSEX')) then
     if (l_SC) then
       l_sc_coh=.true.
       l_sc_sex=.true.
       l_sc_V_is_local=.false.
       l_sc_XC_is_local=.false.
       H_kind=H_kind+H_SE_SEX+H_SE_COH
     endif
   else if (STRING_same(trim(potentials(i_c)),'NONE')) then
     call warning("NULL SC potential! ")
   else
     call error("Unknown SC potential! ")
   endif
   !
 enddo
 !
end subroutine XC_potentials
