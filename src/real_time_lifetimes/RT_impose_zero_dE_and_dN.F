!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_impose_zero_dE_and_dN(E,k,Hole_lifetime,Electron_lifetime,Scatt_kind)
 !
 use pars,           ONLY:SP,DP
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:DbGd_ibz_coo,l_RT_uses_E_DbGd,EE_prefactor,EP_prefactor,EP_abs_prefactor, &
&                         REF_lifetime,G_lesser_reference,&
&                         l_initial_equilibrium,RT_delta_occupations,&
&                         l_RT_impose_E,l_RT_impose_for_metals,RT_nk,RT_ibz_coo,RT_Nk_ibz,&
&                         l_RT_impose_N_dN,RT_bands,RAD_prefactor,l_RT_include_eq_scatt
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k
 real(SP),      intent(inout) :: Hole_lifetime(RT_bands(1):RT_bands(2),RT_nk)
 real(SP),      intent(inout) :: Electron_lifetime(RT_bands(1):RT_bands(2),RT_nk)
 character(*),  intent(in)    :: Scatt_kind
 !
 ! Work Space
 !
 ! DS: This is commented to avoid problems with the test-suite
 !     It should be restored
 !real(DP), parameter :: cut_off=0.1_DP  ! 0.1 = 10%
 !
 integer      :: ib,ik,ik_rim,iE_rim,iref
 real(DP)     :: f0_E,f0_H,df,k_weight,En,Hole_fac,Elec_fac,Hole_fac0,Elec_fac0,GAMMA_h,GAMMA_e,&
&                dN_dT_h_v,dN_dT_h_c,dN_dT_e_v,dN_dT_e_c,dE_dT_h_v,dE_dT_h_c,dE_dT_e_v,dE_dT_e_c,&
&                dN0_dT_h_v,dN0_dT_h_c,dN0_dT_e_v,dN0_dT_e_c,dE0_dT_h_v,dE0_dT_h_c,dE0_dT_e_v,dE0_dT_e_c,&
&                dN_v,dN_c,dE_v,dE_c,dN_e,dN_h,dE_e,dE_h,&
&                dN0_v,dN0_c,dE0_v,dE0_c,dN0_e,dN0_h,dE0_e,dE0_h,&
&                NUM,NUM0,DEN
 !
 ! Prefactors
 real(DP)     :: prefactor(2),prefactor_to_use
 !
 logical      :: l_energy,l_n_electrons,l_n_carriers
 !
 if (l_initial_equilibrium.and.l_RT_include_eq_scatt) return
 !
 if(index(Scatt_kind,'elel')/=0) then
   l_energy      =l_RT_impose_E
   l_n_electrons =l_RT_impose_N_dN
   l_n_carriers  =.false.
 endif
 !
 if(index(Scatt_kind,'elph')/=0) then
   l_energy      =.false.
   l_n_electrons =l_RT_impose_N_dN
   l_n_carriers  =l_RT_impose_N_dN .and. (E%E_ind_gap(1)>0.1/HA2EV)
 endif
 !
 if(index(Scatt_kind,'radiative')/=0) then
   l_energy      =.false.
   l_n_electrons =l_RT_impose_N_dN
   l_n_carriers  =.false.
 endif
 !
 if (trim(Scatt_kind)=="elel" )     iref=1
 if (trim(Scatt_kind)=="elph1")     iref=3
 if (trim(Scatt_kind)=="elph2")     iref=5
 if (trim(Scatt_kind)=="radiative") iref=7
 !
 if(l_initial_equilibrium) then
   REF_lifetime(:,:,iref)  =0._SP 
   REF_lifetime(:,:,iref+1)=0._SP
 endif
 !
 ! Calculate partial contributions of dN/dT and dE/dT
 !====================================================
 !
 dN_dT_h_v=0._DP
 dE_dT_h_v=0._DP
 dN_dT_e_v=0._DP
 dE_dT_e_v=0._DP
 dN_dT_h_c=0._DP
 dE_dT_h_c=0._DP
 dN_dT_e_c=0._DP
 dE_dT_e_c=0._DP
 !
 dN0_dT_h_v=0._DP
 dE0_dT_h_v=0._DP
 dN0_dT_e_v=0._DP
 dE0_dT_e_v=0._DP
 dN0_dT_h_c=0._DP
 dE0_dT_h_c=0._DP
 dN0_dT_e_c=0._DP
 dE0_dT_e_c=0._DP
 !
 do ib=RT_bands(1),RT_bands(2)
   !
   do ik=1,k%nibz
     !
     k_weight=real(k%weights(ik),DP)/real(RT_Nk_ibz(ik),DP)
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       iE_rim=ik_rim-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
       !
       f0_E=real(         aimag(G_lesser_reference(ib,ib,ik_rim)),DP)
       f0_H=real(spin_occ-aimag(G_lesser_reference(ib,ib,ik_rim)),DP)
       !
       df=real(RT_delta_occupations(ib,ik_rim),DP)
       !
       if (l_initial_equilibrium) df=0._DP
       !
       Hole_fac =real(Hole_lifetime(ib,ik_rim)      ,DP)*(f0_H-df)
       Elec_fac =real(Electron_lifetime(ib,ik_rim)  ,DP)*(f0_E+df)
       !
       if(.not.l_RT_include_eq_scatt) then
         f0_E=0._DP
         f0_H=0._DP
       endif
       !
       Hole_fac0=real(REF_lifetime(ib,ik_rim,iref  ),DP)*(f0_H-df)  
       Elec_fac0=real(REF_lifetime(ib,ik_rim,iref+1),DP)*(f0_E+df)
       !
       if(.not.l_RT_impose_for_metals) then
         Hole_fac=Hole_fac+Hole_fac0
         Elec_fac=Elec_fac+Elec_fac0
         Hole_fac0=0._DP
         Elec_fac0=0._DP
       endif
       !
       if(.not.l_RT_uses_E_DbGd) En=real(E%E(ib,ik,1)         ,DP)
       if(     l_RT_uses_E_DbGd) En=real(E%E_DbGd(ib,iE_rim,1),DP) 
       !
       if (En<=0._DP) then
         !
         dN_dT_h_v=dN_dT_h_v+   Hole_fac*k_weight
         dE_dT_h_v=dE_dT_h_v+En*Hole_fac*k_weight
         dN_dT_e_v=dN_dT_e_v+   Elec_fac*k_weight
         dE_dT_e_v=dE_dT_e_v+En*Elec_fac*k_weight
         !
         dN0_dT_h_v=dN0_dT_h_v+   Hole_fac0*k_weight
         dE0_dT_h_v=dE0_dT_h_v+En*Hole_fac0*k_weight
         dN0_dT_e_v=dN0_dT_e_v+   Elec_fac0*k_weight
         dE0_dT_e_v=dE0_dT_e_v+En*Elec_fac0*k_weight
         !
       else
         !
         dN_dT_h_c=dN_dT_h_c+   Hole_fac*k_weight
         dE_dT_h_c=dE_dT_h_c+En*Hole_fac*k_weight
         dN_dT_e_c=dN_dT_e_c+   Elec_fac*k_weight
         dE_dT_e_c=dE_dT_e_c+En*Elec_fac*k_weight
         !
         dN0_dT_h_c=dN0_dT_h_c+   Hole_fac0*k_weight
         dE0_dT_h_c=dE0_dT_h_c+En*Hole_fac0*k_weight
         dN0_dT_e_c=dN0_dT_e_c+   Elec_fac0*k_weight
         dE0_dT_e_c=dE0_dT_e_c+En*Elec_fac0*k_weight
         !
       endif
       !
     enddo
     !
   enddo
 enddo
 !
 ! Compute the pre-factors
 !=========================
 !
 dE_v=dE_dT_e_v+dE_dT_h_v
 dE_c=dE_dT_e_c+dE_dT_h_c
 dN_v=dN_dT_e_v+dN_dT_h_v
 dN_c=dN_dT_e_c+dN_dT_h_c
 dE_e=dE_dT_e_v+dE_dT_e_c
 dE_h=dE_dT_h_v+dE_dT_h_c
 dN_e=dN_dT_e_v+dN_dT_e_c
 dN_h=dN_dT_h_v+dN_dT_h_c
 !
 dE0_v=dE0_dT_e_v+dE0_dT_h_v
 dE0_c=dE0_dT_e_c+dE0_dT_h_c
 dN0_v=dN0_dT_e_v+dN0_dT_h_v
 dN0_c=dN0_dT_e_c+dN0_dT_h_c
 dE0_e=dE0_dT_e_v+dE0_dT_e_c
 dE0_h=dE0_dT_h_v+dE0_dT_h_c
 dN0_e=dN0_dT_e_v+dN0_dT_e_c
 dN0_h=dN0_dT_h_v+dN0_dT_h_c
 !
 prefactor=0._DP
 !
 ! Electrons number and Energy
 !
 if (l_energy.and.l_n_electrons.and..not.l_n_carriers) then
   DEN  = dE_c*dN_v-dE_v*dN_c
   if(abs(DEN)>epsilon(1._DP)) then
     NUM  = dE_c *(dN_e -dN_h )-dN_c *(dE_e -dE_h )
     NUM0 = dE0_c*(dN0_e-dN0_h)-dN0_c*(dE0_e-dE0_h)
     prefactor(1)=( NUM+NUM0 )/ DEN
     NUM  = dN_v *(dE_e -dE_h )-dE_v *(dN_e -dN_h )
     NUM0 = dN0_c*(dE0_e-dE0_h)-dE0_c*(dN0_e-dN0_h)
     prefactor(2)=( NUM+NUM0 )/ DEN
   endif
 endif
 !
 ! Fall back in case any of dN_dT_e_v,dN_dT_h_v, dN_dT_e_c,dN_dT_h_c are zero
 ! Notice that this is imposing not only that the numerator must be greater than zero
 ! but also that the prefactor must be different from one
 !
 if (l_n_carriers) l_n_carriers=  ( abs(dN_dT_e_v)>epsilon(1._DP) .and. abs(dN_dT_h_v)>epsilon(1._DP) .and. &
&                                   abs(dN_dT_e_c)>epsilon(1._DP) .and. abs(dN_dT_h_c)>epsilon(1._DP)       )
 !
 ! Electrons and carriers number
 !
 if (l_n_carriers.and.l_n_electrons.and..not.l_energy) then
   NUM  = dN_dT_e_v -dN_dT_h_v
   NUM0 = dN0_dT_e_v-dN0_dT_h_v
   if(abs(dN_v)>epsilon(1._DP)) prefactor(1)= ( NUM+NUM0 )/dN_v
   NUM  = dN_dT_e_c -dN_dT_h_c
   NUM0 = dN0_dT_e_c-dN0_dT_h_c
   if(abs(dN_c)>epsilon(1._DP)) prefactor(2)= ( NUM+NUM0 )/dN_c
 endif
 !
 ! Electrons number only
 !
 if(.not.l_energy.and..not.l_n_carriers.and.l_n_electrons) then
   NUM  = dN_e -dN_h
   NUM0 = dN0_e-dN0_h
   if(abs(dN_e+dN_h)>epsilon(1._DP)) prefactor(1)=( NUM+NUM0 )/(dN_e+dN_h)
   prefactor(2)=prefactor(1)
 endif
 !
 ! DS: This is commented to avoid problems with the test-suite
 !     It should be restored
! do i1=1,2
!   if ( abs(prefactor(i1))>cut_off ) prefactor(i1)=prefactor(i1)/abs(prefactor(i1))*cut_off
! enddo
 !
 ! Pre-factors transfer to internal variables and application to the lifetimes
 !==============================================================================
 !
 do ik=1,k%nibz
   !
   do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     !
     iE_rim=ik_rim-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
     !
     do ib=RT_bands(1),RT_bands(2)
       !
       if(.not.l_RT_uses_E_DbGd) En=real(E%E(ib,ik,1)         ,DP)
       if(     l_RT_uses_E_DbGd) En=real(E%E_DbGd(ib,iE_rim,1),DP)
       !
       if (En<=0._DP) prefactor_to_use=prefactor(1)
       if (En> 0._DP) prefactor_to_use=prefactor(2)
       !
       if(l_RT_impose_for_metals) then
         GAMMA_h=real(Hole_lifetime(ib,ik_rim)    ,DP)
         GAMMA_e=real(Electron_lifetime(ib,ik_rim),DP)
       else
         GAMMA_e=real(Electron_lifetime(ib,ik_rim),DP)+real(REF_lifetime(ib,ik_rim,iref+1),DP)
         GAMMA_h=real(Hole_lifetime(ib,ik_rim)    ,DP)+real(REF_lifetime(ib,ik_rim,iref)  ,DP)
       endif
       !
       GAMMA_h=GAMMA_h*(1._DP+prefactor_to_use)
       GAMMA_e=GAMMA_e*(1._DP-prefactor_to_use)
       !
       if(l_initial_equilibrium) then
         REF_lifetime(ib,ik_rim,iref  )= real(GAMMA_h,SP)
         REF_lifetime(ib,ik_rim,iref+1)= real(GAMMA_e,SP)
       else if(l_RT_impose_for_metals) then
         Hole_lifetime(ib,ik_rim)    =real(GAMMA_h,SP)
         Electron_lifetime(ib,ik_rim)=real(GAMMA_e,SP)
       else
         Hole_lifetime(ib,ik_rim)    =real(GAMMA_h,SP)-REF_lifetime(ib,ik_rim,iref)
         Electron_lifetime(ib,ik_rim)=real(GAMMA_e,SP)-REF_lifetime(ib,ik_rim,iref+1)
       endif
       !
     enddo
   enddo
 enddo
 !
 ! Save values for RT_output
 !===========================
 !
 if(trim(Scatt_kind)=="elel" )     EE_prefactor    =real(prefactor,SP)
 if(trim(Scatt_kind)=="elph1")     EP_prefactor    =real(prefactor,SP)
 if(trim(Scatt_kind)=="elph2")     EP_abs_prefactor=real(prefactor,SP)
 if(trim(Scatt_kind)=="radiative") RAD_prefactor   =real(prefactor,SP)
 !
end subroutine RT_impose_zero_dE_and_dN
