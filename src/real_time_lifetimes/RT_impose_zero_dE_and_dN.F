!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use functions,      ONLY:NAN
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:DbGd_ibz_coo,l_RT_uses_E_DbGd,EE_prefactor,EP_prefactor,EP_abs_prefactor, &
&                         RT_E_occupations,RT_H_occupations,REF_lifetime,&
&                         l_initial_equilibrium,RT_delta0_occupations,&
&                         l_RT_impose_E,RT_E_occupations_reference,&
&                         RT_H_occupations_reference,RT_nk,RT_ibz_coo,RT_Nk_ibz,&
&                         l_RT_impose_N_dN,RT_bands,RAD_prefactor
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
 real(DP), parameter :: cut_off=0.1_SP  ! 1 = 100%
 !
 integer      :: i1,ib,ik,ik_rim,iE_rim,iref
 real(DP)     :: f_E,f_H,df,k_weight,En,Hole_fac,Elec_fac,prefactor(2),&
&                dN_dT_h_v,dN_dT_h_c,dN_dT_e_v,dN_dT_e_c,dE_dT_h_v,dE_dT_h_c,dE_dT_e_v,dE_dT_e_c,&
&                dN_v,dN_c,dE_v,dE_c,dN_e,dN_h,dE_e,dE_h,prefactor_to_use,GAMMA_h,GAMMA_e
 !
 logical      :: l_energy,l_n_electrons,l_n_carriers
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
       if(l_initial_equilibrium) then
         f_E=real(RT_E_occupations_reference(ib,ik_rim),DP)
         f_H=real(RT_H_occupations_reference(ib,ik_rim),DP)
       else
         f_E=real(RT_E_occupations(ib,ik_rim),DP)
         f_H=real(RT_H_occupations(ib,ik_rim),DP)
       endif 
       !
       df=real(RT_delta0_occupations(ib,ik_rim),DP)
       !
       Hole_fac=real(Hole_lifetime(ib,ik_rim)    ,DP)*f_H-real(REF_lifetime(ib,ik_rim,iref  ),DP)*df
       Elec_fac=real(Electron_lifetime(ib,ik_rim),DP)*f_E+real(REF_lifetime(ib,ik_rim,iref+1),DP)*df
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
       else
         !
         dN_dT_h_c=dN_dT_h_c+   Hole_fac*k_weight
         dE_dT_h_c=dE_dT_h_c+En*Hole_fac*k_weight
         dN_dT_e_c=dN_dT_e_c+   Elec_fac*k_weight
         dE_dT_e_c=dE_dT_e_c+En*Elec_fac*k_weight
         !
       endif
       !
     enddo
     !
   enddo
 enddo
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
 prefactor=0._SP
 !
 ! Electrons number and Energy
 !
 if (l_energy.and.l_n_electrons.and..not.l_n_carriers) then
   prefactor(1)=( dE_c*(dN_e-dN_h)-dN_c*(dE_e-dE_h) )/(dE_c*dN_v-dE_v*dN_c)
   prefactor(2)=( dN_v*(dE_e-dE_h)-dE_v*(dN_e-dN_h) )/(dE_c*dN_v-dE_v*dN_c)
 endif
 !
 ! Electrons and carriers number
 !
 if (l_n_carriers.and.l_n_electrons.and..not.l_energy) then
   prefactor(1)= (dN_dT_e_v-dN_dT_h_v)/dN_v
   prefactor(2)= (dN_dT_e_c-dN_dT_h_c)/dN_c
 endif
 !
 ! Electrons number only
 !
 if(.not.l_energy.and..not.l_n_carriers.and.l_n_electrons) then
   prefactor(1)=(dN_e-dN_h)/(dN_e+dN_h)
   prefactor(2)=prefactor(1)
 endif
 !
 ! Pre-factors transfer to internal variables and application
 ! to the lifetimes
 !============================================================
 !
 do i1=1,2
   !
   if( NAN(real(prefactor(i1),SP)) .or. prefactor(i1)==prefactor(i1)+1._DP ) then
     prefactor(i1)=0._DP
   endif
   !
   if ( abs(prefactor(i1))>cut_off ) prefactor(i1)=prefactor(i1)/abs(prefactor(i1))*cut_off
   !
 enddo
 !
 do ib=RT_bands(1),RT_bands(2)
   do ik=1,k%nibz
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       iE_rim=ik_rim-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
       !
       if(.not.l_RT_uses_E_DbGd) En=real(E%E(ib,ik,1)         ,DP)
       if(     l_RT_uses_E_DbGd) En=real(E%E_DbGd(ib,iE_rim,1),DP)
       !
       if (En<=0._DP) prefactor_to_use=prefactor(1)
       if (En> 0._DP) prefactor_to_use=prefactor(2)
       !
       GAMMA_h=real(Hole_lifetime(ib,ik_rim)    ,DP)+real(REF_lifetime(ib,ik_rim,iref)  ,DP)
       GAMMA_e=real(Electron_lifetime(ib,ik_rim),DP)+real(REF_lifetime(ib,ik_rim,iref+1),DP)
       !
       GAMMA_h=GAMMA_h*(1._DP+prefactor_to_use)
       GAMMA_e=GAMMA_e*(1._DP-prefactor_to_use)
       !
       if (l_initial_equilibrium) then
         REF_lifetime(ib,ik_rim,iref  )= real(GAMMA_h,SP)
         REF_lifetime(ib,ik_rim,iref+1)= real(GAMMA_e,SP)
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
