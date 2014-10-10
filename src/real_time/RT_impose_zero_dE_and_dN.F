!
!        Copyright (C) 2000-2014 the YAMBO team
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
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:RIM_ibz_coo,l_RT_uses_E_RIM,Imp_factors, &
&                         RT_E_occupations,RT_H_occupations,REF_lifetime,&
&                         l_initial_equilibrium,RT_delta0_occupations,&
&                         l_RT_EE_impose_E,RT_E_occupations_reference,&
&                         RT_H_occupations_reference,RT_nk,RT_ibz_coo,RT_Nk_ibz,&
&                         l_RT_impose_N_dN
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k
 real(SP),      intent(inout) :: Hole_lifetime(SC_bands(1):SC_bands(2),RT_nk)
 real(SP),      intent(inout) :: Electron_lifetime(SC_bands(1):SC_bands(2),RT_nk)
 character(5),  intent(in)    :: Scatt_kind
 !
 ! Work Space
 !
 real(SP), parameter :: cut_off=0.1_SP  ! 1 = 100%
 !
 integer      :: i1,ib,ik,ik_rim,iE_rim,indx,iref
 real(SP)     :: f_E,f_H,df,k_weight,En,Hole_fac,Elec_fac,prefactor(2),DEN,Nv,Nc,dNv,dNc,Ev,Ec,dEv,dEc,&
&                SUM_N_Holes(2),SUM_E_Holes(2),SUM_N_Electrons(2),SUM_E_Electrons(2),TMP_VALUE(4)
 !
 real(SP)     :: dN_dT_h_v,dN_dT_h_c,dN_dT_e_v,dN_dT_e_c
 real(SP)     :: dE_dT_h_v,dE_dT_h_c,dE_dT_e_v,dE_dT_e_c
 !
 logical      :: l_energy,l_n_electrons,l_n_carriers
 !
 if(index(Scatt_kind,'elel')/=0) then
   l_energy      =l_RT_EE_impose_E
   l_n_electrons =l_RT_impose_N_dN
   l_n_carriers  =.false.
 endif
 !
 if(index(Scatt_kind,'elph')/=0) then
   l_energy      =.false.
   l_n_electrons =l_RT_impose_N_dN
   l_n_carriers  =l_RT_impose_N_dN
 endif
 !
 if (trim(Scatt_kind)=="elel" ) iref=1
 if (trim(Scatt_kind)=="elph1") iref=3
 if (trim(Scatt_kind)=="elph2") iref=5
 !
 if(l_initial_equilibrium) then
   REF_lifetime(:,:,iref)=  0._SP
   REF_lifetime(:,:,iref+1)=0._SP
 endif
 !
 SUM_N_Holes   =0._SP
 SUM_N_Electrons=0._SP
 SUM_E_Holes   =0._SP
 SUM_E_Electrons=0._SP
 prefactor=0._SP
 !
 dN_dT_h_v=0._SP
 dE_dT_h_v=0._SP
 dN_dT_e_v=0._SP
 dE_dT_e_v=0._SP
 dN_dT_h_c=0._SP
 dE_dT_h_c=0._SP
 dN_dT_e_c=0._SP
 dE_dT_e_c=0._SP
 !
 do ib=SC_bands(1),SC_bands(2)
   !
   do ik=1,k%nibz
     !
     k_weight=k%weights(ik)/real(RT_Nk_ibz(ik))
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       iE_rim=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
       !
       if(l_initial_equilibrium) then
         f_E=RT_E_occupations_reference(ib,ik_rim)
         f_H=RT_H_occupations_reference(ib,ik_rim)
       else
         f_E=RT_E_occupations(ib,ik_rim)
         f_H=RT_H_occupations(ib,ik_rim)
       endif 
       !
       df=RT_delta0_occupations(ib,ik_rim) 
       !
       Hole_fac=Hole_lifetime(ib,ik_rim)    *f_H-REF_lifetime(ib,ik_rim,iref  )*df
       Elec_fac=Electron_lifetime(ib,ik_rim)*f_E+REF_lifetime(ib,ik_rim,iref+1)*df
       !
       if(.not.l_RT_uses_E_RIM) En=E%E(ib,ik,1)
       if(     l_RT_uses_E_RIM) En=E%E_RIM(ib,iE_rim,1) 
       !
       if (ib<=E%nbf) then
         !
         dN_dT_h_v=dN_dT_h_v+   Hole_fac*k_weight
         dE_dT_h_v=dN_dT_h_v+En*Hole_fac*k_weight
         dN_dT_e_v=dN_dT_e_v+   Elec_fac*k_weight
         dE_dT_e_v=dN_dT_e_v+En*Elec_fac*k_weight
         !
       else
         !
         dN_dT_h_c=dN_dT_h_c+   Hole_fac*k_weight
         dE_dT_h_c=dN_dT_h_c+En*Hole_fac*k_weight
         dN_dT_e_c=dN_dT_e_c+   Elec_fac*k_weight
         dE_dT_e_c=dN_dT_e_c+En*Elec_fac*k_weight
         !
       endif
       !
     enddo
     !
   enddo
 enddo
 !
 call compute_prefactors()
 !
 if(trim(Scatt_kind)=="elel" ) Imp_factors(3:5)=0._SP
 if(trim(Scatt_kind)=="elph1") Imp_factors(1:2)=0._SP
 !
 do i1=1,3
   if( isnan(prefactor(i1)) .or. prefactor(i1)==prefactor(i1)+1._SP ) then
     if (trim(Scatt_kind)=="elel") Imp_factors(3:5)=prefactor(1:3)
     if(trim(Scatt_kind)=="elph1") Imp_factors(1:2)=prefactor(1:2)
     if(l_initial_equilibrium) then
       REF_lifetime(:,:,iref)=Hole_lifetime
       REF_lifetime(:,:,iref+1)=Electron_lifetime
     endif
     return
   endif
   if( abs(prefactor(i1))>cut_off ) prefactor(i1)=prefactor(i1)/abs(prefactor(i1))*cut_off
 enddo
 !
 do ib=SC_bands(1),SC_bands(2)
   do ik=1,k%nibz
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       call apply_prefactors()
       !
     enddo
   enddo
 enddo
 !
 if (trim(Scatt_kind)=="elel")  Imp_factors(3:5)=prefactor(1:3)
 if (trim(Scatt_kind)=="elph1") Imp_factors(1:2)=prefactor(1:2)
 !
 if(l_initial_equilibrium) then
   REF_lifetime(:,:,iref)=Hole_lifetime
   REF_lifetime(:,:,iref+1)=Electron_lifetime
 endif
 !
 contains
   !
   subroutine compute_prefactors()
     !
     prefactor=0._SP
     !
     Nv=SUM_N_Holes(1)+SUM_N_Electrons(1)
     Nc=SUM_N_Holes(2)+SUM_N_Electrons(2)
     dNv=SUM_N_Electrons(1)-SUM_N_Holes(1)
     dNc=SUM_N_Electrons(2)-SUM_N_Holes(2)
     !
     Ev=SUM_E_Holes(1)+SUM_E_Electrons(1)
     Ec=SUM_E_Holes(2)+SUM_E_Electrons(2)
     dEv=SUM_E_Electrons(1)-SUM_E_Holes(1)
     dEc=SUM_E_Electrons(2)-SUM_E_Holes(2)
     !
     ! Electrons, carriers and Energy
     if(l_energy.and.l_n_electrons.and.l_n_carriers) then
       DEN= Nc*dNv*Ev -Nv*dNc*Ec -Nc*dEv*Nv +Nv*dEc*Nc
       prefactor(2)=2*(  dNv*dNc*Ev - dEv*dNc*Nv )/DEN
       prefactor(1)=2*(  dNv*dNc*Ec - dNv*dEc*Nc )/DEN
     endif
     ! Electrons number and Energy
     if(l_energy.and.l_n_electrons.and..not.l_n_carriers) then
       DEN=Nc*Ev-Nv*Ec
       prefactor(1)=( -(dEv+dEc)*Nc +(dNv+dNc)*Ec )/DEN
       prefactor(2)=( -(dEv+dEc)*Nv +(dNv+dNc)*Ev )/DEN
     endif
     ! Electrons number and carrires number
     if(l_n_carriers.and.l_n_electrons.and..not.l_energy) then
       prefactor(1)=-dNv/Nv
       prefactor(2)= dNc/Nc
     endif
     ! Electrons number only
     if(.not.l_energy.and..not.l_n_carriers.and.l_n_electrons) &
     &  prefactor(1)=(dNv+dNc)/(Nv+Nc)
     !
   end subroutine compute_prefactors
   !
   subroutine apply_prefactors()
     !
     if ((l_energy.or.l_n_carriers).and.l_n_electrons) then
       if (ib<=E%nbf) then
         Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP+prefactor(1))+&
&                                     REF_lifetime(ib,ik_rim,iref+1)*prefactor(1)
         Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)    *(1._SP-prefactor(1))-&
&                                     REF_lifetime(ib,ik_rim,iref  )*prefactor(1)
       else
         Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP-prefactor(2))-&
&                                     REF_lifetime(ib,ik_rim,iref+1)*prefactor(2)
         Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)    *(1._SP+prefactor(2))+&
&                                     REF_lifetime(ib,ik_rim,iref  )*prefactor(2)
       endif
     else
       Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP-prefactor(1))  -&
&                                   REF_lifetime(ib,ik_rim,iref+1)*prefactor(1)
       Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)    *(1._SP+prefactor(1))  +&
&                                   REF_lifetime(ib,ik_rim,iref  )*prefactor(1)
     endif
     !
   end subroutine apply_prefactors
   !
   !
end subroutine RT_impose_zero_dE_and_dN
