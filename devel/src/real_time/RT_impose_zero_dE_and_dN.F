!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
subroutine RT_impose_zero_dE_and_dN(E,k,Hole_lifetime,Electron_lifetime,Scatt_kind)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,RT_nk,RT_ibz_coo,RT_Nk_ibz
 use real_time,      ONLY:RIM_ibz_coo,l_RT_uses_E_RIM,Imp_factors, &
&                         RT_E_occupations,RT_H_occupations,REF_lifetime,&
&                         l_equilibrium_lifetimes
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
 integer          :: i1,ib,ik,ik_rim,iE_rim,indx,iref
 real(SP)         :: f_E_occ,f_H_occ,k_weight,En
 real(SP)         :: prefactor(3),DEN
 real(SP)         :: Nv,Nc,dNv,dNc,Ev,Ec,dEv,dEc
 real(SP)         :: SUM_N_Holes(2),SUM_E_Holes(2),SUM_N_Electrons(2),SUM_E_Electrons(2),TMP_VALUE(4)
 !
 logical          :: l_energy,l_n_electrons,l_n_carriers
 !
 if(index(Scatt_kind,'elel')/=0) then
   l_energy=.false.
   l_n_electrons=.true.
   l_n_carriers=.false.
 endif
 !
 if(index(Scatt_kind,'elph')/=0) then
   l_energy=.false.
   l_n_electrons=.true.
   l_n_carriers=.true.
 endif
 !
 if (trim(Scatt_kind)=="elel" ) iref=1
 if (trim(Scatt_kind)=="elph1") iref=3
 if (trim(Scatt_kind)=="elph2") iref=5
 if (trim(Scatt_kind)=="elph3") iref=7
 !
 if(.not.l_equilibrium_lifetimes) then
   Hole_lifetime=    Hole_lifetime+REF_lifetime(:,:,iref)
   Electron_lifetime=Electron_lifetime+REF_lifetime(:,:,iref+1)
 endif
 !
 SUM_N_Holes   =0._SP
 SUM_N_Electrons=0._SP
 SUM_E_Holes   =0._SP
 SUM_E_Electrons=0._SP
 prefactor=0._SP
 !
 do ib=SC_bands(1),SC_bands(2)
   if(ib<=E%nbf) indx=1
   if(ib> E%nbf) indx=2
   do ik=1,k%nibz
     !
     TMP_VALUE=0._SP
     k_weight=k%weights(ik)/real(RT_Nk_ibz(ik))
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       iE_rim=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
       !
       call eval_E_and_N()
       !
     enddo
     !
     SUM_N_Holes(indx)=SUM_N_Holes(indx)+TMP_VALUE(1)*k_weight
     SUM_N_Electrons(indx)=SUM_N_Electrons(indx)+TMP_VALUE(2)*k_weight
     SUM_E_Holes(indx)=SUM_E_Holes(indx)+TMP_VALUE(3)*k_weight
     SUM_E_Electrons(indx)=SUM_E_Electrons(indx)+TMP_VALUE(4)*k_weight
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
 if(l_equilibrium_lifetimes) then
   REF_lifetime(:,:,iref)=Hole_lifetime
   REF_lifetime(:,:,iref+1)=Electron_lifetime
 else
   Hole_lifetime=    Hole_lifetime-REF_lifetime(:,:,iref)
   Electron_lifetime=Electron_lifetime-REF_lifetime(:,:,iref+1)
 endif
 !
 contains
   !
   subroutine eval_E_and_N()
     !
     f_E_occ=RT_E_occupations(ib,ik_rim)
     f_H_occ=RT_H_occupations(ib,ik_rim)
     if(.not.l_RT_uses_E_RIM) En=E%E(ib,ik,1)
     if(     l_RT_uses_E_RIM) En=E%E_RIM(ib,iE_rim,1)
     TMP_VALUE(1)=TMP_VALUE(1)+   Hole_lifetime(ib,ik_rim)*    f_H_occ
     TMP_VALUE(2)=TMP_VALUE(2)+   Electron_lifetime(ib,ik_rim)*f_E_occ
     TMP_VALUE(3)=TMP_VALUE(3)+En*Hole_lifetime(ib,ik_rim)*    f_H_occ
     TMP_VALUE(4)=TMP_VALUE(4)+En*Electron_lifetime(ib,ik_rim)*f_E_occ
     !
   end subroutine eval_E_and_N
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
       prefactor(3)=( -Nc*dNv*Ev -Nv*dNc*Ec +Nc*dEv*Nv +Nv*dEc*Nc   )/DEN
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
   !
   subroutine apply_prefactors()
     !
     if((l_energy.or.l_n_carriers).and.l_n_electrons) then
       if(ib<=E%nbf) then
         Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP+prefactor(1)+prefactor(3))
         Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)*(1._SP-prefactor(1)+prefactor(3))
       else
         Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP-prefactor(2)-prefactor(3))
         Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)*(1._SP+prefactor(2)-prefactor(3))
       endif
     else
       Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP-prefactor(1))
       Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)*(1._SP+prefactor(1))
     endif
     !
   end subroutine apply_prefactors
   !
   !
end subroutine RT_impose_zero_dE_and_dN
