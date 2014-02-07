!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:levels,spin_occ
 use drivers,        ONLY:l_elel_scatt
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,it_now,RT_nk,RT_ibz_coo,RT_Nk_ibz
 use real_time,      ONLY:MEM_index,G_MEM_steps,G_lesser,RIM_ibz_coo,&
&                         l_RT_uses_E_RIM,Imp_factors
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
 integer          :: i1,i2,ib,ik,ik_rim,iE_rim,indx
 real(SP)         :: f_occ,k_weight,En
 real(SP)         :: prefactor(2)
 real(SP)         :: N_valence,N_conduction,delta_N,E_valence,E_conduction,delta_E
 real(SP)         :: SUM_N_Holes(2),SUM_E_Holes(2),SUM_N_Electrons(2),SUM_E_Electrons(2),TMP_VALUE(4)
 !
 logical          :: l_energy,l_n_electrons,l_n_carriers
 !
 if(trim(Scatt_kind)=="elel") then
   l_energy=.true.
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
 ! First on the small grid
 !
 SUM_N_Holes   =0._SP
 SUM_N_Electrons=0._SP
 SUM_E_Holes   =0._SP
 SUM_E_Electrons=0._SP
 prefactor=0._SP
 !
 if(trim(Scatt_kind)=="elel".or..not.l_RT_uses_E_RIM) then
   !
   do ik=1,k%nibz
     k_weight=k%weights(ik)
     ik_rim=RT_ibz_coo(ik,1)
     do ib=SC_bands(1),SC_bands(2)
       if(ib<=E%nbf) indx=1
       if(ib> E%nbf) indx=2
       f_occ=-cI*G_lesser(ib,ib,ik_rim,MEM_index(it_now,G_MEM_steps))
       En=E%E(ib,ik,1)
       SUM_N_Holes(indx)=SUM_N_Holes(indx)+(Hole_lifetime(ib,ik_rim)*(spin_occ-f_occ))*k_weight
       SUM_N_Electrons(indx)=SUM_N_Electrons(indx)+(Electron_lifetime(ib,ik_rim)*f_occ)*k_weight
       SUM_E_Holes(indx)=SUM_E_Holes(indx)+(En*Hole_lifetime(ib,ik_rim)*(spin_occ-f_occ))*k_weight
       SUM_E_Electrons(indx)=SUM_E_Electrons(indx)+(En*Electron_lifetime(ib,ik_rim)*f_occ)*k_weight
     enddo
   enddo
   !
   N_valence=SUM_N_Holes(1)+SUM_N_Electrons(1)
   N_conduction=SUM_N_Holes(2)+SUM_N_Electrons(2)
   delta_N=(SUM_N_Electrons(1)-SUM_N_Holes(1))+(SUM_N_Electrons(2)-SUM_N_Holes(2))
   !
   E_valence=SUM_E_Holes(1)+SUM_E_Electrons(1)
   E_conduction=SUM_E_Holes(2)+SUM_E_Electrons(2)
   delta_E=(SUM_E_Electrons(1)-SUM_E_Holes(1))+(SUM_E_Electrons(2)-SUM_E_Holes(2))
   !
   if(    l_energy.and. l_n_electrons) then
     prefactor(1)=(delta_E*N_conduction-delta_N*E_conduction)/(N_conduction*E_valence-N_valence*E_conduction)
     prefactor(2)=(delta_E*N_valence   -delta_N*E_valence   )/(N_conduction*E_valence-N_valence*E_conduction)
   endif
   if(l_n_carriers.and. l_n_electrons) then
     prefactor(1)=(SUM_N_Electrons(1)-SUM_N_Holes(1))/(N_valence)
     prefactor(2)=(SUM_N_Electrons(2)-SUM_N_Holes(2))/(N_conduction)
   endif
   if(.not.l_energy.and..not.l_n_carriers.and.l_n_electrons) prefactor(1)=delta_N/(N_valence+N_conduction)
   !
   if( isnan(prefactor(1)) .or. isnan(prefactor(2)) )   goto 100
   if( prefactor(1)==prefactor(1)+1._SP .or. prefactor(2)==prefactor(2)+1._SP ) goto 100
   !
   if( abs(prefactor(1))>0.1_SP ) prefactor(1)=prefactor(1)/abs(prefactor(1))*0.1_SP
   if( abs(prefactor(2))>0.1_SP ) prefactor(2)=prefactor(2)/abs(prefactor(2))*0.1_SP
   !
   do ik=1,k%nibz
     ik_rim=RT_ibz_coo(ik,1)
     do ib=SC_bands(1),SC_bands(2)
       !
       if((l_energy.or.l_n_carriers).and.l_n_electrons) then
         if(ib<=E%nbf) then
           Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP-prefactor(1))
           Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)    *(1._SP+prefactor(1))
         else
           Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP+prefactor(2))
           Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)    *(1._SP-prefactor(2))
         endif
       else
         Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP-prefactor(1))
         Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)    *(1._SP+prefactor(1))
       endif
       !
     enddo
   enddo
   !
 endif
 !
100 if(.not.l_RT_uses_E_RIM) goto 200
 !
 if (l_RT_uses_E_RIM.and.trim(Scatt_kind)=="elel") Imp_factors(5:6)=prefactor(1:2)
 !
 ! Then on the RIM
 !
 SUM_N_Holes   =0._SP
 SUM_N_Electrons=0._SP
 SUM_E_Holes   =0._SP
 SUM_E_Electrons=0._SP
 prefactor=0._SP
 !
 if(trim(Scatt_kind)=='elel') i2=1
 if(trim(Scatt_kind)=='elph') i2=0
 !
 do ib=SC_bands(1),SC_bands(2)
   if(ib<=E%nbf) indx=1
   if(ib> E%nbf) indx=2
   do ik=1,k%nibz
     !
     TMP_VALUE=0._SP
     k_weight=k%weights(ik)/real(RT_Nk_ibz(ik))
     !
     do ik_rim=RT_ibz_coo(ik,1)+i2,RT_ibz_coo(ik,2)
       iE_rim=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
       f_occ=-cI*G_lesser(ib,ib,ik_rim,MEM_index(it_now,G_MEM_steps))
       En=E%E_RIM(ib,iE_rim,1)
       TMP_VALUE(1)=TMP_VALUE(1)+Hole_lifetime(ib,ik_rim)*(spin_occ-f_occ)
       TMP_VALUE(2)=TMP_VALUE(2)+Electron_lifetime(ib,ik_rim)*f_occ
       TMP_VALUE(3)=TMP_VALUE(3)+En*Hole_lifetime(ib,ik_rim)*(spin_occ-f_occ)
       TMP_VALUE(4)=TMP_VALUE(4)+En*Electron_lifetime(ib,ik_rim)*f_occ
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
 N_valence=SUM_N_Holes(1)+SUM_N_Electrons(1)
 N_conduction=SUM_N_Holes(2)+SUM_N_Electrons(2)
 delta_N=(SUM_N_Electrons(1)-SUM_N_Holes(1))+(SUM_N_Electrons(2)-SUM_N_Holes(2))
 !
 E_valence=SUM_E_Holes(1)+SUM_E_Electrons(1)
 E_conduction=SUM_E_Holes(2)+SUM_E_Electrons(2)
 delta_E=(SUM_E_Electrons(1)-SUM_E_Holes(1))+(SUM_E_Electrons(2)-SUM_E_Holes(2))
 !
 if(    l_energy.and. l_n_electrons) then
   prefactor(1)=(delta_E*N_conduction-delta_N*E_conduction)/(N_conduction*E_valence-N_valence*E_conduction)
   prefactor(2)=(delta_E*N_valence   -delta_N*E_valence)/(N_conduction*E_valence-N_valence*E_conduction)
 endif
 if(l_n_carriers.and. l_n_electrons) then
   prefactor(1)=(SUM_N_Electrons(1)-SUM_N_Holes(1))/(N_valence)
   prefactor(2)=(SUM_N_Electrons(2)-SUM_N_Holes(2))/(N_conduction)
 endif
 if(.not.l_energy.and..not.l_n_carriers.and.l_n_electrons) prefactor(1)=delta_N/(N_valence+N_conduction)
 !
 if( isnan(prefactor(1)) .or. isnan(prefactor(2)) )   goto 200
 if( prefactor(1)==prefactor(1)+1._SP .or. prefactor(2)==prefactor(2)+1._SP ) goto 200
 !
 if( abs(prefactor(1))>0.1_SP ) prefactor(1)=prefactor(1)/abs(prefactor(1))*0.1_SP
 if( abs(prefactor(2))>0.1_SP ) prefactor(2)=prefactor(2)/abs(prefactor(2))*0.1_SP
 !
 do ib=SC_bands(1),SC_bands(2)
   do ik=1,k%nibz
     k_weight=k%weights(ik)/real(RT_Nk_ibz(ik))
     do ik_rim=RT_ibz_coo(ik,1)+i2,RT_ibz_coo(ik,2)
       !
       if(     l_energy.and.     l_n_electrons) then
         if(ib<=E%nbf) then
           Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP-prefactor(1))
           Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)*(1._SP+prefactor(1))
         else
           Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP+prefactor(2))
           Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)*(1._SP-prefactor(2))
         endif
       else
         Electron_lifetime(ib,ik_rim)=Electron_lifetime(ib,ik_rim)*(1._SP-prefactor(1))
         Hole_lifetime(ib,ik_rim)    =Hole_lifetime(ib,ik_rim)*(1._SP+prefactor(1))
       endif
       !
     enddo
   enddo
 enddo
 !
200 if(trim(Scatt_kind)=="elel")  Imp_factors(3:4)=prefactor(1:2)
 !
 if(trim(Scatt_kind)=="elph1") Imp_factors(1:2)=prefactor(1:2)
 !
end subroutine RT_impose_zero_dE_and_dN
