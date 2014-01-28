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
subroutine RT_impose_zero_dE_and_dN(E,k,Hole_lifetime,Electron_lifetime,l_energy,l_n_electrons)
 !
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:levels,spin_occ
 use drivers,        ONLY:l_elel_scatt
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,it_now,RT_nk,RT_ibz_coo,RT_Nk_ibz
 use real_time,      ONLY:MEM_index,G_MEM_steps,G_lesser,RIM_ibz_coo,l_RT_uses_E_RIM
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 real(SP)                  :: Hole_lifetime(SC_bands(1):SC_bands(2),RT_nk)
 real(SP)                  :: Electron_lifetime(SC_bands(1):SC_bands(2),RT_nk)
 logical                   :: l_energy,l_n_electrons
 !
 ! Work Space
 !
 integer          :: ib,ik,ik_rim,iE_rim,indx
 real(SP)         :: f_occ,k_weight,En
 real(SP)         :: prefactor(2)
 real(SP)         :: N_valence,N_conduction,delta_N,E_valence,E_conduction,delta_E
 real(SP)         :: SUM_N_Holes(2),SUM_E_Holes(2),SUM_N_Electrons(2),SUM_E_Electrons(2)
 !
 SUM_N_Holes    =0._SP
 SUM_N_Electrons=0._SP
 SUM_E_Holes    =0._SP
 SUM_E_Electrons=0._SP
 prefactor      =0._SP
 !
 do ik=1,k%nibz
   k_weight=k%weights(ik)/real(RT_Nk_ibz(ik))
   do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     iE_rim=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
     do ib=SC_bands(1),SC_bands(2)
       if(ib<=E%nbf) indx=1
       if(ib> E%nbf) indx=2
       f_occ=-cI*G_lesser(ib,ib,ik_rim,MEM_index(it_now,G_MEM_steps))
       if(     l_RT_uses_E_RIM) En=E%E_RIM(ib,iE_RIM,1)
       if(.not.l_RT_uses_E_RIM) En=E%E(ib,ik,1)
       SUM_N_Holes(indx)=SUM_N_Holes(indx)+(Hole_lifetime(ib,ik_rim)*(spin_occ-f_occ))*k_weight
       SUM_N_Electrons(indx)=SUM_N_Electrons(indx)+(Electron_lifetime(ib,ik_rim)*f_occ)*k_weight
       SUM_E_Holes(indx)=SUM_E_Holes(indx)+(En*Hole_lifetime(ib,ik_rim)*(spin_occ-f_occ))*k_weight
       SUM_E_Electrons(indx)=SUM_E_Electrons(indx)+(En*Electron_lifetime(ib,ik_rim)*f_occ)*k_weight
     enddo
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
 if(     l_energy.and.     l_n_electrons) then
   prefactor(1)=(delta_E*N_conduction-delta_N*E_conduction)/(N_conduction*E_valence-N_valence*E_conduction)
   prefactor(2)=(delta_E*N_valence   -delta_N*E_valence   )/(N_conduction*E_valence-N_valence*E_conduction)
 endif
 if(     l_energy.and..not.l_n_electrons) prefactor(1)=delta_E/(E_conduction+E_valence)
 if(.not.l_energy.and.     l_n_electrons) prefactor(1)=delta_N/(N_conduction+N_valence)
 !
 if( isnan(prefactor(1)) ) return
 !
 if(     l_energy.and.     l_n_electrons) then
   Electron_lifetime(:E%nbf,:)=Electron_lifetime(:E%nbf,:)*(1._SP-prefactor(1))
   Hole_lifetime(    :E%nbf,:)=    Hole_lifetime(:E%nbf,:)*(1._SP+prefactor(1))
   Electron_lifetime(E%nbf+1:,:)=Electron_lifetime(E%nbf+1:,:)*(1._SP+prefactor(2))
   Hole_lifetime(    E%nbf+1:,:)=    Hole_lifetime(E%nbf+1:,:)*(1._SP-prefactor(2))
 else
   Electron_lifetime(:,:)=Electron_lifetime(:,:)*(1._SP-prefactor(1))
   Hole_lifetime(:,:)    =Hole_lifetime(:,:)*(1._SP+prefactor(1))
 endif
 !
end subroutine RT_impose_zero_dE_and_dN
