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
subroutine RT_impose_elel_conserves_E_and_N(E,k)
 !
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:levels,spin_occ
 use drivers,        ONLY:l_elel_scatt
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,it_now,RT_nk,RT_ibz_coo,RT_Nk_ibz
 use real_time,      ONLY:E_E_Hole_lifetime,E_E_Electron_lifetime,MEM_index,&
&                         G_MEM_steps,G_lesser,RIM_ibz_coo,l_RT_uses_E_RIM
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 !
 ! Work Space
 !
 integer          :: ib,ik,ik_rim,iE_rim,indx
 real(SP)         :: f_occ,k_weight,En
 real(SP)         :: prefactor(2)
 real(SP)         :: N_valence,N_conduction,delta_N,E_valence,E_conduction,delta_E
 real(SP)         :: SUM_N_Holes(2),SUM_E_Holes(2),SUM_N_Electrons(2),SUM_E_Electrons(2)
 !
 logical          :: l_energy=.false.
 logical          :: l_n_electrons=.true.
 !
 return
 !
 if (.not.l_elel_scatt) return
 !
 SUM_N_Holes   =0._SP
 SUM_N_Electrons=0._SP
 SUM_E_Holes   =0._SP
 SUM_E_Electrons=0._SP
 prefactor=0._SP
 !
 do ik=1,k%nibz
   k_weight=k%weights(ik) !/real(RT_Nk_ibz(ik))
   ik_rim=RT_ibz_coo(ik,1)
   !do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     iE_rim=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
     do ib=SC_bands(1),SC_bands(2)
       if(ib<=E%nbf) indx=1
       if(ib> E%nbf) indx=2
       f_occ=-cI*G_lesser(ib,ib,ik_rim,MEM_index(it_now,G_MEM_steps))
       if(     l_RT_uses_E_RIM) En=E%E_RIM(ib,iE_RIM,1)
       if(.not.l_RT_uses_E_RIM) En=E%E(ib,ik,1)
       SUM_N_Holes(indx)=SUM_N_Holes(indx)+(E_E_Hole_lifetime(ib,ik_rim)*(spin_occ-f_occ))*k_weight
       SUM_N_Electrons(indx)=SUM_N_Electrons(indx)+(E_E_Electron_lifetime(ib,ik_rim)*f_occ)*k_weight
       SUM_E_Holes(indx)=SUM_E_Holes(indx)+(En*E_E_Hole_lifetime(ib,ik_rim)*(spin_occ-f_occ))*k_weight
       SUM_E_Electrons(indx)=SUM_E_Electrons(indx)+(En*E_E_Electron_lifetime(ib,ik_rim)*f_occ)*k_weight
     enddo
   !enddo
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
 ! Comment 1 line and uncomment the next line to get the old RT_impose_N_conservation
 !if(.not.l_energy.and.     l_n_electrons) prefactor(1)=delta_N/(N_conduction+N_valence)
 if(.not.l_energy.and.     l_n_electrons) prefactor(1)=delta_N/(sum_N_Electrons(1)+sum_N_Electrons(2))
 !
 if( isnan(prefactor(1)) ) return
 !
 if(     l_energy.and.     l_n_electrons) then
   E_E_Electron_lifetime(:E%nbf,:)=E_E_Electron_lifetime(:E%nbf,:)*(1._SP-prefactor(1))
   E_E_Hole_lifetime(    :E%nbf,:)=    E_E_Hole_lifetime(:E%nbf,:)*(1._SP+prefactor(1))
   E_E_Electron_lifetime(E%nbf+1:,:)=E_E_Electron_lifetime(E%nbf+1:,:)*(1._SP+prefactor(2))
   E_E_Hole_lifetime(    E%nbf+1:,:)=    E_E_Hole_lifetime(E%nbf+1:,:)*(1._SP-prefactor(2))
 else
   E_E_Electron_lifetime(:,:)=E_E_Electron_lifetime(:,:)*(1._SP-prefactor(1))
   ! Comment 1 line to get the old RT_impose_N_conservation
   !E_E_Hole_lifetime(    :,:)=    E_E_Hole_lifetime(:,:)*(1._SP+prefactor(1))
 endif
 !
end subroutine RT_impose_elel_conserves_E_and_N
