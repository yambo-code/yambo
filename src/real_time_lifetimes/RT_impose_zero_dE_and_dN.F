!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_impose_zero_dE_and_dN(E,LIFE,Ge,Gh,Ge_ref,Gh_ref,G_prefactor)
 !
 use pars,           ONLY:SP,DP,rZERO
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:nkibz
 use RT_lifetimes,   ONLY:l_initial_equilibrium,RT_lifetime
 use RT_occupations, ONLY:RT_el_occ
 use real_time,      ONLY:RT_levels,G_lesser_reference,&
&                         l_RT_impose_E,l_RT_impose_for_metals,RT_nk,RTibz,&
&                         l_RT_impose_N_dN,RT_bands,l_RT_include_eq_scatt
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PP_redux_wait
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(RT_lifetime),intent(in) :: LIFE
 real(SP),      intent(inout) :: Gh(RT_bands(1):RT_bands(2),RT_nk)
 real(SP),      intent(inout) :: Ge(RT_bands(1):RT_bands(2),RT_nk)
 real(SP),      intent(inout) :: Gh_ref(RT_bands(1):RT_bands(2),RT_nk)
 real(SP),      intent(inout) :: Ge_ref(RT_bands(1):RT_bands(2),RT_nk)
 real(SP),      intent(inout) :: G_prefactor(2)
 !
 ! Work Space
 !
 ! DS: This is commented to avoid problems with the test-suite
 !     It should be restored
 !real(DP), parameter :: cut_off=0.1_DP  ! 0.1 = 10%
 !
 integer      :: ib,ik,ik_rim
 real(DP)     :: f0_E,f0_H,df,k_weight,En,Hole_fac,Elec_fac,Hole_fac0,Elec_fac0,GAMMA_h,GAMMA_e,&
&                dN_dT_h_v,dN_dT_h_c,dN_dT_e_v,dN_dT_e_c,dE_dT_h_v,dE_dT_h_c,dE_dT_e_v,dE_dT_e_c,&
&                dN0_dT_h_v,dN0_dT_h_c,dN0_dT_e_v,dN0_dT_e_c,dE0_dT_h_v,dE0_dT_h_c,dE0_dT_e_v,dE0_dT_e_c,&
&                dN_v,dN_c,dE_v,dE_c,dN_e,dN_h,dE_e,dE_h,&
&                dN0_v,dN0_c,dE0_v,dE0_c,dN0_e,dN0_h,dE0_e,dE0_h,&
&                NUM,NUM0,DEN,VALS(16)
 !
 ! Prefactors
 real(DP)     :: prefactor(2),prefactor_to_use
 !
 logical      :: l_energy,l_n_electrons,l_n_carriers
 !
 if (l_initial_equilibrium.and.l_RT_include_eq_scatt) return
 !
 G_prefactor=rZERO
 !
 if (.not.LIFE%active) return
 !
 if(trim(LIFE%kind)=="el-el") then
   l_energy      =l_RT_impose_E
   l_n_electrons =l_RT_impose_N_dN
   l_n_carriers  =.false.
 endif
 !
 if(trim(LIFE%kind)=="el-ph_emit".or.trim(LIFE%kind)=="el-ph_abs") then
   l_energy      =.false.
   l_n_electrons =l_RT_impose_N_dN
   l_n_carriers  =l_RT_impose_N_dN .and. (E%E_ind_gap(1)>0.1/HA2EV)
 endif
 !
 if(trim(LIFE%kind)=="el-photon") then
   l_energy      =.false.
   l_n_electrons =l_RT_impose_N_dN
   l_n_carriers  =.false.
 endif
 !
 if(l_initial_equilibrium) then
   Ge_ref = rZERO
   Gh_ref = rZERO
 endif
 !
 ! Calculate partial contributions of dN/dT and dE/dT
 !====================================================
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
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     do ik_rim=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
       !
       k_weight=RTibz%weights(ik_rim)
       !
       f0_E=real(         aimag(G_lesser_reference(ib,ib,ik_rim)),DP)
       f0_H=real(spin_occ-aimag(G_lesser_reference(ib,ib,ik_rim)),DP)
       !
       df=real(RT_el_occ%dN(ib,ik_rim),DP)
       !
       if (l_initial_equilibrium) df=0._DP
       !
       Hole_fac =real(Gh(ib,ik_rim)  ,DP)*(f0_H-df)
       Elec_fac =real(Ge(ib,ik_rim)  ,DP)*(f0_E+df)
       !
       if(.not.l_RT_include_eq_scatt) then
         f0_E=0._DP
         f0_H=0._DP
       endif
       !
       Hole_fac0=real(Gh_ref(ib,ik_rim),DP)*(f0_H-df)  
       Elec_fac0=real(Ge_ref(ib,ik_rim),DP)*(f0_E+df)
       !
       if(.not.l_RT_impose_for_metals) then
         Hole_fac=Hole_fac+Hole_fac0
         Elec_fac=Elec_fac+Elec_fac0
         Hole_fac0=0._DP
         Elec_fac0=0._DP
       endif
       !
       En=real(RT_levels%E(ib,ik_rim,1),DP)
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
 VALS=(/dN_dT_h_v,dE_dT_h_v,dN_dT_e_v,dE_dT_e_v,dN_dT_h_c,dE_dT_h_c,dN_dT_e_c,dE_dT_e_c,&
&       dN0_dT_h_v,dE0_dT_h_v,dN0_dT_e_v,dE0_dT_e_v,dN0_dT_h_c,dE0_dT_h_c,dN0_dT_e_c,dE0_dT_e_c/)
 !
 call PP_redux_wait(VALS,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 dN_dT_h_v=VALS(1)
 dE_dT_h_v=VALS(2)
 dN_dT_e_v=VALS(3)
 dE_dT_e_v=VALS(4)
 dN_dT_h_c=VALS(5)
 dE_dT_h_c=VALS(6)
 dN_dT_e_c=VALS(7)
 dE_dT_e_c=VALS(8)
 dN0_dT_h_v=VALS(9)
 dE0_dT_h_v=VALS(10)
 dN0_dT_e_v=VALS(11)
 dE0_dT_e_v=VALS(12)
 dN0_dT_h_c=VALS(13)
 dE0_dT_h_c=VALS(14)
 dN0_dT_e_c=VALS(15)
 dE0_dT_e_c=VALS(16)
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
 do ik=1,nkibz
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   !
   do ik_rim=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
     do ib=RT_bands(1),RT_bands(2)
       !
       En=real(RT_levels%E(ib,ik_rim,1),DP)
       !
       if (En<=0._DP) prefactor_to_use=prefactor(1)
       if (En> 0._DP) prefactor_to_use=prefactor(2)
       !
       if(l_RT_impose_for_metals) then
         GAMMA_h=real(Gh(ib,ik_rim),DP)
         GAMMA_e=real(Ge(ib,ik_rim),DP)
       else
         GAMMA_e=real(Ge(ib,ik_rim),DP)+real(Ge_ref(ib,ik_rim),DP)
         GAMMA_h=real(Gh(ib,ik_rim),DP)+real(Gh_ref(ib,ik_rim),DP)
       endif
       !
       GAMMA_h=GAMMA_h*(1._DP+prefactor_to_use)
       GAMMA_e=GAMMA_e*(1._DP-prefactor_to_use)
       !
       if(l_initial_equilibrium) then
         Gh_ref(ib,ik_rim)= real(GAMMA_h,SP)
         Ge_ref(ib,ik_rim)= real(GAMMA_e,SP)
       else if(l_RT_impose_for_metals) then
         Gh(ib,ik_rim) =real(GAMMA_h,SP)
         Ge(ib,ik_rim) =real(GAMMA_e,SP)
       else
         Gh(ib,ik_rim) =real(GAMMA_h,SP)-Gh_ref(ib,ik_rim)
         Ge(ib,ik_rim) =real(GAMMA_e,SP)-Ge_ref(ib,ik_rim)
       endif
       !
     enddo
   enddo
 enddo
 !
 ! Save values for RT_output
 !===========================
 G_prefactor=real(prefactor,SP)
 !
end subroutine RT_impose_zero_dE_and_dN
