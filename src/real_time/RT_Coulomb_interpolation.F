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
subroutine RT_Coulomb_interpolation(E,k)
 !
 ! Interpolate the EE_lifetimes using the insight that 
 ! f(E)=gamma_H(E)/(gamma_H(E)+gamma_E(E)) is the distribution
 ! at which the out-of equilibrium Fermi gas would converge if 
 ! the gammas were constant
 ! (For a more sophisticated implementation check revision 3074)
 !  
 use pars,            ONLY:SP,rZero
 use units,           ONLY:HA2KEL,HA2EV
 use electrons,       ONLY:levels,spin_occ,spin
 use R_lattice,       ONLY:bz_samp
 use vec_operate,     ONLY:sort
 use functions,       ONLY:Fermi_fnc,Fermi_fnc_fit
 use QP_m,            ONLY:QP_table,QP_n_states
 use SC,              ONLY:RT_ibz_coo,RT_Nk_ibz,SC_bands,it_now
 use real_time,       ONLY:E_E_Electron_lifetime,E_E_Hole_lifetime,&
&                          RIM_closest_E,RIM_ibz_coo,l_RT_uses_E_RIM,&
&                          MEM_index,G_MEM_steps,G_lesser
 !
 implicit none
 !
 type(levels),     intent(in) :: E
 type(bz_samp),    intent(in) :: k
 !
 ! Work Space ...
 !
 integer               :: nstates
 !
 integer               :: i_k,i_k_rim,i_E_rim,i_k_ref,i_qp,i_spin,i_n,i_n_ref,ifit,i_RT,i1,i2,&
&                         i_sorted,i_unsorted,nfit,RT_table(SC_bands(1):SC_bands(2),k%nibz),&
&                         E_sort_index(k%nibz*(SC_bands(2)-SC_bands(1)+1))
 real(SP)              :: gamma_E_factor,gamma_H_factor,alpha,beta,x,k_weight,f_occ,&
&                         SUM_N_Electrons,SUM_N_Holes,Nel_change
 logical               :: l_condition_Hole,l_condition_Elec
 !
 real(SP) :: T_fit(2),Ef_fit(2),err_fit(4),deviation( k%nibz*(SC_bands(2)-SC_bands(1)+1) ), &
&            RT_occ_infinity( k%nibz*(SC_bands(2)-SC_bands(1)+1) ),RT_occ_sorted(k%nibz*(SC_bands(2)-SC_bands(1)+1)),&
&            RT_E(k%nibz*(SC_bands(2)-SC_bands(1)+1))
 !
 ! Step one: I interpolate using the small plasma
 !
 if(.not.l_RT_uses_E_RIM) return
 !
 nstates=k%nibz*(SC_bands(2)-SC_bands(1)+1)
 !
 do i_k=1,k%nibz
   !
   i_k_ref=RT_ibz_coo(i_k,1)
   !
   do i_n=SC_bands(1),SC_bands(2)
     do i_k_rim=RT_ibz_coo(i_k,1)+1,RT_ibz_coo(i_k,2)
       !
       E_E_Electron_lifetime(i_n,i_k_rim)=E_E_Electron_lifetime(i_n,i_k_ref)
       E_E_Hole_lifetime(i_n,i_k_rim)    =E_E_Hole_lifetime(i_n,i_k_ref)
       !
     enddo
     !
   enddo
   !
 enddo
 !
 ! Step 2: I use the ratio imposed by the fitting temperature
 !
 i_RT=0
 !
 do i_k=1,k%nibz
   do i_n=SC_bands(1),SC_bands(2)
     !
     i_RT=i_RT+1
     i_k_rim=RT_ibz_coo(i_k,1)
     !
     RT_occ_infinity(i_RT)=E_E_Hole_lifetime(i_n,i_k_rim)/&
     &  (E_E_Hole_lifetime(i_n,i_k_rim)+E_E_Electron_lifetime(i_n,i_k_rim))
     !
     RT_E(i_RT)=E%E(i_n,i_k,1)
     !
   enddo
   !
 enddo
 !
 call sort(RT_E,indx=E_sort_index)
 RT_occ_sorted(:)=RT_occ_infinity(E_sort_index(:))*spin_occ
 !
 !DEBUG <
 !do i_RT=1,nstates
 !  write(200,*) RT_E(i_RT),RT_occ_infinity(i_RT)
 !enddo
 !write(200,*) ' '
 !DEBUG >
 !
 nfit=Fermi_fnc_fit(RT_E,RT_occ_sorted,nstates,T_fit,Ef_fit,err_fit)
 !
 if(nfit==0) return
 !
 l_condition_Hole=err_fit(3)<0.1
 l_condition_Elec=err_fit(4)<0.1
 !
 if(nfit==3) then
   if ( .not.l_condition_Hole .and. .not.l_condition_Elec ) return
   if ( .not.l_condition_Elec ) nfit=1
   if ( .not.l_condition_Hole ) nfit=2
 endif
 if ( nfit==2 .and. .not.l_condition_Elec ) return
 if ( nfit==1 .and. .not.l_condition_Hole ) return
 !
 !DEBUG <
 !write(100,*) T_fit*HA2KEL,Ef_fit*HA2EV,err_fit
 !DEBUG > 
 !
 do ifit=1,2
   !
   if(ifit==1.and.nfit==2) cycle
   if(ifit==2.and.nfit==1) cycle
   !
   i_RT=0
   do i_k=1,k%nibz
     !
     k_weight=k%weights(i_k)/real(RT_Nk_ibz(i_k))
     !
     do i_n=SC_bands(1),SC_bands(2)
       !
       i_RT=i_RT+1
       !
       if(i_n<=E%nbf.and.ifit==2) cycle
       if(i_n> E%nbf.and.ifit==1) cycle
       !
       if(abs(RT_occ_infinity(i_RT))>0.9) cycle
       if(abs(RT_occ_infinity(i_RT))<0.1) cycle
       !
       deviation(i_RT)=RT_occ_infinity(i_RT)-    &
       &                 Fermi_fnc(E%E(i_n,i_k,1)-Ef_fit(ifit),T_fit(ifit))
       !
       !SUM_N_Holes=0._SP
       !SUM_N_Electrons=0._SP
       !do i_k_rim=RT_ibz_coo(i_k,1),RT_ibz_coo(i_k,2)
       !  !
       !  f_occ=aimag(G_lesser(i_n,i_n,i_k_rim,MEM_index(it_now,G_MEM_steps)))
       !  SUM_N_Holes=SUM_N_Holes+(E_E_Hole_lifetime(i_n,i_k_rim)*(spin_occ-f_occ))*k_weight
       !  SUM_N_Electrons=SUM_N_Electrons+(E_E_Electron_lifetime(i_n,i_k_rim)*f_occ)*k_weight
       !  !
       !enddo
       !
       !i_k_rim=RT_ibz_coo(i_k,1)
       !f_occ=aimag(G_lesser(i_n,i_n,i_k_rim,MEM_index(it_now,G_MEM_steps)))
       !SUM_N_Holes=k%weights(i_k)*E_E_Hole_lifetime(i_n,i_k_rim)*(spin_occ-f_occ)
       !SUM_N_Electrons=k%weights(i_k)*E_E_Electron_lifetime(i_n,i_k_rim)*f_occ
       !!
       !Nel_change=(SUM_N_Holes-SUM_N_Electrons) !*(1._SP-1._SP/real(RT_Nk_ibz(i_k)))
       !
       !DEBUG <
       !write(100,*) 'Nel_change= ',Nel_change
       !DEBUG >
       !
       do i_k_rim=RT_ibz_coo(i_k,1)+1,RT_ibz_coo(i_k,2)
         !
         i_E_rim=i_k_rim-RT_ibz_coo(i_k,1)+RIM_ibz_coo(i_k,1)
         !
         x=Fermi_fnc(E%E_RIM(i_n,i_E_rim,1)-Ef_fit(ifit),T_fit(ifit))+deviation(i_RT)
         !
         if(abs(E_E_Electron_lifetime(i_n,i_k_rim)*x+E_E_Hole_lifetime(i_n,i_k_rim)*(1._SP-x))>0._SP) then
           alpha= (E_E_Electron_lifetime(i_n,i_k_rim)*x-E_E_Hole_lifetime(i_n,i_k_rim)*(1._SP-x))/ &
&                 (E_E_Electron_lifetime(i_n,i_k_rim)*x+E_E_Hole_lifetime(i_n,i_k_rim)*(1._SP-x))
         elseif(abs(E_E_Electron_lifetime(i_n,i_k_rim)*x-E_E_Hole_lifetime(i_n,i_k_rim)*(1._SP-x))>0._SP) then
           alpha= +1._SP
         elseif(abs(E_E_Electron_lifetime(i_n,i_k_rim)*x-E_E_Hole_lifetime(i_n,i_k_rim)*(1._SP-x))>0._SP) then
           alpha= -1._SP
         else
           alpha=0._SP
         endif
         !
         !DEBUG <
         !write(100,*) 'E= ',E%E_RIM(i_n,i_E_rim,1)*HA2EV
         !write(100,*) '(x,alpha)= ',x,alpha
         !DEBUG >
         !
         if(abs(alpha)>1._SP) alpha=alpha/abs(alpha)
         !
         E_E_Hole_lifetime(i_n,i_k_rim)    =E_E_Hole_lifetime(i_n,i_k_rim)    *(1._SP+alpha)
         E_E_Electron_lifetime(i_n,i_k_rim)=E_E_Electron_lifetime(i_n,i_k_rim)*(1._SP-alpha)
         !
       enddo
       !
       ! Additional condition: Keep fixed the number of electrons scattered by the group
       !
       !SUM_N_Holes=0._SP
       !SUM_N_Electrons=0._SP
       !do i_k_rim=RT_ibz_coo(i_k,1),RT_ibz_coo(i_k,2) ! +1,RT_ibz_coo(i_k,2)
       !  !
       !  f_occ=aimag(G_lesser(i_n,i_n,i_k_rim,MEM_index(it_now,G_MEM_steps)))
       !  SUM_N_Holes=SUM_N_Holes+(E_E_Hole_lifetime(i_n,i_k_rim)*(spin_occ-f_occ))*k_weight
       !  SUM_N_Electrons=SUM_N_Electrons+(E_E_Electron_lifetime(i_n,i_k_rim)*f_occ)*k_weight
       !  !
       !enddo
       !!
       !if(abs((SUM_N_Holes-SUM_N_Electrons)/Nel_change)<0.2 ) cycle
       !if(SUM_N_Holes<1.E-7 .and. SUM_N_Electrons<1.E-7     ) cycle
       !
       !i1=RT_ibz_coo(i_k,1) !+1
       !i2=RT_ibz_coo(i_k,2)
       !beta=((SUM_N_Electrons-SUM_N_Holes)+Nel_change)/(SUM_N_Electrons+SUM_N_Holes)
       !if( abs(beta)>0.2 ) beta=0.2*beta/abs(beta)
       !E_E_Hole_lifetime(i_n,i1:i2)    =E_E_Hole_lifetime(i_n,i1:i2)    *(1._SP+beta)
       !E_E_Electron_lifetime(i_n,i1:i2)=E_E_Electron_lifetime(i_n,i1:i2)*(1._SP-beta)
       !
       !DEBUG <
       !write(300,*) 'FACTORS= ',SUM_N_Holes-SUM_N_Electrons,Nel_change,SUM_N_Holes-SUM_N_Electrons-Nel_change
       !write(100,*) '(E,beta)= ',E%E(i_n,i_k,1),beta
       !DEBUG >
       !
     enddo
   enddo
   !
 enddo
 !
end subroutine RT_Coulomb_interpolation
