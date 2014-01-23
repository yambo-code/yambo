!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team 
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
 use real_time,       ONLY:E_E_Electron_lifetime,E_E_Hole_lifetime,         &
&                          RIM_closest_E,RIM_ibz_coo,l_RT_uses_E_RIM,       &
&                          MEM_index,G_MEM_steps,G_lesser,RT_Tfit_lifetimes,&
&                          RT_Efit_lifetimes,l_lftm_fit_stable,Nfitted_lifetimes
 !
 implicit none
 !
 type(levels),     intent(in) :: E
 type(bz_samp),    intent(in) :: k
 !
 ! Work Space ...
 !
 integer  :: nstates
 !
 integer  :: i_k,i_k_rim,i_E_rim,i_k_ref,i_qp,i_spin,i_n,i_n_ref,order, &
&            ifit,i_RT,i1,i2,i_sorted,i_unsorted,nfit,                  &
&            RT_table(SC_bands(1):SC_bands(2),k%nibz),                  &
&            E_sort_index(k%nibz*(SC_bands(2)-SC_bands(1)+1)),          &
&            E_sort_index_m1(k%nibz*(SC_bands(2)-SC_bands(1)+1))
 !
 real(SP) :: gamma_E_factor,gamma_H_factor,alpha,beta,x,k_weight,fac,   &
&            f_occ,SUM_N_Electrons,SUM_N_Holes,Nel_change,EPS,E_ref(2), &
&            Energy(2)
 !
 logical  :: l_condition(2)
 !
 real(SP) :: T_fit(2),Ef_fit(2),err_fit(4),Hole_coeff(3),Elec_coeff(3), &
&            deviation( k%nibz*(SC_bands(2)-SC_bands(1)+1) ),           &
&            RT_occ_infinity( k%nibz*(SC_bands(2)-SC_bands(1)+1) ),     &
&            RT_occ_sorted(k%nibz*(SC_bands(2)-SC_bands(1)+1)),         &
&            RT_E(k%nibz*(SC_bands(2)-SC_bands(1)+1))
 !
 if(.not.l_RT_uses_E_RIM) return
 !
 nstates=k%nibz*(SC_bands(2)-SC_bands(1)+1)
 !
 ! Sort energies
 !
 i_RT=0
 !
 do i_k=1,k%nibz
   do i_n=SC_bands(1),SC_bands(2)
     i_RT=i_RT+1
     RT_E(i_RT)=E%E(i_n,i_k,1)
   enddo
 enddo
 !
 call sort(RT_E,indx=E_sort_index,indx_m1=E_sort_index_m1)
 !
 ! Step 1: start setting all lifetimes in the group identical
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
 ! Step 2: I correct the gamma on the BIG gid imposing the ratio
 !         obtained fitting the ratio of the gamma on the SMALL grid
 !         with a Fermi function
 i_RT=0
 !
 do i_k=1,k%nibz
   do i_n=SC_bands(1),SC_bands(2)
     !
     i_RT=i_RT+1
     i_k_rim=RT_ibz_coo(i_k,1)
     !
     if(i_n<=E%nbf) RT_occ_infinity(i_RT)=1._SP
     if(i_n> E%nbf) RT_occ_infinity(i_RT)=0._SP 
     !
     if(abs(E_E_Hole_lifetime(i_n,i_k_rim)+E_E_Electron_lifetime(i_n,i_k_rim))>0._SP) then
       RT_occ_infinity(i_RT)=E_E_Hole_lifetime(i_n,i_k_rim)/&
       &  (E_E_Hole_lifetime(i_n,i_k_rim)+E_E_Electron_lifetime(i_n,i_k_rim))
     endif
     !
   enddo
   !
 enddo
 !
 RT_occ_sorted(:)=RT_occ_infinity(E_sort_index(:))*spin_occ
 !
 T_fit=RT_Tfit_lifetimes(3:4)
 Ef_fit=RT_Efit_lifetimes(3:4)
 !
 nfit=Fermi_fnc_fit(RT_E,RT_occ_sorted,nstates,T_fit,Ef_fit,err_fit)
 !
 if (nfit==0) return
 !
 do ifit=1,2
   !
   l_condition(ifit)=err_fit(ifit+2)<0.05
   if (l_condition(ifit) ) Nfitted_lifetimes(ifit)=Nfitted_lifetimes(ifit)+1
   if (Nfitted_lifetimes(ifit)>500) l_lftm_fit_stable(ifit)=.true.
   !
   if (l_condition(ifit) .or. .not.l_lftm_fit_stable(ifit)) then
     RT_Tfit_lifetimes(ifit+2)=T_fit(ifit)
     RT_Efit_lifetimes(ifit+2)=Ef_fit(ifit)
   else if(RT_Tfit_lifetimes(ifit+2)>0._SP) then
     T_fit(ifit)=RT_Tfit_lifetimes(ifit+2)
     Ef_fit(ifit)=RT_Efit_lifetimes(ifit+2)
     l_condition(ifit)=.true.
   endif
   !
   if(.not.l_condition(ifit)) then
     Nfitted_lifetimes(ifit)=0
     RT_Tfit_lifetimes(ifit+2)=-RT_Tfit_lifetimes(ifit+2)
   endif
   !
 enddo
 !
 if(nfit==3) then
   if ( .not.l_condition(1) .and. .not.l_condition(2) ) return
   if ( .not.l_condition(2) ) nfit=1
   if ( .not.l_condition(1) ) nfit=2
 endif
 if ( nfit==1 .and. .not.l_condition(1) ) return
 if ( nfit==2 .and. .not.l_condition(2) ) return
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
       deviation(i_RT)=RT_occ_infinity(i_RT)-    &
       &                 Fermi_fnc(E%E(i_n,i_k,1)-Ef_fit(ifit),T_fit(ifit))
       !
       do i_k_rim=RT_ibz_coo(i_k,1)+1,RT_ibz_coo(i_k,2)
         !
         i_E_rim=i_k_rim-RT_ibz_coo(i_k,1)+RIM_ibz_coo(i_k,1)
         !
         x=Fermi_fnc(E%E_RIM(i_n,i_E_rim,1)-Ef_fit(ifit),T_fit(ifit))+deviation(i_RT) !*fac
         if(x<0._SP) x=0.0
         if(x>1._SP) x=1.0
         !
         if(E_E_Hole_lifetime(i_n,i_k_rim)    <1.E-6 ) cycle
         if(E_E_Electron_lifetime(i_n,i_k_rim)<1.E-6 ) cycle
         !
         alpha=0._SP
         if(abs(E_E_Electron_lifetime(i_n,i_k_rim)*x+E_E_Hole_lifetime(i_n,i_k_rim)*(1._SP-x))>0._SP) &
         & alpha= (E_E_Electron_lifetime(i_n,i_k_rim)*x-E_E_Hole_lifetime(i_n,i_k_rim)*(1._SP-x))/    &
         &        (E_E_Electron_lifetime(i_n,i_k_rim)*x+E_E_Hole_lifetime(i_n,i_k_rim)*(1._SP-x))
         !
         if(abs(alpha)>1._SP) alpha=alpha/abs(alpha)
         !
         E_E_Hole_lifetime(i_n,i_k_rim)    =E_E_Hole_lifetime(i_n,i_k_rim)    *(1._SP+alpha)
         E_E_Electron_lifetime(i_n,i_k_rim)=E_E_Electron_lifetime(i_n,i_k_rim)*(1._SP-alpha)
         !
       enddo
       !
     enddo
   enddo
   !
 enddo
 !
end subroutine RT_Coulomb_interpolation
