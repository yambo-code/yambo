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
subroutine RT_mean_carrier_lifetimes(E,k)
 ! 
 use pars,           ONLY:SP
 use units,          ONLY:HA2KEL
 use real_time,      ONLY:E_P_Hole_lifetime,E_P_Electron_lifetime,E_P_El_abs_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,RT_mean_lifetimes,  &
&                         E_P_Ho_abs_lifetime,RT_Tfit_lifetimes,RT_Efit_lifetimes,    &
&                         REF_lifetime,RT_all2ibz,RT_Nk_ibz,RT_nk,RT_bands,           &
&                         E_PHOT_Hole_lifetime, E_PHOT_Electron_lifetime,             &
&                         l_lftm_fit_temperatures,RT_all2ibz
 use RT_control,     ONLY:RT_do_it
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin_occ
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
 !
 implicit none
 !
 type(levels)           :: E
 type(bz_samp)          :: k
 !
 ! Work space
 !
 integer            :: ib,ik
 real(SP)           :: n_states,pre_factor,sum_lifetimes(2)
 real(SP)           :: EE_occupations(RT_bands(1):RT_bands(2),RT_nk),&
&                      EP_occupations(RT_bands(1):RT_bands(2),RT_nk),&
&                      LOCAL_Lifetime(RT_bands(1):RT_bands(2),RT_nk,6)
 !
 if (.not.RT_do_it('OUT')) return
 !
 if (allocated(E_E_Hole_lifetime))     LOCAL_Lifetime(:,:,1)=REF_lifetime(:,:,1)+E_E_Hole_lifetime
 if (allocated(E_E_Electron_lifetime)) LOCAL_Lifetime(:,:,2)=REF_lifetime(:,:,2)+E_E_Electron_lifetime
 if (allocated(E_P_Hole_lifetime))     LOCAL_Lifetime(:,:,3)=REF_lifetime(:,:,3)+E_P_Hole_lifetime
 if (allocated(E_P_Electron_lifetime)) LOCAL_Lifetime(:,:,4)=REF_lifetime(:,:,4)+E_P_Electron_lifetime
 if (allocated(E_P_Ho_abs_lifetime))   LOCAL_Lifetime(:,:,5)=REF_lifetime(:,:,5)+E_P_Ho_abs_lifetime
 if (allocated(E_P_El_abs_lifetime))   LOCAL_Lifetime(:,:,6)=REF_lifetime(:,:,6)+E_P_El_abs_lifetime
 !
 n_states         =0._SP
 RT_mean_lifetimes=0._SP
 EE_occupations   =0._SP
 EP_occupations   =0._SP
 !
 do ik=1,RT_nk
   !
   if( .not.PAR_IND_Xk_ibz%element_1D(RT_all2ibz(ik)) ) cycle
   !
   pre_factor=k%weights(RT_all2ibz(ik))/real(RT_Nk_ibz(RT_all2ibz(ik)))
   !
   do ib=RT_bands(1),RT_bands(2)
     !
     n_states=n_states+pre_factor
     !
     if (allocated(E_P_Hole_lifetime)) then
       sum_lifetimes(1)= LOCAL_Lifetime(ib,ik,3)+LOCAL_Lifetime(ib,ik,5)
       sum_lifetimes(2)= LOCAL_Lifetime(ib,ik,4)+LOCAL_Lifetime(ib,ik,6)+sum_lifetimes(1)
       if(sum_lifetimes(2)>0._SP) then
         EP_occupations(ib,ik)=sum_lifetimes(1)/sum_lifetimes(2)*spin_occ
       else
         if(ib<=E%nbf) EP_occupations(ib,ik)=spin_occ
         if(ib> E%nbf) EP_occupations(ib,ik)=0._SP
       endif
       RT_mean_lifetimes(3)=RT_mean_lifetimes(3)+(E_P_Hole_lifetime(ib,ik)+REF_lifetime(ib,ik,3))*pre_factor
       RT_mean_lifetimes(4)=RT_mean_lifetimes(4)+(E_P_Electron_lifetime(ib,ik)+REF_lifetime(ib,ik,4))*pre_factor
       RT_mean_lifetimes(5)=RT_mean_lifetimes(5)+(E_P_Ho_abs_lifetime(ib,ik)+REF_lifetime(ib,ik,5))*pre_factor
       RT_mean_lifetimes(6)=RT_mean_lifetimes(6)+(E_P_El_abs_lifetime(ib,ik)+REF_lifetime(ib,ik,6))*pre_factor
     endif
     !
     if (allocated(E_E_Hole_lifetime)) then
       sum_lifetimes(1)= LOCAL_Lifetime(ib,ik,1)
       sum_lifetimes(2)= LOCAL_Lifetime(ib,ik,1)+LOCAL_Lifetime(ib,ik,2)
       if(sum_lifetimes(2)>0._SP) then
         EE_occupations(ib,ik)=sum_lifetimes(1)/sum_lifetimes(2)*spin_occ
       else
         if(ib<=E%nbf) EE_occupations(ib,ik)=spin_occ
         if(ib> E%nbf) EE_occupations(ib,ik)=0._SP
       endif
       RT_mean_lifetimes(1)=RT_mean_lifetimes(1)+(E_E_Hole_lifetime(ib,ik)+REF_lifetime(ib,ik,1))*pre_factor
       RT_mean_lifetimes(2)=RT_mean_lifetimes(2)+(E_E_Electron_lifetime(ib,ik)+REF_lifetime(ib,ik,2))*pre_factor
     endif
     !
     if (allocated(E_PHOT_Hole_lifetime)) then
       RT_mean_lifetimes(7)=RT_mean_lifetimes(7)+(E_PHOT_Hole_lifetime(ib,ik)+REF_lifetime(ib,ik,7))*pre_factor
       RT_mean_lifetimes(8)=RT_mean_lifetimes(8)+(E_PHOT_Electron_lifetime(ib,ik)+REF_lifetime(ib,ik,8))*pre_factor
     endif
     !
   enddo
   !
 enddo
 !
 call PP_redux_wait(RT_mean_lifetimes,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(EE_occupations,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(EP_occupations,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 RT_Tfit_lifetimes=0._SP
 if(allocated(E_P_Hole_lifetime).and.l_lftm_fit_temperatures) then
   call RT_occupations_FIT(RT_Tfit_lifetimes(1:2),RT_Efit_lifetimes(1:2),EP_occupations)
   if (RT_Tfit_lifetimes(1)>50000/HA2KEL.or.RT_Tfit_lifetimes(1)<0._SP) RT_Tfit_lifetimes(1)=0._SP
   if (RT_Tfit_lifetimes(2)>50000/HA2KEL.or.RT_Tfit_lifetimes(2)<0._SP) RT_Tfit_lifetimes(2)=0._SP
 endif
 !
 if(allocated(E_E_Hole_lifetime).and.l_lftm_fit_temperatures) then
   call RT_occupations_FIT(RT_Tfit_lifetimes(3:4),RT_Efit_lifetimes(3:4),EE_occupations)
   if (RT_Tfit_lifetimes(3)>50000/HA2KEL.or.RT_Tfit_lifetimes(3)<0._SP) RT_Tfit_lifetimes(3)=0._SP
   if (RT_Tfit_lifetimes(4)>50000/HA2KEL.or.RT_Tfit_lifetimes(4)<0._SP) RT_Tfit_lifetimes(4)=0._SP
 endif
 !
 RT_mean_lifetimes=RT_mean_lifetimes/n_states
 !
end subroutine RT_mean_carrier_lifetimes
