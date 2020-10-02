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
subroutine RT_mean_carrier_lifetimes(E,k)
 ! 
 use pars,           ONLY:SP
 use units,          ONLY:HA2KEL
 use RT_lifetimes,   ONLY:RT_EP_life,RT_EE_life,RT_EH_life,&
&                         RT_Tfit_lifetimes,RT_Efit_lifetimes,&
&                         REF_lifetime,l_lftm_fit_temperatures,RT_mean_lifetimes
 use real_time,      ONLY:RTibz,RT_nk,RT_bands
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
 real(SP)           :: n_states(1),pre_factor,sum_lifetimes(2)
 real(SP)           :: EE_occupations(RT_bands(1):RT_bands(2),RT_nk),&
&                      EP_occupations(RT_bands(1):RT_bands(2),RT_nk),&
&                      LOCAL_Lifetime(RT_bands(1):RT_bands(2),RT_nk,6)
 !
 if (allocated(RT_EE_life%Ho_abs))     LOCAL_Lifetime(:,:,1)=REF_lifetime(:,:,1)+RT_EE_life%Ho_abs
 if (allocated(RT_EE_life%El_emit)) LOCAL_Lifetime(:,:,2)=REF_lifetime(:,:,2)+RT_EE_life%El_emit
 if (allocated(RT_EP_life%Ho_abs))     LOCAL_Lifetime(:,:,3)=REF_lifetime(:,:,3)+RT_EP_life%Ho_abs
 if (allocated(RT_EP_life%El_emit)) LOCAL_Lifetime(:,:,4)=REF_lifetime(:,:,4)+RT_EP_life%El_emit
 if (allocated(RT_EP_life%Ho_emit))   LOCAL_Lifetime(:,:,5)=REF_lifetime(:,:,5)+RT_EP_life%Ho_emit
 if (allocated(RT_EP_life%El_abs))   LOCAL_Lifetime(:,:,6)=REF_lifetime(:,:,6)+RT_EP_life%El_abs
 !
 n_states(1)      =0._SP
 RT_mean_lifetimes=0._SP
 EE_occupations   =0._SP
 EP_occupations   =0._SP
 !
 do ik=1,RT_nk
   !
   if( .not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
   !
   pre_factor=RTibz%weights(ik)
   !
   do ib=RT_bands(1),RT_bands(2)
     !
     n_states(1)=n_states(1)+pre_factor
     !
     if (allocated(RT_EP_life%Ho_abs)) then
       sum_lifetimes(1)= LOCAL_Lifetime(ib,ik,3)+LOCAL_Lifetime(ib,ik,5)
       sum_lifetimes(2)= LOCAL_Lifetime(ib,ik,4)+LOCAL_Lifetime(ib,ik,6)+sum_lifetimes(1)
       if(sum_lifetimes(2)>0._SP) then
         EP_occupations(ib,ik)=sum_lifetimes(1)/sum_lifetimes(2)*spin_occ
       else
         if(ib<=E%nbf) EP_occupations(ib,ik)=spin_occ
         if(ib> E%nbf) EP_occupations(ib,ik)=0._SP
       endif
       RT_mean_lifetimes(3)=RT_mean_lifetimes(3)+(RT_EP_life%Ho_abs(ib,ik)+REF_lifetime(ib,ik,3))*pre_factor
       RT_mean_lifetimes(4)=RT_mean_lifetimes(4)+(RT_EP_life%El_emit(ib,ik)+REF_lifetime(ib,ik,4))*pre_factor
       RT_mean_lifetimes(5)=RT_mean_lifetimes(5)+(RT_EP_life%Ho_emit(ib,ik)+REF_lifetime(ib,ik,5))*pre_factor
       RT_mean_lifetimes(6)=RT_mean_lifetimes(6)+(RT_EP_life%El_abs(ib,ik)+REF_lifetime(ib,ik,6))*pre_factor
     endif
     !
     if (allocated(RT_EE_life%Ho_abs)) then
       sum_lifetimes(1)= LOCAL_Lifetime(ib,ik,1)
       sum_lifetimes(2)= LOCAL_Lifetime(ib,ik,1)+LOCAL_Lifetime(ib,ik,2)
       if(sum_lifetimes(2)>0._SP) then
         EE_occupations(ib,ik)=sum_lifetimes(1)/sum_lifetimes(2)*spin_occ
       else
         if(ib<=E%nbf) EE_occupations(ib,ik)=spin_occ
         if(ib> E%nbf) EE_occupations(ib,ik)=0._SP
       endif
       RT_mean_lifetimes(1)=RT_mean_lifetimes(1)+(RT_EE_life%Ho_abs(ib,ik)+REF_lifetime(ib,ik,1))*pre_factor
       RT_mean_lifetimes(2)=RT_mean_lifetimes(2)+(RT_EE_life%El_emit(ib,ik)+REF_lifetime(ib,ik,2))*pre_factor
     endif
     !
     if (allocated(RT_EH_life%Ho_emit)) then
       RT_mean_lifetimes(7)=RT_mean_lifetimes(7)+(RT_EH_life%Ho_emit(ib,ik)+REF_lifetime(ib,ik,7))*pre_factor
       RT_mean_lifetimes(8)=RT_mean_lifetimes(8)+(RT_EH_life%El_abs(ib,ik)+REF_lifetime(ib,ik,8))*pre_factor
     endif
     !
   enddo
   !
 enddo
 !
 call PP_redux_wait(RT_mean_lifetimes,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(EE_occupations,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(EP_occupations,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(n_states,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 RT_Tfit_lifetimes=0._SP
 if(allocated(RT_EP_life%Ho_abs).and.l_lftm_fit_temperatures) then
   call RT_occupations_FIT(RT_Tfit_lifetimes(1:2),RT_Efit_lifetimes(1:2),EP_occupations)
   if (RT_Tfit_lifetimes(1)>50000/HA2KEL.or.RT_Tfit_lifetimes(1)<0._SP) RT_Tfit_lifetimes(1)=0._SP
   if (RT_Tfit_lifetimes(2)>50000/HA2KEL.or.RT_Tfit_lifetimes(2)<0._SP) RT_Tfit_lifetimes(2)=0._SP
 endif
 !
 if(allocated(RT_EE_life%Ho_abs).and.l_lftm_fit_temperatures) then
   call RT_occupations_FIT(RT_Tfit_lifetimes(3:4),RT_Efit_lifetimes(3:4),EE_occupations)
   if (RT_Tfit_lifetimes(3)>50000/HA2KEL.or.RT_Tfit_lifetimes(3)<0._SP) RT_Tfit_lifetimes(3)=0._SP
   if (RT_Tfit_lifetimes(4)>50000/HA2KEL.or.RT_Tfit_lifetimes(4)<0._SP) RT_Tfit_lifetimes(4)=0._SP
 endif
 !
 RT_mean_lifetimes=RT_mean_lifetimes/n_states(1)
 !
end subroutine RT_mean_carrier_lifetimes
