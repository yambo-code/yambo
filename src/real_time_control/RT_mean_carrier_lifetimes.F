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
subroutine RT_mean_carrier_lifetimes( LIFE, LIFE_ref )
 ! 
 use pars,           ONLY:SP
 use RT_lifetimes_DO,ONLY:ABS_RT_life_one_KIND,EMIT_RT_life_one_KIND
 use RT_lifetimes,   ONLY:RT_lifetime
 use real_time,      ONLY:RTibz,RT_nk,RT_bands
 use electrons,      ONLY:spin_occ,n_full_bands
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
 !
 implicit none
 !
 type(RT_lifetime), intent(inout) :: LIFE, LIFE_ref
 !
 ! Work space
 !
 integer            :: ib,ik
 real(SP)           :: n_states(1),pre_factor,sum_lifetimes(2)
 real(SP)           :: EE_occupations(RT_bands(1):RT_bands(2),RT_nk),&
&                      EP_occupations(RT_bands(1):RT_bands(2),RT_nk)
 !
 if (.not.LIFE%active) return
 !
 n_states(1)     =0._SP
 LIFE%SPONT_mean =0._SP
 LIFE%STIM_mean  =0._SP
 EE_occupations  =0._SP
 EP_occupations  =0._SP
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
     sum_lifetimes(1)= ABS_RT_life_one_KIND(LIFE,LIFE_ref,ib,ik)
     sum_lifetimes(2)= sum_lifetimes(1)+EMIT_RT_life_one_KIND(LIFE,LIFE_ref,ib,ik)
     if(sum_lifetimes(2)>0._SP) then
       EP_occupations(ib,ik)=sum_lifetimes(1)/sum_lifetimes(2)*spin_occ
     else
       if(ib<=n_full_bands) EP_occupations(ib,ik)=spin_occ
       if(ib> n_full_bands) EP_occupations(ib,ik)=0._SP
     endif
     !
     LIFE%SPONT_mean(1)=LIFE%SPONT_mean(1)+(LIFE%SPONT_abs(ib,ik)+LIFE_ref%SPONT_abs(ib,ik))*pre_factor
     LIFE%SPONT_mean(2)=LIFE%SPONT_mean(2)+(LIFE%SPONT_emit(ib,ik)+LIFE_ref%SPONT_emit(ib,ik))*pre_factor
     !
     if (.not.LIFE%STIMULATED_components) cycle
     !
     LIFE%STIM_mean(1)=LIFE%STIM_mean(1)+(LIFE%STIM_emit(ib,ik)+LIFE_ref%STIM_emit(ib,ik))*pre_factor
     LIFE%STIM_mean(2)=LIFE%STIM_mean(2)+(LIFE%STIM_abs(ib,ik)+LIFE_ref%STIM_abs(ib,ik))*pre_factor
     !
   enddo
   !
 enddo
 !
 call PP_redux_wait(LIFE%SPONT_mean,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(LIFE%STIM_mean, COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(EE_occupations,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(EP_occupations,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(n_states,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 LIFE%SPONT_mean=LIFE%SPONT_mean/n_states(1)
 LIFE%STIM_mean =LIFE%STIM_mean/n_states(1)
 !
! RT_Tfit_lifetimes=0._SP
! if(allocated(RT_EP_life%SPONT_abs).and.l_lftm_fit_temperatures) then
!   call RT_occupations_FIT(RT_Tfit_lifetimes(1:2),RT_Efit_lifetimes(1:2),EP_occupations)
!   if (RT_Tfit_lifetimes(1)>50000/HA2KEL.or.RT_Tfit_lifetimes(1)<0._SP) RT_Tfit_lifetimes(1)=0._SP
!   if (RT_Tfit_lifetimes(2)>50000/HA2KEL.or.RT_Tfit_lifetimes(2)<0._SP) RT_Tfit_lifetimes(2)=0._SP
! endif
! !
! if(allocated(RT_EE_life%SPONT_abs).and.l_lftm_fit_temperatures) then
!   call RT_occupations_FIT(RT_Tfit_lifetimes(3:4),RT_Efit_lifetimes(3:4),EE_occupations)
!   if (RT_Tfit_lifetimes(3)>50000/HA2KEL.or.RT_Tfit_lifetimes(3)<0._SP) RT_Tfit_lifetimes(3)=0._SP
!   if (RT_Tfit_lifetimes(4)>50000/HA2KEL.or.RT_Tfit_lifetimes(4)<0._SP) RT_Tfit_lifetimes(4)=0._SP
! endif
 !
end subroutine RT_mean_carrier_lifetimes
