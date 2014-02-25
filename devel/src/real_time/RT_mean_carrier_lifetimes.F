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
subroutine RT_mean_carrier_lifetimes(E,k)
 ! 
 use pars,           ONLY:SP,cI
 use real_time,      ONLY:E_P_Hole_lifetime,E_P_Electron_lifetime,E_P_El_abs_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,RT_mean_lifetimes,  &
&                         E_P_Ho_abs_lifetime,RT_Tfit_lifetimes,RT_Efit_lifetimes,    &
&                         NE_i_time,RT_IO_output_steps,l_RT_uses_E_rim
 use SC,             ONLY:SC_bands,RT_all2ibz,RT_Nk_ibz,RT_nk
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin_occ
 implicit none
 !
 type(levels)           :: E
 type(bz_samp)          :: k
 !
 ! Work space
 !
 integer            :: ib,ik
 real(SP)           :: n_states,pre_factor,sum_lifetimes(2)
 real(SP)           :: EE_occupations(SC_bands(1):SC_bands(2),RT_nk),&
&                      EP_occupations(SC_bands(1):SC_bands(2),RT_nk)
 !
 if ( mod(NE_i_time-1,RT_IO_output_steps)/=0 ) return
 !
 n_states=0._SP
 !
 RT_mean_lifetimes=0._SP
 !
 do ik=1,RT_nk
   !
   pre_factor=k%weights(RT_all2ibz(ik))/real(RT_Nk_ibz(RT_all2ibz(ik)))
   !
   do ib=SC_bands(1),SC_bands(2)
     !
     n_states=n_states+pre_factor
     !
     if (allocated(E_P_Hole_lifetime)) then
       sum_lifetimes(1)= E_P_Hole_lifetime(ib,ik)+E_P_Ho_abs_lifetime(ib,ik)
       sum_lifetimes(2)= E_P_Electron_lifetime(ib,ik)+E_P_El_abs_lifetime(ib,ik)+sum_lifetimes(1)
       if(sum_lifetimes(2)>0._SP) then
         EP_occupations(ib,ik)=sum_lifetimes(1)/sum_lifetimes(2)*spin_occ
       else
         if(ib<=E%nbf) EP_occupations(ib,ik)=spin_occ
         if(ib> E%nbf) EP_occupations(ib,ik)=0._SP
       endif
     endif
     !
     if (allocated(E_E_Hole_lifetime).and..not.l_RT_uses_E_rim) then
       sum_lifetimes(1)= E_E_Hole_lifetime(ib,ik)
       sum_lifetimes(2)= E_E_Electron_lifetime(ib,ik)+E_E_Hole_lifetime(ib,ik)
       if(sum_lifetimes(2)>0._SP) then
         EE_occupations(ib,ik)=sum_lifetimes(1)/sum_lifetimes(2)*spin_occ
       else
         if(ib<=E%nbf) EE_occupations(ib,ik)=spin_occ
         if(ib> E%nbf) EE_occupations(ib,ik)=0._SP
       endif
     endif
     !
     if (allocated(E_P_Hole_lifetime)) then
       RT_mean_lifetimes(2)=RT_mean_lifetimes(2)+E_P_Electron_lifetime(ib,ik)*pre_factor
       RT_mean_lifetimes(4)=RT_mean_lifetimes(4)+E_P_El_abs_lifetime(ib,ik)*pre_factor
     endif
     if (allocated(E_E_Hole_lifetime)) then
       RT_mean_lifetimes(6)=RT_mean_lifetimes(6)+E_E_Electron_lifetime(ib,ik)*pre_factor
     endif
     !
     if (allocated(E_P_Hole_lifetime)) then
       RT_mean_lifetimes(1)=RT_mean_lifetimes(1)+E_P_Hole_lifetime(ib,ik)*pre_factor
       RT_mean_lifetimes(3)=RT_mean_lifetimes(3)+E_P_Ho_abs_lifetime(ib,ik)*pre_factor
     endif
     if (allocated(E_E_Hole_lifetime)) then
       RT_mean_lifetimes(5)=RT_mean_lifetimes(5)+E_E_Hole_lifetime(ib,ik)*pre_factor
     endif
     !
   enddo
   !
 enddo
 !
 if(allocated(E_P_Hole_lifetime)) then
   call RT_occupations_FIT_and_lock(E,k,RT_Tfit_lifetimes(1:2),RT_Efit_lifetimes(1:2),EP_occupations)
 endif
 if(allocated(E_E_Hole_lifetime).and. (.not.l_RT_uses_E_rim)) then
   call RT_occupations_FIT_and_lock(E,k,RT_Tfit_lifetimes(3:4),RT_Efit_lifetimes(3:4),EE_occupations)
 endif
 !
 RT_mean_lifetimes=RT_mean_lifetimes/n_states
 !
end subroutine RT_mean_carrier_lifetimes
