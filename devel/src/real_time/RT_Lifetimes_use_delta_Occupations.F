!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
subroutine RT_Lifetimes_use_delta_Occupations(l_eval)
 !
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use electrons,      ONLY:spin_occ
 use pars,           ONLY:SP
 use SC,             ONLY:SC_bands,RT_nk,it_now
 use parallel_m,     ONLY:ncpu
 use real_time,      ONLY:RT_occupations_locked,RT_E_occupations_reference,RT_H_occupations_reference,&
&                         RT_delta_occupations,l_use_updated_reference,l_equilibrium_lifetimes,&
&                         RT_H_occupations,RT_E_occupations,RT_TIME_status,RT_delta0_occupations,&
&                         SAVED_lifetime,Life_MEM_steps,REF_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,E_P_Hole_lifetime,&
&                         E_P_Ho_abs_lifetime,E_P_El_abs_lifetime,E_P_Electron_lifetime,&
&                         G_lesser_reference
 !
 implicit none
 !
 logical                      :: l_eval
 !
 ! Work Space
 !
 integer :: ik,ib
 real(SP):: f_sum,f_diff,f_E_occ,f_H_occ,f_E_occ_reference,f_H_occ_reference
 !
 !
 l_use_updated_reference=.not.l_equilibrium_lifetimes
 if(l_use_updated_reference) l_use_updated_reference=(RT_TIME_status(it_now)> 1)
 !
 if (.not.l_eval) return
 !
 do ik=1,RT_nk
   do ib=SC_bands(1),SC_bands(2)
     !
     RT_occupations_locked(ib,ik)=.FALSE. 
     !
     if (l_use_updated_reference) then
       !
       f_E_occ=RT_E_occupations(ib,ik)
       f_H_occ=RT_H_occupations(ib,ik)
       f_E_occ_reference=RT_E_occupations_reference(ib,ik)
       f_H_occ_reference=RT_H_occupations_reference(ib,ik)
       !
       ! We need to establish a criterium for locking ...
       !f_sum  =  (f_occ+f_occ_reference)/2.
       !if (abs(f_sum)>1.E-5) then
       !  if (abs(f_diff/f_sum)<1.E-5) RT_occupations_locked(ib,ik)=.TRUE.
       !endif
       !
       if(f_E_occ<=0.5*spin_occ) RT_delta_occupations(ib,ik) =   f_E_occ-f_E_occ_reference
       if(f_E_occ> 0.5*spin_occ) RT_delta_occupations(ib,ik) = -(f_H_occ-f_H_occ_reference)
       !
       if (l_elel_scatt) then
         E_E_Hole_lifetime(ib,ik)=    (SAVED_lifetime(ib,ik,1,Life_MEM_steps)-REF_lifetime(ib,ik,1))/real(ncpu)
         E_E_Electron_lifetime(ib,ik)=(SAVED_lifetime(ib,ik,2,Life_MEM_steps)-REF_lifetime(ib,ik,2))/real(ncpu)
       endif
       if (l_elph_scatt) then
         E_P_Hole_lifetime(ib,ik)     =(SAVED_lifetime(ib,ik,3,Life_MEM_steps)-REF_lifetime(ib,ik,3))/real(ncpu)
         E_P_Electron_lifetime(ib,ik) =(SAVED_lifetime(ib,ik,4,Life_MEM_steps)-REF_lifetime(ib,ik,4))/real(ncpu)
         E_P_Ho_abs_lifetime(ib,ik)   =(SAVED_lifetime(ib,ik,5,Life_MEM_steps)-REF_lifetime(ib,ik,5))/real(ncpu)
         E_P_El_abs_lifetime(ib,ik)   =(SAVED_lifetime(ib,ik,6,Life_MEM_steps)-REF_lifetime(ib,ik,6))/real(ncpu)
       endif
       !
     else
       !
       RT_delta_occupations(ib,ik)=RT_delta0_occupations(ib,ik)
       RT_delta_occupations(ib,ik)=RT_delta0_occupations(ib,ik)
       RT_E_occupations_reference(ib,ik)=         aimag(G_lesser_reference(ib,ib,ik))
       RT_H_occupations_reference(ib,ik)=spin_occ-aimag(G_lesser_reference(ib,ib,ik))
       !
       if (l_elel_scatt) then
         E_E_Hole_lifetime(ib,ik)=    0._SP
         E_E_Electron_lifetime(ib,ik)=0._SP
       endif
       if (l_elph_scatt) then
         E_P_Hole_lifetime(ib,ik)=    0._SP
         E_P_Electron_lifetime(ib,ik)=0._SP
         E_P_Ho_abs_lifetime(ib,ik)=  0._SP
         E_P_El_abs_lifetime(ib,ik)=  0._SP
       endif
     endif
     !
   enddo
 enddo
 !
end subroutine RT_Lifetimes_use_delta_Occupations
