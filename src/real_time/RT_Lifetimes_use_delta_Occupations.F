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
subroutine RT_Lifetimes_use_delta_Occupations(G,l_eval)
 !
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use pars,           ONLY:SP
 use SC,             ONLY:SC_bands,RT_nk,RT_occupations,it_now
 use parallel_m,     ONLY:ncpu
 use real_time,      ONLY:RT_occupations_locked,RT_occupations_reference,&
&                         RT_delta_occupations,l_use_delta_occupations,  &
&                         RT_one_minus_occupations,RT_GAMMA_T_status,&
&                         SAVED_lifetime,Life_MEM_steps,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,E_P_Hole_lifetime,&
&                         E_P_Ho_abs_lifetime,E_P_El_abs_lifetime,E_P_Electron_lifetime
 implicit none
 !
 complex(SP),   intent(inout) :: G(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 logical                      :: l_eval
 !
 ! Work Space
 !
 integer :: ik,ib
 real(SP):: f_sum,f_diff,f_occ,f_occ_reference
 !
 l_use_delta_occupations=RT_GAMMA_T_status(it_now)> 1
 !
 if (.not.l_eval) return
 !
 do ik=1,RT_nk
   do ib=SC_bands(1),SC_bands(2)
     !
     RT_occupations_locked(ib,ik)=.FALSE. 
     !
     f_occ=-real((0._SP,1._SP)*G(ib,ib,ik))
     f_occ_reference=RT_occupations_reference(ib,ik)
     !
     if (l_use_delta_occupations) then
       f_sum  =  (f_occ+f_occ_reference)/2.
       f_diff =   f_occ-f_occ_reference
       if (abs(f_sum)>1.E-5) then
         if (abs(f_diff/f_sum)<1.E-5) RT_occupations_locked(ib,ik)=.TRUE.
       endif
       RT_occupations(ib,ik)          = 0._SP
       RT_one_minus_occupations(ib,ik)= 0._SP
       RT_delta_occupations(ib,ik)    = f_diff
       RT_occupations_reference(ib,ik)= f_occ
       if (l_elel_scatt) then
         E_E_Hole_lifetime(ib,ik)=    SAVED_lifetime(ib,ik,1,Life_MEM_steps)/real(ncpu)
         E_E_Electron_lifetime(ib,ik)=SAVED_lifetime(ib,ik,2,Life_MEM_steps)/real(ncpu)
       endif
       if (l_elph_scatt) then
         E_P_Hole_lifetime(ib,ik)=    SAVED_lifetime(ib,ik,3,Life_MEM_steps)/real(ncpu)
         E_P_Electron_lifetime(ib,ik)=SAVED_lifetime(ib,ik,4,Life_MEM_steps)/real(ncpu)
         E_P_Ho_abs_lifetime(ib,ik)=  SAVED_lifetime(ib,ik,5,Life_MEM_steps)/real(ncpu)
         E_P_El_abs_lifetime(ib,ik)=  SAVED_lifetime(ib,ik,6,Life_MEM_steps)/real(ncpu)
       endif
     else 
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
       RT_occupations(ib,ik)          = f_occ
       RT_one_minus_occupations(ib,ik)= 2._SP-f_occ
       RT_delta_occupations(ib,ik)    = 0._SP
       RT_occupations_reference(ib,ik)= 0._SP
     endif
     !
   enddo
 enddo
 !
end subroutine RT_Lifetimes_use_delta_Occupations
