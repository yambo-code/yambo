!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_Lifetimes_save_and_extrap(Time,EVAL,LIFE)
 !
 use pars,           ONLY:SP
 use RT_lifetimes,   ONLY:SAVED_times,Life_MEM_steps,l_RT_extrapolate_lifetimes,l_RT_constant_lifetimes,RT_lifetime
 use RT_operations,  ONLY:ADD_to_saved_values
 use real_time,      ONLY:RT_nk,RT_E_occupations,RT_E_occupations_reference,&
&                         RT_H_occupations,RT_H_occupations_reference,RT_bands,RTibz
 use parallel_m,     ONLY:PAR_IND_Xk_ibz
 !
 implicit none
 !
 real(SP),          intent(in)    :: Time
 logical                          :: EVAL
 type(RT_lifetime), intent(inout) :: LIFE
 !
 ! Work Space
 !
 integer          :: ib,ik
 !
 if (.not.LIFE%active) return
 !
 if (EVAL) then
   !
   !======
   ! SAVE
   !======
   !
   ! The lifetimes themeselves ...
   !
   do ik=1,RT_nk
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
     !
     do ib=RT_bands(1),RT_bands(2)
       !
       call ADD_to_saved_values(LIFE%emit(ib,ik),LIFE%emit_save(ib,ik,:),Life_MEM_steps)
       call ADD_to_saved_values(LIFE%abs(ib,ik), LIFE%abs_save(ib,ik,:), Life_MEM_steps)
       !
     enddo
   enddo
   !
   ! Store the occupations related to the last saved lifetimes
   !-----------------------------------------------------------
   if (.not.trim(LIFE%kind)=="el-ph_abs") then
     RT_E_occupations_reference=RT_E_occupations
     RT_H_occupations_reference=RT_H_occupations
   endif
   !
 else if (l_RT_extrapolate_lifetimes.or.l_RT_constant_lifetimes) then 
   !
   do ik=1,RT_nk
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
     !
     do ib=RT_bands(1),RT_bands(2)
       !
       !==============
       ! EXTRAPOLATE 
       !=============
       !
       if (l_RT_extrapolate_lifetimes) then
         call RT_Lifetimes_Extrapolation(LIFE%emit(ib,ik),Time,LIFE%emit_save(ib,ik,:),SAVED_times)
         call RT_Lifetimes_Extrapolation(LIFE%abs(ib,ik),Time,LIFE%abs_save(ib,ik,:),SAVED_times)
       endif
       !
       !===============
       ! KEEP CONSTANT 
       !===============
       !
       if (l_RT_constant_lifetimes) then
         LIFE%emit(ib,ik)=LIFE%emit_save(ib,ik,Life_MEM_steps)
         LIFE%abs(ib,ik)=LIFE%abs_save(ib,ik,Life_MEM_steps)
       endif
       !
     enddo
     !
   enddo
   !
 endif
 !
end subroutine RT_Lifetimes_save_and_extrap
