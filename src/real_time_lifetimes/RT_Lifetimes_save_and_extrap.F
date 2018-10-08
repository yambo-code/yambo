!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_Lifetimes_save_and_extrap(Time,EVAL,E,k,LIFE,SAVE_index)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elel_scatt,l_elphoton_scatt,l_elph_scatt
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:SAVED_times,Life_MEM_steps,SAVED_lifetime,RT_nk,&
&                         ADD_to_saved_values,RT_E_occupations,RT_E_occupations_reference,&
&                         RT_H_occupations,RT_H_occupations_reference,RT_bands,&
&                         l_RT_extrapolate_lifetimes,l_RT_constant_lifetimes
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k
 integer,       intent(in)    :: SAVE_index
 real(SP),      intent(in)    :: Time
 real(SP),      intent(inout) :: LIFE(RT_bands(1):RT_bands(2),RT_nk)
 logical                      :: EVAL
 !
 ! Work Space
 !
 integer          :: ib,ik
 logical          :: SAVE_it
 !
 if (EVAL) then
   !
   !======
   ! SAVE
   !======
   !
   ! ... the time points where the lifetimes have been saved ...
   !
   ! This call must be done only once. That's why it is called depending on the logicals
   !
   if (l_elel_scatt.and.SAVE_index==1) then
     SAVE_it=.TRUE.
   else if (l_elph_scatt.and.SAVE_index==3) then
     SAVE_it=.TRUE.
   else if (l_elphoton_scatt.and.SAVE_index==7) then
     SAVE_it=.TRUE.
   endif
   !
   ! ... and the lifetimes themeselves ...
   !
   do ik=1,RT_nk
     !
     do ib=RT_bands(1),RT_bands(2)
       !
       call ADD_to_saved_values(LIFE(ib,ik),SAVED_lifetime(ib,ik,SAVE_index,:),Life_MEM_steps)
       !
     enddo
   enddo
   !
   ! Store the occupations related to the last saved lifetimes
   !-----------------------------------------------------------
   if (SAVE_it) then
     RT_E_occupations_reference=RT_E_occupations
     RT_H_occupations_reference=RT_H_occupations
   endif
   !
 else if (l_RT_extrapolate_lifetimes.or.l_RT_constant_lifetimes) then 
   !
   do ik=1,RT_nk
     !
     do ib=RT_bands(1),RT_bands(2)
       !
       !==============
       ! EXTRAPOLATE 
       !=============
       !
       if (l_RT_extrapolate_lifetimes) then
         call RT_Lifetimes_Extrapolation(LIFE(ib,ik),Time,SAVED_lifetime(ib,ik,SAVE_index,:),SAVED_times)
       endif
       !
       !===============
       ! KEEP CONSTANT 
       !===============
       !
       if (l_RT_constant_lifetimes) then
         LIFE(ib,ik)=SAVED_lifetime(ib,ik,SAVE_index,Life_MEM_steps)
       endif
       !
     enddo
     !
   enddo
   !
 endif
 !
end subroutine RT_Lifetimes_save_and_extrap
