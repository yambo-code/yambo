!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine RT_Lifetimes_save_and_interp(Time,EVAL,E,k,LIFE,SAVE_index)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use units,          ONLY:FS2AUT
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,it_now,RT_nk,RT_ibz_coo
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_Plasma_INDEX
 use real_time,      ONLY:RT_TIME_status,SAVED_times,FIRST_step,&
&                         Life_MEM_steps,SAVED_lifetime,RT_dynamics_steps,&
&                         ADD_to_saved_values,l_initial_equilibrium,&
&                         RT_E_occupations,RT_E_occupations_reference,&
&                         RT_H_occupations,RT_H_occupations_reference,&
&                         I_relax_dyn
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k
 integer,       intent(in)    :: SAVE_index
 real(SP),      intent(in)    :: Time
 real(SP),      intent(inout) :: LIFE(SC_bands(1):SC_bands(2),RT_nk)
 logical                      :: EVAL
 !
 ! Work Space
 !
 integer          :: ib,ik,ik_rim
 !
 if (RT_dynamics_steps(1)<0.) return
 !
 if (EVAL) then
   !
   !======
   ! SAVE
   !======
   if (l_elel_scatt     .and.SAVE_index==1) call ADD_to_saved_values(Time,SAVED_times,Life_MEM_steps)
   if (.not.l_elel_scatt.and.SAVE_index==3) call ADD_to_saved_values(Time,SAVED_times,Life_MEM_steps)
   !
   do ib=SC_bands(1),SC_bands(2)
     do ik=1,RT_nk
       call ADD_to_saved_values(LIFE(ib,ik),SAVED_lifetime(ib,ik,SAVE_index,:),Life_MEM_steps)
       !
       if (SAVE_index==1.or.SAVE_index==3.or.SAVE_index==5) then
         I_relax_dyn(ib,ik,1,1)=I_relax_dyn(ib,ik,1,1)+LIFE(ib,ik)
       else
         I_relax_dyn(ib,ik,2,1)=I_relax_dyn(ib,ik,2,1)+LIFE(ib,ik)
       endif
       !
     enddo
   enddo
   !
   ! Store the occupations related to the last saved lifetimes
   !-----------------------------------------------------------
   if (SAVE_index==1.or.(.not.l_elel_scatt.and.SAVE_index==3)) then
     RT_E_occupations_reference=RT_E_occupations
     RT_H_occupations_reference=RT_H_occupations
   endif
   !
 else
   !
   if (RT_TIME_status(it_now)==0 .or. (RT_TIME_status(it_now)>0.and..not.FIRST_step(it_now,Time)) ) then
     !==============
     ! INTERPOLATE 
     !=============
     do ib=SC_bands(1),SC_bands(2)
       do ik=1,RT_nk
         call RT_Lifetimes_Interpolation(ib,ik,SAVE_index,LIFE(ib,ik),Time,SAVED_lifetime(ib,ik,SAVE_index,:),SAVED_times)
       enddo
     enddo
     !
   else if (RT_TIME_status(it_now)<0) then
     !===============
     ! KEEP CONSTANT 
     !===============
     !
     LIFE=SAVED_lifetime(:,:,SAVE_index,Life_MEM_steps)
     !
   endif
 endif
 !
end subroutine RT_Lifetimes_save_and_interp

