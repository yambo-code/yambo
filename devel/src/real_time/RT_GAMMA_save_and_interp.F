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
subroutine RT_GAMMA_save_and_interp(Time,E,k,LIFE,SAVE_index)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elel_scatt
 use units
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,it_now,RT_nk,RT_ibz_coo
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_Plasma_INDEX
 use real_time,      ONLY:it_start,RT_GAMMA_T_status,RT_step,SAVED_times,&
&                         Life_MEM_steps,SAVED_lifetime,RT_dynamics_steps
 !
 implicit none
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k
 integer,       intent(in)    :: SAVE_index
 real(SP),      intent(in)    :: Time
 real(SP),      intent(inout) :: LIFE(SC_bands(1):SC_bands(2),RT_nk)
 !
 ! Work Space
 !
 integer          :: ib,ik,ik_rim
 !
 if (RT_GAMMA_T_status(it_now)>0) then 
   !=============================
   if (SAVE_index<=2) then
     do ik=1,k%nibz
       ik_rim=RT_ibz_coo(ik,1)
       call PP_redux_wait(LIFE(:,ik_rim))
     enddo
     if (SAVE_index==2) call RT_Coulomb_interpolation(E,k)
   else
     do ik=1,RT_nk
       LIFE(:,ik)=LIFE(:,ik)/real(PAR_COM_Plasma_INDEX%n_CPU)
       call PP_redux_wait(LIFE(:,ik))
     enddo
   endif
 endif
 !
 if (RT_dynamics_steps(1)<0.) return
 !
 if (RT_GAMMA_T_status(it_now)>0.and.abs(Time-(it_now-1)*RT_step)<1.E-6) then
   !=============================
   !
   if (l_elel_scatt     .and.SAVE_index==1) call ADD_to_saved_values(Time,SAVED_times)
   if (.not.l_elel_scatt.and.SAVE_index==3) call ADD_to_saved_values(Time,SAVED_times)
   !
   do ib=SC_bands(1),SC_bands(2)
     if (SAVE_index<=2) then
       ! e-e case
       do ik=1,k%nibz
         ik_rim=RT_ibz_coo(ik,1)
         call ADD_to_saved_values(LIFE(ib,ik_rim),SAVED_lifetime(ib,ik_rim,SAVE_index,:))
       enddo
     else
       ! e-p case
       do ik=1,RT_nk
         call ADD_to_saved_values(LIFE(ib,ik),SAVED_lifetime(ib,ik,SAVE_index,:))
       enddo
     endif
   enddo
 endif
 !
 if (RT_GAMMA_T_status(it_now)==0) then
   !==============================
   do ib=SC_bands(1),SC_bands(2)
     if (SAVE_index<=2) then
       ! e-e case
       do ik=1,k%nibz
         ik_rim=RT_ibz_coo(ik,1)
         call RT_Lifetimes_Interpolation(LIFE(ib,ik_rim),SAVED_lifetime(ib,ik_rim,SAVE_index,:),SAVED_times)
       enddo
       if (SAVE_index==2) call RT_Coulomb_interpolation(E,k)
     else
       ! e-p case
       do ik=1,RT_nk
         call RT_Lifetimes_Interpolation(LIFE(ib,ik),SAVED_lifetime(ib,ik,SAVE_index,:),SAVED_times)
       enddo
     endif
   enddo
 endif
 !
 if (RT_GAMMA_T_status(it_now)<0) then
   if (SAVE_index<=2) then
     ! e-e case
     do ik=1,k%nibz
       ik_rim=RT_ibz_coo(ik,1)
       LIFE(:,ik_rim)=SAVED_lifetime(:,ik_rim,SAVE_index,Life_MEM_steps)
     enddo
     if (SAVE_index==2) call RT_Coulomb_interpolation(E,k)
   else
     LIFE(:,:)=SAVED_lifetime(:,:,SAVE_index,Life_MEM_steps)
   endif
   if (SAVE_index==2) call RT_Coulomb_interpolation(E,k)
 endif
 !
 contains
   !
   subroutine ADD_to_saved_values(VAL,ARRAY)
     real(SP) :: VAL,ARRAY(Life_MEM_steps)
     integer  :: i1
     do i1=1,Life_MEM_steps-1
       ARRAY(i1)=ARRAY(i1+1)
     enddo
     ARRAY(Life_MEM_steps)=VAL
   end subroutine
   !
end subroutine RT_GAMMA_save_and_interp

