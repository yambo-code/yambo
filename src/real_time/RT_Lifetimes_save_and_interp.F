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
subroutine RT_Lifetimes_save_and_interp(Time,EVAL,E,k,LIFE,SAVE_index)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use units,          ONLY:FS2AUT
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,it_now,RT_nk,RT_ibz_coo
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_Plasma_INDEX
 use real_time,      ONLY:RT_TIME_status,SAVED_times,&
&                         Life_MEM_steps,SAVED_lifetime,RT_dynamics_steps,&
&                         FIRST_step,l_RT_skip_impose,E_E_Hole_lifetime,E_E_Electron_lifetime,&
&                         E_P_Electron_lifetime,E_P_Hole_lifetime,E_P_El_abs_lifetime,&
&                         E_P_Ho_abs_lifetime,E_P_El_abs_lifetime,ADD_to_saved_values
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
 integer          :: ib,ik,ik_rim,Nk_ee
 !
 Nk_ee=k%nibz
#if defined _RT_EE_RIM
 Nk_ee=RT_nk
#endif
 !
 if (EVAL) then
   !===========
   ! ALL 2 ALL
   !
   if (SAVE_index<=2) then
     do ik=1,Nk_ee
#if defined _RT_EE_RIM
       ik_rim=ik
#else
       ik_rim=RT_ibz_coo(ik,1)
#endif
       call PP_redux_wait(LIFE(:,ik_rim))
     enddo
#if !defined _RT_EE_RIM
     if (SAVE_index==2) call RT_Coulomb_interpolation(E,k)
#endif
   else
     do ik=1,RT_nk
       LIFE(:,ik)=LIFE(:,ik)/real(PAR_COM_Plasma_INDEX%n_CPU)
       call PP_redux_wait(LIFE(:,ik))
     enddo
   endif
   !
   ! Zero dN/dT & dE/dT & d(DeltaN)/dT
   !============================================
   if (.not.l_RT_skip_impose) call Impose_Conservations( )
   !
   !===========
   ! SAVE
   !===========
   !
   if (RT_dynamics_steps(1)<0.) return
   !
   if (l_elel_scatt     .and.SAVE_index==1) call ADD_to_saved_values(Time,SAVED_times,Life_MEM_steps)
   if (.not.l_elel_scatt.and.SAVE_index==3) call ADD_to_saved_values(Time,SAVED_times,Life_MEM_steps)
   !
   do ib=SC_bands(1),SC_bands(2)
     if (SAVE_index<=2) then
       ! e-e case
       do ik=1,Nk_ee
#if defined _RT_EE_RIM
         ik_rim=ik
#else
         ik_rim=RT_ibz_coo(ik,1)
#endif
         call ADD_to_saved_values(LIFE(ib,ik_rim),SAVED_lifetime(ib,ik_rim,SAVE_index,:),Life_MEM_steps)
       enddo
     else
       ! e-p case
       do ik=1,RT_nk
         call ADD_to_saved_values(LIFE(ib,ik),SAVED_lifetime(ib,ik,SAVE_index,:),Life_MEM_steps)
       enddo
     endif
   enddo
 else 
   if (RT_TIME_status(it_now)==0.or. (RT_TIME_status(it_now)>0.and..not.FIRST_step(it_now,Time)) ) then
     !=============================
     ! INTERPOLATE 
     !=============
     do ib=SC_bands(1),SC_bands(2)
       if (SAVE_index<=2) then
         ! e-e case
         do ik=1,Nk_ee
#if defined _RT_EE_RIM
           ik_rim=ik
#else
           ik_rim=RT_ibz_coo(ik,1)
#endif
           call RT_Lifetimes_Interpolation(LIFE(ib,ik_rim),Time,SAVED_lifetime(ib,ik_rim,SAVE_index,:),SAVED_times)
         enddo
#if !defined _RT_EE_RIM
         if (SAVE_index==2) call RT_Coulomb_interpolation(E,k)
#endif
       else
         ! e-p case
         do ik=1,RT_nk
           call RT_Lifetimes_Interpolation(LIFE(ib,ik),Time,SAVED_lifetime(ib,ik,SAVE_index,:),SAVED_times)
         enddo
       endif
     enddo
     !
     ! Zero dN/dT & dE/dT & d(DeltaN)/dT
     !============================================
     if (.not.l_RT_skip_impose) call Impose_Conservations( )
     !
   else if (RT_TIME_status(it_now)<0) then
     !==================================
     ! KEEP CONSTANT 
     !=============
     if (SAVE_index<=2) then
       ! e-e case
       do ik=1,Nk_ee
#if defined _RT_EE_RIM
         ik_rim=ik
#else
         ik_rim=RT_ibz_coo(ik,1)
#endif
         LIFE(:,ik_rim)=SAVED_lifetime(:,ik_rim,SAVE_index,Life_MEM_steps)
       enddo
#if !defined _RT_EE_RIM
       if (SAVE_index==2) call RT_Coulomb_interpolation(E,k)
#endif
     else
       LIFE(:,:)=SAVED_lifetime(:,:,SAVE_index,Life_MEM_steps)
     endif
   endif
 endif
 !
 contains
   !
   subroutine Impose_Conservations( )
     !
     ! Zero dN/dT & dE/dT & d(DeltaN)/dT
     !===================================
     !
     if (l_elel_scatt.and.SAVE_index==2) call RT_impose_zero_dE_and_dN(E,k,E_E_Hole_lifetime,E_E_Electron_lifetime,'elel ') 
     if (l_elph_scatt.and.SAVE_index==6) then
       call RT_impose_zero_dE_and_dN(E,k,E_P_Hole_lifetime,E_P_Electron_lifetime,'elph1')
       call RT_impose_zero_dE_and_dN(E,k,E_P_Ho_abs_lifetime,E_P_El_abs_lifetime,'elph2')
     endif
     !
   end subroutine
   !
end subroutine RT_Lifetimes_save_and_interp

