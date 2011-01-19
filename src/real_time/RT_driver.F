!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_driver(en,Xen,X,k,xk,q,Xw)
 !
 use pars,           ONLY:SP
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:FS2AUT
 use timing,         ONLY:live_timing 
 use electrons,      ONLY:levels
 use X_m,            ONLY:X_t,X_alloc
 use wave_func,      ONLY:WF_load,WF_free
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_potential,SC_bands,it_now,local_V,WF_G_max,WF_Go_indx
 use real_time,      ONLY:Thermal_steps,NE_steps,RT_step,G_lesser,l_NE_simulate_IP, &
&                         G_lesser_previous,l_NE_with_fields,RT_alloc,RT_free,      &
&                         NE_i_time,Integrator_name,l_RT_diagonalize,max_rt_steps_disk,&
&                         G_lesser_reference,RT_is_dumped,RT_diagonalization_steps, &
&                         it_start,RT_output,NE_time,NE_dynamics
#if defined _TWO_LEVELS
 use real_time,      ONLY:l_3levels
 use parser_m,       ONLY:parser
#endif
 use fields,           ONLY:A_vecpot,A_vecpot_previous
 use collisions_CACHE, ONLY:free_COLLISIONS_v

 use real_time, ONLY: I1_lesser,I2_lesser
 !
 implicit none
 !
 ! if l_RT_diagonalize=.true.  "en"  will contains new energies and occupations
 ! otherwise it will not be used
 !
 ! Notice that Xen is never changed!
 !
 type(levels)  :: en,Xen
 type(w_samp)  :: Xw(2) 
 type(bz_samp) :: k,Xk,q
 type(X_t):: X(4)
 logical, external  ::stop_now
 ! 
 ! Work Space
 !
 integer                  :: it,ib
 logical                  :: Dynamics_is_over
 complex(SP)              :: RT_Energies(7)
 !
 call section('*',trim(SC_potential)//' Real-Time Dynamics')
 !##########################################################
 !
 call set_real_printed_length(f_length=10,g_length=10)  
 !
#if defined _TWO_LEVELS
 call parser('RT3Levels',l_3levels)
#endif
 ! 
 ! Switch 
 !=======
 !
 call XC_switch()
 !
 ! Dipoles & WF dimensions
 !=========================
 !
 call SC_WF_and_dipole_dimensions(X)
 !
 ! Dipoles
 !========
 !
 ! before loading the WF in R-space I check and/or calculate
 ! the optical collisions that are done in G-space and would
 ! force the reloading of the WFs
 !
 call Dipole_driver(Xen, Xk, X(1), X(1)%q0)
 !
 ! WFs loading
 !=============
 !
 call WF_load(WF_G_max,WF_Go_indx,(/1,SC_bands/),(/1,k%nibz/),space='R',title='-RT')  
 !
 ! Core initialization
 !
 call RT_initialize()
 !
 ! Core allocations
 !
 call RT_alloc(Xen)
 !
 if(.not.local_V.and..not.l_NE_simulate_IP) then
   !
   call COLLISIONS_IO_eval(en,X,k,k,q,Xw)
   !
   call load_COLLISIONS()
   !
 endif
 !
 ! Restart from a previous RT run or from a SC calculation
 ! 
 call RT_start_and_restart(Xen,Xk,k,q)
 !
 ! Calculation already done
 !
 if(it_start>=Thermal_steps+NE_steps) then
   !      
   call RT_free()
   !
   call WF_free()
   !
   call X_alloc("P_square")
   !
   return
   !
 endif
 !
 ! IP calculation => No need of WFs
 !
 if(l_NE_simulate_IP) call WF_free()
 !
 ! Initialize output files
 !==========================
 !
 call RT_output(what="open dyn cur pol")
 if(l_NE_with_fields) call RT_output(what="open pot")
 !
 ! Loop on time
 !==============
 Dynamics_is_over=.FALSE.
 !
 it=it_start-1
 !
 do while (.not.Dynamics_is_over)
   !
   it=it+1
   !
   ! Steps & times ...
   !===================
   !
   ! 1 .... Thermal_steps (Thermal period) => NE_time=-Thermal_steps*RT_step ... -RT_step
   ! it = Thermal_steps+1                  => NE_time =0 (NE_i_time = 1)
   !
   ! at  it = Thermal_steps+1 NE_dynamics=.TRUE.
   !
   NE_i_time=it-Thermal_steps
   NE_time=(NE_i_time-1)*RT_step
   NE_dynamics=NE_i_time>=1
   !
   ! Start-Up & Timing
   !===================
   !
   if(it==it_start) then
     !
     if (NE_i_time<0 ) call live_timing('[RT] Thermalization :',Thermal_steps-it_start+1,SERIAL=.true.)
     if (NE_i_time>=0) call live_timing('[RT] Dynamics :',NE_steps-it_start+1,SERIAL=.true.)
     !
     ! Use the (eventually) thermalized G_lesser as a reference...
     !
     G_lesser_reference=G_lesser
     !
     ! ... and use it to update the dipoles
     !
     if (Thermal_steps/=0) call RT_update_dipoles(G_lesser_reference,Xen,k,q)
     !
   endif
   !
   ! Keep a copy of G_lesser and A_vecpot before starting the collisions
   ! ===========
   !
   G_lesser_previous=G_lesser
   A_vecpot_previous=A_vecpot
   !
   ! Simmetrization
   !
   forall( ib=1:SC_bands )  G_lesser_previous(ib,ib,:)=(0._SP,1._SP)*aimag(G_lesser(ib,ib,:))
   !
#if defined _TWO_LEVELS
   if(l_3levels) then
     call RT_three_levels(G_lesser,it)
   else
     call RT_two_levels(G_lesser)
   endif
#endif
   !
   call RT_Energy(Xen,k,RT_Energies)
   !
   ! Diagonalization
   !================
   if(l_RT_diagonalize) then
     call RT_qp_analyze(it,en,k,Xk,G_lesser)
     call RT_update_G_reference(it,Xen,G_lesser_reference)
   endif   
   !
   ! End dynamics ?
   !===============
   if (it==Thermal_steps+NE_steps) Dynamics_is_over=.TRUE.
   if (stop_now(.FALSE.)) then
      NE_steps=(int((it-Thermal_steps)/max_rt_steps_disk)+1)*max_rt_steps_disk
      Dynamics_is_over=.TRUE.
   endif
   !
   ! Write DBs
   !===========
   it_now=it ! used in RESTART
   !
   call set_real_printed_length()
   call RT_databases_IO(Xen,it)
   call set_real_printed_length(f_length=10,g_length=10)  
   !
   ! Current and Polarization
   !==========================
   call RT_current_AND_polarization(Xk,G_lesser,A_vecpot)
   !
   ! Now print Energetics, current and (eventually) potential 
   !=========================================================
   call RT_output('dyn',real(RT_Energies),NE_time/FS2AUT)
   call RT_output('cur pol',TIME_or_E=NE_time/FS2AUT)
   if(l_NE_with_fields) call RT_output('pot',TIME_or_E=NE_time/FS2AUT) 
   !
   ! Integrate equation of motion
   !==============================
   ! T -> T+dT step 
   !
   call RT_Collision(G_lesser_previous,A_vecpot_previous,Xen,k,q,NE_time)
   !
   call RT_Integrator(G_lesser,G_lesser_previous,A_vecpot,A_vecpot_previous,RT_step,Xen,k,q,Integrator_name,NE_time)
   !
   ! Timing
   !========
   !
   call live_timing(steps=1)
   !
   if (it==Thermal_steps) call live_timing()
   !
 enddo
 !
 if(NE_steps/=0) call live_timing
 !
 call RT_output(what="close cur pol pot dyn")
 !
 if (l_RT_diagonalize) call SC_History(-1,en)
 !
 ! Finalize
 !-----------
 !
 call RT_free()
 !
 call WF_free()
 !
 if(.not.local_V.and..not.l_NE_simulate_IP) call free_COLLISIONS_v()
 !
 call X_alloc("P_square")
 !
 call set_real_printed_length()
 !
end subroutine RT_driver
